package epsa.controllers

import epsa.I18N
import epsa.I18N.Strings
import epsa.controllers.MainController.State
import epsa.model.Savings
import epsa.util.Awaits
import grizzled.slf4j.Logging
import java.time.LocalDate
import javafx.beans.binding.Bindings
import javafx.beans.property.{SimpleObjectProperty, SimpleStringProperty}
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.Scene
import javafx.scene.control._
import javafx.scene.image.ImageView
import javafx.stage.{Stage, WindowEvent}
import suiryc.scala.concurrent.Callable
import suiryc.scala.{javafx => jfx}
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.event.EventHandler._
import suiryc.scala.javafx.scene.control.TableViews
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.stage.Stages.StageLocation
import suiryc.scala.javafx.util.Callback
import suiryc.scala.settings.Preference

class AccountHistoryController extends Logging {

  import AccountHistoryController._

  @FXML
  protected var splitPane: SplitPane = _

  @FXML
  protected var historyTable: TreeTableView[AssetEventItem] = _

  private val columnEventDate = new TreeTableColumn[AssetEventItem, String](Strings.date)

  private val columnEventDesc = new TreeTableColumn[AssetEventItem, AssetEventItem](Strings.event)

  private val historyColumns = List(
    "date" -> columnEventDate,
    "desc" -> columnEventDesc
  )

  val currency = epsa.Settings.currency()

  private var stage: Stage = _

  def initialize(stage: Stage, state: State): Unit = {
    import scala.collection.JavaConversions._
    this.stage = stage

    val events = Awaits.readDataStoreEvents(Some(stage)).getOrElse(Nil) ++ state.eventsUpd
    val assetEvents = events.filter(_.isInstanceOf[Savings.AssetEvent]).asInstanceOf[Seq[Savings.AssetEvent]]

    columnEventDate.setCellValueFactory(Callback { data =>
      new SimpleStringProperty(data.getValue.valueProperty().get().date.map(_.toString).orNull)
    })
    columnEventDesc.setCellValueFactory(Callback { data =>
      new SimpleObjectProperty(data.getValue.valueProperty().get())
    })
    columnEventDesc.setCellFactory(Callback {
      new TreeTableCell[AssetEventItem, AssetEventItem] {
        // Display graphic on the right side
        setContentDisplay(ContentDisplay.RIGHT)

        override def updateItem(item: AssetEventItem, empty: Boolean): Unit = {
          super.updateItem(item, empty)
          if (empty) setText(null)
          else setText(item.desc)
          (if (empty) None else item.comment) match {
            case Some(v) =>
              setTooltip(new Tooltip(v))
              setGraphic(new ImageView(Images.iconQuestionBalloon))

            case None =>
              setTooltip(null)
              setGraphic(null)
          }
        }
      }:TreeTableCell[AssetEventItem, AssetEventItem]
    })

    historyTable.getColumns.addAll(columnEventDate, columnEventDesc)
    val items = assetEvents.map { event =>
      val eventItems = getEventItems(state.savingsUpd, event)
      val root = new TreeItem[AssetEventItem](eventItems.head)
      eventItems.tail.foreach { eventItem =>
        root.getChildren.add(new TreeItem[AssetEventItem](eventItem))
      }
      root
    }
    val root = new TreeItem[AssetEventItem]()
    root.getChildren.addAll(items)
    historyTable.setRoot(root)
    historyTable.setShowRoot(false)
  }

  /** Restores (persisted) view. */
  private def restoreView(): Unit = {
    // Restore stage location
    Option(stageLocation()).foreach { loc =>
      Stages.setLocation(stage, loc, setSize = true)
    }

    // Restore assets columns order and width
    TableViews.setColumnsView(historyTable, historyColumns, Option(historyColumnsPref()))
    // Now what we want is for both columns to occupy the whole table width.
    // Using a constrained resizing policy gets in the way of restoring the
    // view, so a solution is to create a binding through which we set the
    // 'event description' column (preferred) width according to the width
    // of other elements:
    // column2Width = tableWidth - tablePadding - column1Width
    val padding2Width = Bindings.createDoubleBinding(Callable {
      val insets = historyTable.paddingProperty.get
      insets.getLeft + insets.getRight
    }, historyTable.paddingProperty)
    columnEventDesc.prefWidthProperty.bind(historyTable.widthProperty.subtract(padding2Width).subtract(columnEventDate.widthProperty))

    def restoreDividerPositions(): Unit = {
      // Restore SplitPane divider positions
      Option(splitPaneDividerPositions()).foreach { dividerPositions =>
        try {
          val positions = dividerPositions.split(';').map(_.toDouble)
          splitPane.setDividerPositions(positions: _*)
        } catch {
          case ex: Throwable => warn(s"Could not restore SplitPane divider positions[$dividerPositions]: ${ex.getMessage}")
        }
      }
    }

    // On Linux, we must wait a bit after changing stage size before setting
    // divider positions, otherwise the value gets altered a bit by stage
    // resizing ...
    import scala.concurrent.duration._
    if (!jfx.isLinux) restoreDividerPositions()
    else JFXSystem.scheduleOnce(200.millis)(restoreDividerPositions())
  }

  /** Persists view (stage location, ...). */
  private def persistView(): Unit = {
    // Persist stage location
    // Note: if iconified, resets it
    stageLocation() = Stages.getLocation(stage).orNull

    // Persist assets table columns order and width
    historyColumnsPref() = TableViews.getColumnsView(historyTable, historyColumns)

    // Persist SplitPane divider positions
    splitPaneDividerPositions() = splitPane.getDividerPositions.mkString(";")
  }

  def onCloseRequest(event: WindowEvent): Unit = {
    persistView()
  }

  private def getEventItems(savings: Savings, event: Savings.AssetEvent): List[AssetEventItem] = event match {
    case e: Savings.MakePayment =>
      List(
        // $1=amount $2=fund $3=scheme
        AssetEventItem(event.date, Strings.assetEventPaymentMain.format(
          Form.formatAmount(e.part.amount(e.part.value), currency),
          savings.getFund(e.part.fundId).name, savings.getScheme(e.part.schemeId).name), event.comment),
        // $1=units $2=NAV
        AssetEventItem(Strings.assetEventPaymentDetails1.format(e.part.units, e.part.value))
      )

    case e: Savings.MakeTransfer =>
      List(
        // $1=src amount $2=src fund $3=src scheme $4=dst fund $5=dst scheme
        AssetEventItem(event.date, Strings.assetEventTransferMain.format(
          Form.formatAmount(e.partSrc.amount(e.partSrc.value), currency),
          savings.getFund(e.partSrc.fundId).name, savings.getScheme(e.partSrc.schemeId).name,
          savings.getFund(e.partDst.fundId).name, savings.getScheme(e.partDst.schemeId).name), event.comment),
        // $1=src units $2=src NAV
        AssetEventItem(Strings.assetEventTransferDetails1.format(e.partSrc.units, e.partSrc.value)),
        // $1=dst units $2=dst NAV $3=dst amount
        AssetEventItem(Strings.assetEventTransferDetails2.format(e.partDst.units, e.partDst.value,
          Form.formatAmount(e.partDst.amount(e.partDst.value), currency)))
      )

    case e: Savings.MakeRefund =>
      List(
        // $1=amount $2=fund $3=scheme
        AssetEventItem(event.date, Strings.assetEventRefundMain.format(
          Form.formatAmount(e.part.amount(e.part.value), currency),
          savings.getFund(e.part.fundId).name, savings.getScheme(e.part.schemeId).name), event.comment),
        // $1=units $2=NAV
        AssetEventItem(Strings.assetEventRefundDetails1.format(e.part.units, e.part.value))
      )
  }

}

object AccountHistoryController {

  import epsa.Settings.prefs
  import Preference._

  private val prefsKeyPrefix = "stage.account-history"

  private val stageLocation = Preference.from(s"$prefsKeyPrefix.location", null:StageLocation)

  private val splitPaneDividerPositions = Preference.from(s"$prefsKeyPrefix.splitPane.dividerPositions", null:String)

  private val historyColumnsPref = Preference.from(s"$prefsKeyPrefix.history.columns", null:String)

  case class AssetEventItem(date: Option[LocalDate], desc: String, comment: Option[String])

  object AssetEventItem {

    def apply(date: LocalDate, desc: String, comment: Option[String]): AssetEventItem =
      AssetEventItem(Some(date), desc, comment)

    def apply(desc: String): AssetEventItem =
      AssetEventItem(None, desc, None)

  }

  /** Builds a dialog out of this controller. */
  def buildDialog(state: State): Stage = {
    val stage = new Stage()
    stage.getIcons.setAll(Images.iconClockHistory)
    stage.setTitle(Strings.accountHistory)

    val loader = new FXMLLoader(getClass.getResource("/fxml/account-history.fxml"), I18N.getResources)
    stage.setScene(new Scene(loader.load()))
    stage.getScene.getStylesheets.add(getClass.getResource("/css/main.css").toExternalForm)
    val controller = loader.getController[AccountHistoryController]
    controller.initialize(stage, state)

    // Delegate closing request to controller
    stage.setOnCloseRequest(controller.onCloseRequest _)

    // Wait for dialog to be shown before restoring the view
    stage.showingProperty().listen2 { cancellable =>
      cancellable.cancel()
      controller.restoreView()
    }

    Stages.trackMinimumDimensions(stage)

    stage
  }

}
