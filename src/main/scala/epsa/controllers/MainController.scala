package epsa.controllers

import akka.actor.{Actor, ActorRef, Props}
import epsa.I18N
import epsa.charts.ChartHandler
import epsa.model.Savings
import epsa.tools.EsaliaInvestmentFundProber
import java.nio.file.Path
import java.util.{ResourceBundle, UUID}
import javafx.beans.binding.Bindings
import javafx.beans.property.SimpleObjectProperty
import javafx.collections.FXCollections
import javafx.collections.transformation.SortedList
import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene}
import javafx.scene.control._
import javafx.stage._
import javafx.stage.FileChooser.ExtensionFilter
import suiryc.scala.RichOption._
import suiryc.scala.concurrent.Callable
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.event.EventHandler._
import suiryc.scala.javafx.scene.control.TableViews
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.util.Callback
import suiryc.scala.settings.Preference

// TODO - have I18N resource fall back to default language value, or key (log issue)
// TODO - menu key shortcuts ?
// TODO - change menu for OS integration ? (e.g. Ubuntu)
// TODO - save fund value history in datastore
// TODO - display base and current (to date) amounts in assets table
// TODO - display asset gain/loss (amount/percentage) in assets table
// TODO - display details next to assets table when selecting entry; display values history graph (or button for new window)
// TODO - menu entry to save pending events to datastore
// TODO - load (and replay) events upon starting application
// TODO - menu entry to select datastore location
// TODO - menu entries with latest datastore locations ?
// TODO - notify datastore saving issues
// TODO - menu entry and dialog to create a payment/transfer/refund event
// TODO - menu entry and dialog to display/edit events history ?
class MainController {

  import epsa.Main.prefs
  import MainController._
  import Preference._
  import Stages.StageLocation

  private val stageLocation = Preference.from("stage.main.location", null:StageLocation)

  private val assetsColumnsPref = Preference.from("stage.main.assets.columns", null:String)

  //@FXML
  //protected var location: URL = _

  @FXML
  protected var resources: ResourceBundle = _

  @FXML
  protected var assetsTable: TableView[Savings.Asset] = _

  private var actor: ActorRef = _

  lazy private val columnScheme =
    new TableColumn[Savings.Asset, String](resources.getString("Scheme"))

  lazy private val columnFund =
    new TableColumn[Savings.Asset, String](resources.getString("Fund"))

  lazy private val columnAmount =
    new TableColumn[Savings.Asset, BigDecimal](resources.getString("Amount"))

  lazy private val columnUnits =
    new TableColumn[Savings.Asset, BigDecimal](resources.getString("Units"))

  lazy private val assetsColumns = List(
    "scheme" -> columnScheme,
    "fund" -> columnFund,
    "amount" -> columnAmount,
    "units" -> columnUnits
  )

  //def initialize(): Unit = { }

  def initialize(state: State): Unit = {
    // Note: make the actor name unique (with timestamp) so that it can be
    // recreated later.
    actor = JFXSystem.newJFXActor(
      ControllerActor.props(state),
      s"epsa-main@${System.currentTimeMillis}"
    )

    // Note: scheme and fund columns cell value factory relies on the current
    // Savings instance. We could try to define those in the actor, but table
    // view handling makes it difficult or impossible to ensure all updated
    // values (e.g. changed scheme/fund name) are applied.
    // Instead, have the savings a Property that triggers cell content updating
    // when changed. Store it as the table user data since we don't use it for
    // anything else while it needs to be shared between the controller and its
    // actor.
    // Note: handle unknown scheme/fund in updated Savings. It if happens,
    // corresponding assets will disappear from the table right away (items
    // updated after savings).
    val savingsProperty = new SimpleObjectProperty[Savings](state.savingsUpd)
    assetsTable.setUserData(savingsProperty)
    columnScheme.setCellValueFactory(Callback { data =>
      Bindings.createStringBinding(
        Callable(savingsProperty.get().findScheme(data.getValue.schemeId).map(_.name).orNull),
        savingsProperty
      )
    })
    columnFund.setCellValueFactory(Callback { data =>
      Bindings.createStringBinding(
        Callable(savingsProperty.get().findFund(data.getValue.fundId).map(_.name).orNull),
        savingsProperty
      )
    })
    columnAmount.setCellValueFactory(Callback { data =>
      new SimpleObjectProperty(data.getValue.amount)
    })
    columnAmount.setCellFactory(Callback { new AmountCell[Savings.Asset] })
    columnUnits.setCellValueFactory(Callback { data =>
      new SimpleObjectProperty(data.getValue.units)
    })

    // Restore assets columns order and width
    TableViews.setColumnsView(assetsTable, assetsColumns, Option(assetsColumnsPref()))
    // Note: Asset gives scheme/fund UUID. Since State is immutable (and is
    // changed when applying events in controller) we must delegate scheme/fund
    // lookup to the controller.
    assetsTable.setRowFactory(Callback { newAssetRow() })
  }

  def onCloseRequest(event: WindowEvent): Unit = {
    actor ! OnExit
    // Note: consume the event, the actor is responsible for shutting down
    event.consume()
  }

  def onExit(event: ActionEvent): Unit = {
    actor ! OnExit
  }

  def onEditSchemes(event: ActionEvent): Unit = {
    actor ! OnEditSchemes
  }

  def onEditFunds(event: ActionEvent): Unit = {
    actor ! OnEditFunds
  }

  def onOptions(event: ActionEvent): Unit = {
    actor ! OnOptions
  }

  def onTest(event: ActionEvent): Unit = {
    actor ! OnTest
  }

  def onFundGraph(event: ActionEvent): Unit = {
    actor ! OnFundGraph
  }

  /**
   * Creates a new Asset table view row.
   *
   * Binds menu context to edit asset scheme/fund.
   */
  private def newAssetRow(): TableRow[Savings.Asset] = {
    val row = new TableRow[Savings.Asset]()

    // See: https://www.marshall.edu/genomicjava/2013/12/30/javafx-tableviews-with-contextmenus/
    val menu = new ContextMenu()
    val editScheme = new MenuItem(resources.getString("Edit scheme"))
    editScheme.setOnAction { (event: ActionEvent) =>
      Option(row.getItem).foreach { asset =>
        actor ! OnEditScheme(asset.schemeId)
      }
    }
    val editFund = new MenuItem(resources.getString("Edit fund"))
    editFund.setOnAction { (event: ActionEvent) =>
      Option(row.getItem).foreach { asset =>
        actor ! OnEditFund(asset.fundId)
      }
    }
    menu.getItems.addAll(editScheme, editFund)

    row.contextMenuProperty().bind {
      Bindings.when(Bindings.isNotNull(row.itemProperty))
        .`then`(menu)
        .otherwise(null:ContextMenu)
    }

    row
  }

  object ControllerActor {
    def props(state: State) = Props(new ControllerActor(state))
  }

  class ControllerActor(state0: State) extends Actor {

    // Cheap trick to fill fields with Savings data
    processEvents(state0, Nil)

    override def receive: Receive = receive(state0)

    def receive(state: State): Receive = {
      case OnExit           => onExit(state)
      case OnEditSchemes    => onEditSchemes(state, None)
      case OnEditScheme(id) => onEditSchemes(state, Some(state.savingsUpd.getScheme(id)))
      case OnEditFunds      => onEditFunds(state, None)
      case OnEditFund(id)   => onEditFunds(state, Some(state.savingsUpd.getFund(id)))
      case OnOptions        => onOptions(state)
      case OnTest           => onTest(state)
      case OnFundGraph      => onFundGraph(state)
    }

    def processEvents(state: State, events: List[Savings.Event]): Unit = {
      val newEvents = state.eventsUpd ::: events
      val newSavings = Savings.processEvents(state.savingsUpd, events)
      val newState = state.copy(eventsUpd = newEvents, savingsUpd = newSavings)

      // First update savings associated to assets table: takes care of
      // schemes/funds updated names if any.
      assetsTable.getUserData.asInstanceOf[SimpleObjectProperty[Savings]].set(newSavings)
      // Then update table content: takes care of added/removed entries
      import scala.collection.JavaConversions._
      val sortedAssets = new SortedList(FXCollections.observableList(newSavings.assets))
      sortedAssets.comparatorProperty.bind(assetsTable.comparatorProperty)
      assetsTable.setItems(sortedAssets)

      context.become(receive(newState))
    }

    def onExit(state: State): Unit = {
      val shutdown = if (state.eventsUpd.nonEmpty) {
        // TODO - also propose saving before leaving ?
        val alert = new Alert(Alert.AlertType.CONFIRMATION)
        alert.initOwner(state.window)
        alert.setHeaderText(resources.getString("confirmation.pending-changes"))
        alert.showAndWait().contains(ButtonType.OK)
      }
      else true

      if (shutdown) {
        persistView(state)

        context.stop(self)
        epsa.Main.shutdown(state.stage)
      }
    }

    def onEditSchemes(state: State, edit0: Option[Savings.Scheme]): Unit = {
      val edit = edit0.orElse(Option(assetsTable.getSelectionModel.getSelectedItem).map { asset =>
        state.savingsUpd.getScheme(asset.schemeId)
      })
      val dialog = EditSchemesController.buildDialog(state.savingsUpd, edit)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.initOwner(state.window)
      dialog.setResizable(true)
      val events = dialog.showAndWait().orElse(Nil)
      processEvents(state, events)
    }

    def onEditFunds(state: State, edit0: Option[Savings.Fund]): Unit = {
      val edit = edit0.orElse(Option(assetsTable.getSelectionModel.getSelectedItem).map { asset =>
        state.savingsUpd.getFund(asset.fundId)
      })
      val dialog = EditFundsController.buildDialog(state.savingsUpd, edit)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.initOwner(state.window)
      dialog.setResizable(true)
      val events = dialog.showAndWait().orElse(Nil)
      processEvents(state, events)
    }

    def onOptions(state: State): Unit = {
      val dialog = OptionsController.buildDialog()
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.initOwner(state.window)
      dialog.setResizable(true)
      val reload = dialog.showAndWait().orElse(false)
      if (reload) {
        // Persist now to restore it when rebuilding the stage
        persistView(state)
        context.stop(self)
        MainController.build(state, needRestart = true)
      }
    }

    def onTest(state: State): Unit = {
      import java.time.LocalDate

      val s1 = Savings().createSchemeEvent("Scheme 1")
      val s2 = Savings().createSchemeEvent("Scheme 2")
      val f1 = Savings().createFundEvent("Fund 1")
      val f2 = Savings().createFundEvent("Fund 2")
      processEvents(state, List(s1, s2, f1, f2,
        Savings.AssociateFund(s1.schemeId, f1.fundId),
        Savings.AssociateFund(s1.schemeId, f2.fundId),
        Savings.AssociateFund(s2.schemeId, f2.fundId),
        Savings.MakePayment(LocalDate.now, Savings.Asset(s1.schemeId, f1.fundId, None, 10.0, 10.0)),
        Savings.MakePayment(LocalDate.now, Savings.Asset(s1.schemeId, f1.fundId, None, 20.0, 10.0)),
        Savings.MakePayment(LocalDate.now, Savings.Asset(s1.schemeId, f1.fundId, Some(LocalDate.now.plusMonths(12)), 20.0, 10.0)),
        Savings.MakeRefund(LocalDate.now, Savings.Asset(s1.schemeId, f1.fundId, None, 10.0, 5.0)),
        Savings.MakeTransfer(LocalDate.now, Savings.Asset(s1.schemeId, f1.fundId, None, 10.0, 10.0), Savings.Asset(s1.schemeId, f2.fundId, None, 10.0, 20.0))
      ))
    }

    def onFundGraph(state: State): Unit = {
      import Preference._

      val stage = new Stage()

      val fundPathFolder = Preference.from("fund.path.folder", null:Path)
      val fundPathFile = Preference.from("fund.path.file", null:String)

      val fileChooser = new FileChooser()
      fileChooser.setTitle("Open Investment Fund File")
      fileChooser.getExtensionFilters.addAll(
        new ExtensionFilter("Excel Files", "*.xls", "*.xlsx"),
        new ExtensionFilter("All Files", "*.*")
      )
      fundPathFolder.option.foreach { path =>
        fileChooser.setInitialDirectory(path.toFile)
        fundPathFile.option.foreach(fileChooser.setInitialFileName)
      }
      val selectedFile = fileChooser.showOpenDialog(stage)
      Option(selectedFile).flatMap { file =>
        EsaliaInvestmentFundProber.probe(file.toPath)
      } match {
        case Some(fund) =>
          // Save path in preferences
          fundPathFolder() = selectedFile.getParentFile.toPath
          fundPathFile() = selectedFile.getName
          // Then build and display chart
          val chartHandler = new ChartHandler(fund)
          val chartPane = chartHandler.chartPane
          chartPane.setPrefSize(640, 480)
          val scene = new Scene(chartPane)
          stage.setScene(scene)
          stage.initModality(Modality.WINDOW_MODAL)
          stage.initOwner(state.window)
          stage.show()

        case None =>
      }
    }

    /** Persists view (stage location, ...). */
    private def persistView(state: State): Unit = {
      // Persist stage location
      // Note: if iconified, resets it
      stageLocation() = Stages.getLocation(state.stage).orNull

      // Persist assets table columns order and width
      assetsColumnsPref() = TableViews.getColumnsView(assetsTable, assetsColumns)
    }

  }

}

object MainController {

  case class State(stage: Stage, savingsInit: Savings, eventsUpd: List[Savings.Event], savingsUpd: Savings) {
    lazy val window = stage.getScene.getWindow
  }

  case object OnExit

  case object OnEditSchemes

  case object OnEditFunds

  case object OnOptions

  case class OnEditScheme(schemeId: UUID)

  case class OnEditFund(fundId: UUID)

  case object OnTest

  case object OnFundGraph

  def build(state: State, needRestart: Boolean = false): Unit = {
    val stage = state.stage

    val resources = I18N.getResources
    val loader = new FXMLLoader(getClass.getResource("/fxml/main.fxml"), resources)
    val root = loader.load[Parent]()
    val controller = loader.getController[MainController]
    controller.initialize(state)

    /*pane.getChildren.setAll(root)
    AnchorPane.setTopAnchor(root, 0)
    AnchorPane.setRightAnchor(root, 0)
    AnchorPane.setBottomAnchor(root, 0)
    AnchorPane.setLeftAnchor(root, 0)*/

    if (Option(stage.getScene).isDefined) stage.hide()
    // Delegate closing request to controller
    stage.setOnCloseRequest(controller.onCloseRequest _)
    stage.setScene(new Scene(root))
    stage.show()

    // Restore stage location
    Option(controller.stageLocation()).foreach { loc =>
      Stages.setLocation(stage, loc, setSize = true)
    }
    Stages.trackMinimumDimensions(stage)

    if (needRestart) {
      val alert = new Alert(Alert.AlertType.INFORMATION)
      alert.initOwner(state.window)
      alert.setHeaderText(resources.getString("information.need-restart"))
      alert.showAndWait()
    }
  }

}
