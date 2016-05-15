package epsa.controllers

import akka.actor.Cancellable
import com.sun.javafx.scene.control.skin.{TreeTableViewSkin, VirtualFlow, VirtualScrollBar}
import epsa.I18N
import epsa.I18N.Strings
import epsa.charts._
import epsa.controllers.MainController.State
import epsa.model.Savings
import epsa.util.{Awaits, JFXStyles}
import epsa.util.JFXStyles.AnimationHighlighter
import grizzled.slf4j.Logging
import java.time.LocalDate
import java.util.UUID
import javafx.beans.property.{SimpleObjectProperty, SimpleStringProperty}
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.Scene
import javafx.scene.control._
import javafx.scene.image.ImageView
import javafx.scene.layout.{AnchorPane, Region}
import javafx.stage.{Stage, WindowEvent}
import scala.collection.JavaConversions._
import scala.concurrent.Future
import scala.concurrent.duration._
import suiryc.scala.javafx.beans.value.RichObservableValue
import suiryc.scala.{javafx => jfx}
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.event.EventHandler._
import suiryc.scala.javafx.scene.control.{Dialogs, TableViews}
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.stage.Stages.StageLocation
import suiryc.scala.javafx.util.Callback
import suiryc.scala.math.Ordered._
import suiryc.scala.math.Ordering._
import suiryc.scala.settings.Preference

// TODO: display details of account at selected (or hovered ?) date in chart
class AccountHistoryController extends Logging {

  import AccountHistoryController._

  @FXML
  protected var splitPane: SplitPane = _

  @FXML
  protected var historyPane: AnchorPane = _

  @FXML
  protected var progressIndicator: ProgressIndicator = _

  @FXML
  protected var historyTable: TreeTableView[AssetEventItem] = _

  private val columnEventDate = new TreeTableColumn[AssetEventItem, String](Strings.date)

  private val columnEventDesc = new TreeTableColumn[AssetEventItem, AssetEventItem](Strings.event)

  private val historyColumns = List(
    "date" -> columnEventDate,
    "desc" -> columnEventDesc
  )

  private var animationHighlighter: Option[AnimationHighlighter] = None

  private val currency = epsa.Settings.currency()

  private var stage: Stage = _

  private var chartHandler: Option[ChartHandler[HistoryMark]] = None

  def initialize(stage: Stage, state: State): Unit = {
    import epsa.Main.Akka.dispatcher
    this.stage = stage

    // Sort events
    val events0 = Awaits.readDataStoreEvents(Some(stage)).getOrElse(Nil) ++ state.eventsUpd
    val events = Savings.sortEvents(events0)

    // Prepare to display progress indicator (if action takes too long)
    val showIndicator = epsa.Main.Akka.system.scheduler.scheduleOnce(500.milliseconds) {
      progressIndicator.setVisible(true)
    }

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
              setGraphic(new ImageView(Images.iconInformationBalloon))

            case None =>
              setTooltip(null)
              setGraphic(null)
          }
        }
      }:TreeTableCell[AssetEventItem, AssetEventItem]
    })

    // Keep link between top-level items (history entries) and associated row.
    historyTable.setRowFactory(Callback {
      val row = new TreeTableRow[AssetEventItem]()
      row.itemProperty.listen { (_, oldItem, newItem) =>
        // Beware to check that the old item was still using our row before
        // resetting as sometimes it was already changed: upon sorting by
        // column(s), items are usually swapped, meaning we see item A being
        // replaced by B in one row then B being replaced by A in another row.
        Option(oldItem).filter(_.row.contains(row)).foreach(_.row = None)
        Option(newItem).foreach(_.row = Some(row))
      }
      row
    })

    historyTable.getColumns.addAll(columnEventDate, columnEventDesc)

    // Replay events to get history entries (main level and details).
    val root = new TreeItem[AssetEventItem]()
    events.foldLeft(Savings()) { (savings, event) =>
      event match {
        case event: Savings.AssetEvent =>
          val eventItems = getEventItems(savings, event)
          val treeItem = new TreeItem[AssetEventItem](eventItems.head)
          eventItems.tail.foreach { eventItem =>
            treeItem.getChildren.add(new TreeItem[AssetEventItem](eventItem))
          }
          root.getChildren.add(treeItem)
          savings.processEvent(event)

        case _ =>
          savings.processEvent(event)
      }
    }
    // Note: changing root clears the table sort order. Fortunately we do
    // set the order upon restoring view which is done later.
    historyTable.setRoot(root)
    historyTable.setShowRoot(false)

    // React when entry is selected in history
    historyTable.getSelectionModel.selectedItemProperty.listen { item =>
      onHistoryEntry(Option(item))
    }

    // Now build (async) the account history chart.
    Future {
      buildHistory(state, events, showIndicator)
    }
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
    //  column2Width = tableWidth - tablePadding - column1Width
    // However the vertical scrollbar which may appear is not taken into
    // account in table width. It is in the "clipped-container" that is a
    // Region of the viewed content:
    //  column2Width = containerWidth - column1Width
    //
    // The table width is changed before the container one, which triggers
    // glitches when resizing down using the second formula: the horizontal
    // scrollbar appears (and disappears upon interaction or resizing up).
    // Requesting layout (in 'runLater') makes it disappear right away.
    // But listening to table width too (which is changed first) and keeping
    // the minimum width between 'tableWidth - tablePadding - scrollBarWidth'
    // and 'containerWidth' prevents the horizontal scrollbar from appearing.
    val clippedContainer = historyTable.lookup(".clipped-container").asInstanceOf[Region]
    val scrollBar = historyTable.lookupAll(".scroll-bar").collect {
      case scrollBar: VirtualScrollBar if scrollBar.getPseudoClassStates.map(_.getPseudoClassName).contains("vertical") => scrollBar
    }.head

    def updateColumnWidth(): Unit = {
      val insets = historyTable.getPadding
      val padding = insets.getLeft + insets.getRight
      val scrollbarWidth =
        if (!scrollBar.isVisible) 0
        else scrollBar.getWidth
      val width = math.min(historyTable.getWidth - padding - scrollbarWidth, clippedContainer.getWidth) - columnEventDate.getWidth
      columnEventDesc.setPrefWidth(width)
    }

    RichObservableValue.listen[AnyRef](
      List(historyTable.widthProperty, clippedContainer.widthProperty, columnEventDate.widthProperty),
      updateColumnWidth()
    )
    // Requesting layout (in runLater) usually helps when the table is first
    // being shown. Otherwise the column is often properly resized but it is
    // not applied (visually) until we interact with the stage ...
    JFXSystem.runLater {
      historyTable.requestLayout()
    }

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

  private def onMarkEvent(mark: HistoryMark, event: ChartMarkEvent.Value): Unit = {
    // Do some animation (highlighting) when getting on the marker.
    if (event == ChartMarkEvent.Entered) {
      val items = historyTable.getRoot.getChildren.filter { item =>
        mark.items.contains(item.getValue)
      }.toList
      val rows = items.flatMap(_.getValue.row)

      // Make sure the table entry is visible.
      // Get the VirtualFlow in the table skin (should be the first child,
      // accessible once table is shown), and ask to show the minimum entry
      // index associated to the marker.
      historyTable.getSkin.asInstanceOf[TreeTableViewSkin[_]].getChildren.find(_.isInstanceOf[VirtualFlow[_]]).foreach {
        case flow: VirtualFlow[_] =>
          val indices = items.map(historyTable.getRow).filter(_ >= 0)
          if (indices.nonEmpty) flow.show(indices.min)
      }

      animationHighlighter = Some(JFXStyles.highlightAnimation(rows, animationHighlighter))
    }
  }

  private def buildHistory(state: State, events: Seq[Savings.Event], showIndicator: Cancellable): Unit = {
    // Cached data store NAVs, and index in sequence (for more efficient search)
    var assetsNAVs = Map[UUID, Seq[Savings.AssetValue]]()
    var assetsNAVIdxs = Map[UUID, Int]()
    // Known NAV dates from data store
    val assetsNAVDates = assetsNAVs.values.flatten.map(_.date).toList.toSet
    // Known NAVs through account history events.
    // This can complete data store NAVs, especially for old (now deleted) funds.
    val assetEvents = events.filter(_.isInstanceOf[Savings.AssetEvent]).asInstanceOf[Seq[Savings.AssetEvent]]
    val eventsNAVs = assetEvents.flatMap {
      case e: Savings.MakePayment =>
        List(e.part.fundId -> Savings.AssetValue(e.date, e.part.value))

      case e: Savings.MakeTransfer =>
        List(e.partSrc.fundId -> Savings.AssetValue(e.date, e.partSrc.value),
          e.partDst.fundId -> Savings.AssetValue(e.date, e.partDst.value))

      case e: Savings.MakeRefund =>
        List(e.part.fundId -> Savings.AssetValue(e.date, e.part.value))
    }.groupBy(_._1).mapValues { v =>
      v.map(_._2).sortBy(_.date)
    }
    // Known NAV dates from account history events
    val eventsNAVDates = eventsNAVs.values.flatten.map(_.date).toList.toSet
    // All known NAV dates (data store + history), for which we may have a chart series data
    val dates = eventsNAVDates ++ assetsNAVDates
    // The oldest date we can display in account history
    val firstDate = dates.toList.sorted.headOption

    // Reads NAV history from data store, and cache it
    def getAssetHistory(fundId: UUID): Seq[Savings.AssetValue] =
      assetsNAVs.get(fundId) match {
        case Some(navs) =>
          navs

        case None =>
          val navs = Awaits.readDataStoreNAVs(Some(stage), fundId).getOrElse(Seq.empty)
          assetsNAVs += (fundId -> navs)
          navs
      }

    // Whether a date is 'valid', that is can have its own series data in the
    // history chart. Only dates for which we have a NAV (exact date matching)
    // are eligible: there is no meaning to display data for other dates since
    // they would have the same value than the nearest predating eligible one.
    def validDate(date: LocalDate, navs: Map[UUID, Savings.AssetValue]): Boolean =
      (eventsNAVDates ++ navs.values.map(_.date)).contains(date)

    // Gets a fund NAV for a given date in data store.
    def assetNAV(fundId: UUID, date: LocalDate): Option[Savings.AssetValue] = {
      // Notes:
      // We need to find the NAV by date. We know data store gives fund NAVs in
      // order.
      // The easiest way is to query the data store. But it takes too long when
      // building history (thousands of queries). So we cache the whole NAV
      // histories  and search in it.
      // The easy way is to take fund NAVs predating the target date, and keep
      // the last one ('takeWhile' followed by 'takeRight(1)', which is faster
      // than 'reverse').
      // The fastest way is to loop over NAVs until we find a matching NAV, and
      // remember the found index to start over from there for the next search
      // (which requires events to be ordered).
      // The latter is >2x faster (e.g. ~40ms vs ~100ms on first run) to >10x
      // faster (e.g. ~3ms vs ~40ms on best run) than the former when searching
      // NAVs for 4000 successive days (~12 years of history) when fund has
      // 2200 NAVs over that period.
      @scala.annotation.tailrec
      def loop(navs: Seq[Savings.AssetValue], idx: Int, acc: Option[Savings.AssetValue]): (Int, Option[Savings.AssetValue]) =
        navs.headOption match {
          case Some(nav) =>
            if (nav.date > date) (math.max(0, idx - 1), acc)
            else loop(navs.tail, idx + 1, Some(nav))

          case None =>
            (math.max(0, idx - 1), acc)
        }

      val navs = getAssetHistory(fundId)
      val idx = assetsNAVIdxs.getOrElse(fundId, 0)
      val (idx2, v) = loop(navs.drop(idx), 0, None)
      assetsNAVIdxs += fundId -> (idx + idx2)
      v
    }

    // Gets all assets NAVs for a given date.
    def assetsNAV(savings: Savings, date: LocalDate): Map[UUID, Savings.AssetValue] =
      savings.assets.list.map(_.fundId).distinct.flatMap { fundId =>
        assetNAV(fundId, date).map(fundId -> _)
      }.toMap

    // Gets a fund NAV at given date, from history events and data store NAV history.
    def getNAV(fundId: UUID, date: LocalDate, navs: Map[UUID, Savings.AssetValue]): Option[Savings.AssetValue] =
      (eventsNAVs.getOrElse(fundId, Nil) ++ navs.get(fundId)).filter { nav =>
        nav.date <= date
      }.sortBy(date.toEpochDay - _.date.toEpochDay).headOption

    // Updates history for given date.
    // If date is valid, gets NAVs and return history updated with computed
    // history data. Otherwise returns input history.
    def atDate(history: History, savings: Savings, date: LocalDate): History = {
      val navs = assetsNAV(savings, date)
      if (validDate(date, navs)) {
        val (history2, historyData) = savings.assets.list.foldLeft(history, HistoryData(date)) { case ((acc1, acc2), asset) =>
          getNAV(asset.fundId, date, navs) match {
            case Some(nav) =>
              (acc1,
                acc2.addInvestedAmount(asset.investedAmount).addGrossAmount(asset.amount(nav.value)))

            case None =>
              // $1=fund $2=date
              (acc1.addIssue(Strings.accountHistoryIssuesNAV.format(savings.getFund(asset.fundId), date)),
                acc2.addInvestedAmount(asset.investedAmount))
          }
        }
        history2.addData(historyData)
      } else history
    }

    // Updates history for given dates range.
    def atDates(history: History, savings: Savings, since: Option[LocalDate], to: LocalDate): History = {
      val from = since.orElse(firstDate).getOrElse(to)
      Stream.from(0).map { i =>
        from.plusDays(i)
      }.takeWhile { date =>
        date <= to
      }.toList.foldLeft(history) { (acc, date) =>
        atDate(acc, savings, date)
      }
    }

    // Computes history data from history events.
    @scala.annotation.tailrec
    def loop(savings: Savings, events: Seq[Savings.Event], since: Option[LocalDate], history: History): History =
      events.headOption match {
        case Some(event: Savings.AssetEvent) =>
          // Update history since last asset event
          val history2 = atDates(history, savings, since, event.date.minusDays(1))
          // Filter event date from history: there may more than one event on
          // the same date, only the latest computed one matters
          val history3 = history2.copy(data = history2.data.filterNot(_.date == event.date))
          // Apply current asset event in history
          val savings2 = savings.processEvent(event)
          val history4 = atDate(history3, savings2, event.date)
          loop(savings2, events.tail, Some(event.date.plusDays(1)), history4)

        case Some(event) =>
          loop(savings.processEvent(event), events.tail, since, history)

        case None =>
          atDates(history, savings, since, LocalDate.now)
      }

    val history = loop(Savings(), events, None, History())
    // Display warning if necessary
    if (history.issues.nonEmpty) {
      Dialogs.warning(
        owner = Some(stage),
        title = Some(title),
        headerText = Some(Strings.accountHistoryIssues),
        contentText = Some(history.issues.mkString("\n"))
      )
    }

    // TODO: handle more than one series in chart (invested + gross) ?
    val grossHistory = history.data.map { data =>
      ChartSeriesData(data.date, data.grossAmount)
    }
    val marks = historyTable.getRoot.getChildren.toList.map(_.getValue).groupBy(_.date.get).map { case (date, items) =>
      date -> HistoryMark(date, items)
    }
    val meta = ChartMeta(marks, onMarkEvent _)
    val chartHandler = new ChartHandler(
      seriesName = title,
      seriesValues = grossHistory,
      meta = meta,
      settings = ChartSettings.hidden.copy(
        xLabel = Strings.date,
        // TODO: i18n "gross amount" ?
        yLabel = Strings.gross,
        ySuffix = epsa.Settings.defaultCurrency
      )
    )
    val pane = chartHandler.chartPane
    AnchorPane.setTopAnchor(pane, 0.0)
    AnchorPane.setRightAnchor(pane, 0.0)
    AnchorPane.setBottomAnchor(pane, 0.0)
    AnchorPane.setLeftAnchor(pane, 0.0)

    // It is better to add chart in scene through JavaFX thread
    JFXSystem.runLater{
      // Hide indicator and display chart
      showIndicator.cancel()
      progressIndicator.setVisible(false)
      historyPane.getChildren.add(pane)
      this.chartHandler = Some(chartHandler)
    }
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

  private def onHistoryEntry(itemOpt: Option[TreeItem[AssetEventItem]]): Unit = {
    for {
      item <- itemOpt
      date <- item.getValue.date
      chartHandler <- this.chartHandler
    } {
      chartHandler.highlightMark(date)
    }
  }

}

object AccountHistoryController {

  import epsa.Settings.prefs
  import Preference._

  private val prefsKeyPrefix = "stage.account-history"

  private val stageLocation = Preference.from(s"$prefsKeyPrefix.location", null:StageLocation)

  private val splitPaneDividerPositions = Preference.from(s"$prefsKeyPrefix.splitPane.dividerPositions", null:String)

  private val historyColumnsPref = Preference.from(s"$prefsKeyPrefix.history.columns", null:String)

  def title = Strings.accountHistory

  case class AssetEventItem(date: Option[LocalDate], desc: String, comment: Option[String]) {
    var row: Option[TreeTableRow[AssetEventItem]] = None
  }

  object AssetEventItem {

    def apply(date: LocalDate, desc: String, comment: Option[String]): AssetEventItem =
      AssetEventItem(Some(date), desc, comment)

    def apply(desc: String): AssetEventItem =
      AssetEventItem(None, desc, None)

  }

  case class History(data: List[HistoryData] = Nil, issues: List[String] = Nil) {
    def addData(more: HistoryData) = copy(data :+ more)
    def addIssue(issue: String) = copy(issues = issues :+ issue)
  }

  case class HistoryData(date: LocalDate, investedAmount: BigDecimal = BigDecimal(0), grossAmount: BigDecimal = BigDecimal(0)) {
    def addInvestedAmount(v: BigDecimal) = copy(investedAmount = investedAmount + v)
    def addGrossAmount(v: BigDecimal) = copy(grossAmount = grossAmount + v)
  }

  case class HistoryMark(date: LocalDate, items: List[AssetEventItem]) extends ChartMark {
    override val comment = Some(items.map(_.desc).mkString("\n"))
  }

  /** Builds a dialog out of this controller. */
  def buildDialog(state: State): Stage = {
    val stage = new Stage()
    stage.getIcons.setAll(Images.iconClockHistory)
    stage.setTitle(title)

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
