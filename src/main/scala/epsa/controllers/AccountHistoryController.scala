package epsa.controllers

import com.typesafe.scalalogging.StrictLogging
import epsa.{I18N, Main, Settings}
import epsa.I18N.Strings
import epsa.Settings._
import epsa.charts._
import epsa.controllers.MainController.State
import epsa.model.Savings
import epsa.util.{Awaits, JFXStyles}
import epsa.util.JFXStyles.AnimationHighlighter
import javafx.beans.property.SimpleObjectProperty
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene}
import javafx.scene.control._
import javafx.scene.image.ImageView
import javafx.scene.input.MouseEvent
import javafx.scene.layout.AnchorPane
import javafx.stage.Stage
import monix.execution.Cancelable
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.scene.control.skin.SplitPaneSkinEx
import suiryc.scala.javafx.scene.control.{Dialogs, Panes, TableViews}
import suiryc.scala.javafx.stage.{StageLocationPersistentView, Stages}
import suiryc.scala.javafx.stage.Stages.StageLocation
import suiryc.scala.math.Ordered._
import suiryc.scala.math.Ordering._
import suiryc.scala.settings.ConfigEntry

import java.time.LocalDate
import java.util.UUID
import scala.annotation.nowarn
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

class AccountHistoryController extends StageLocationPersistentView(AccountHistoryController.stageLocation) with StrictLogging {

  import Main.settings.scalePercents
  import AccountHistoryController._

  @FXML
  protected var splitPane: SplitPane = _

  @FXML
  protected var splitPane_2: SplitPane = _

  @FXML
  protected var historyPane: AnchorPane = _

  @FXML
  protected var progressIndicator: ProgressIndicator = _

  @FXML
  protected var dateLabel: Label = _

  @FXML
  protected var investedAmountLabel: Label = _

  @FXML
  protected var grossAmountLabel: Label = _

  @FXML
  protected var grossGainLabel: Label = _

  @FXML
  protected var grossGainPctLabel: Label = _

  @FXML
  protected var historyTable: TreeTableView[AssetEventItem] = _

  private var mainController: MainController = _

  private val historyChartContextMenu = new ContextMenu()

  private val columnEventDate = new TreeTableColumn[AssetEventItem, AssetEventItem](Strings.date)

  private val columnEventDesc = new TreeTableColumn[AssetEventItem, AssetEventItem](Strings.event)

  private val historyColumns = List(
    "date" -> columnEventDate,
    "desc" -> columnEventDesc
  )

  private var animationHighlighter: Option[AnimationHighlighter] = None

  private val currency = Main.settings.currency.get

  protected var stage: Stage = _

  private var chartMarks: Map[LocalDate, HistoryMark] = Map.empty

  private var chartHandler: Option[ChartHandler[HistoryMark]] = None

  // Full history events
  private var events: List[Savings.Event] = Nil

  // NAVs deduced from history events
  private var eventsNAVs: Map[UUID, Seq[Savings.AssetValue]] = Map.empty

  private var cachedSavings: Savings = _

  private var cachedSavingsBaseDate: LocalDate = _

  def initialize(mainController: MainController, stage: Stage, state: State): Unit = {
    import epsa.Main.Akka.dispatcher
    this.mainController = mainController
    this.stage = stage

    // Get&sort events
    events = Awaits.getEventsHistory(stage)

    // Known NAVs through account history events.
    // This can complete data store NAVs, especially for old (now deleted) funds.
    eventsNAVs = Savings.getEventsNAVs(events)

    // Prepare to display progress indicator (if action takes too long)
    val showIndicator = JFXSystem.scheduler.scheduleOnce(500.milliseconds) {
      progressIndicator.setVisible(true)
    }

    // Display account details as of today
    showAccountDetails(LocalDate.now)

    columnEventDate.setCellValueFactory(data => {
      new SimpleObjectProperty(data.getValue.valueProperty().get())
    })
    columnEventDate.setCellFactory(_ => {
      new TreeTableCell[AssetEventItem, AssetEventItem] {
        override def updateItem(item: AssetEventItem, empty: Boolean): Unit = {
          super.updateItem(item, empty)
          if (empty) setText(null)
          else setText(item.date.map(_.toString).orNull)
        }
      }
    })
    // Note: AssetEventItem defines its own ordering.

    // There is no meaning to sort on event description, so disable it.
    columnEventDesc.setSortable(false)
    columnEventDesc.setCellValueFactory(data => {
      new SimpleObjectProperty(data.getValue.valueProperty().get())
    })
    columnEventDesc.setCellFactory(_ => {
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
      }
    })

    // Keep link between top-level items (history entries) and associated row.
    historyTable.setRowFactory(_ => newEventRow())

    historyTable.getColumns.addAll(columnEventDate, columnEventDesc)

    // Replay events to get history entries (main level and details).
    val root = new TreeItem[AssetEventItem]()
    events.zipWithIndex.foldLeft(Savings(levies = state.savings.levies)) { case (savings, (event, index)) =>
      val eventItems = getEventItems(savings, event, index)
      if (eventItems.nonEmpty) {
        val treeItem = new TreeItem[AssetEventItem](eventItems.head)
        eventItems.tail.foreach { eventItem =>
          treeItem.getChildren.add(new TreeItem[AssetEventItem](eventItem))
        }
        root.getChildren.add(treeItem)
      }
      savings.processEvent(event)
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
    // And display any unexpected issue.
    Future {
      buildHistory(events, showIndicator)
    }.failed.foreach {
      case ex: Exception =>
      Dialogs.error(
        owner = Some(stage),
        title = Some(title),
        contentText = Some(Strings.unexpectedIssue),
        ex = Some(ex)
      )
    }: @nowarn
  }

  /** Restores (persisted) view. */
  override protected def restoreView(): Unit = {
    super.restoreView()

    // Restore SplitPane divider positions
    splitPaneDividerPositions.opt.foreach { dividerPositions =>
      Panes.restoreDividerPositions(splitPane, dividerPositions)
    }
    splitPane2DividerPositions.opt.foreach { dividerPositions =>
      Panes.restoreDividerPositions(splitPane_2, dividerPositions)
    }

    // Restore assets columns order and width
    TableViews.setColumnsView(historyTable, historyColumns, historyColumnsPref.opt)

    // Automatically resize 'event' column to fit the whole table width.
    TableViews.autowidthColumn(columnEventDesc)
  }

  /** Persists view (stage location, ...). */
  override protected def persistView(): Unit = {
    super.persistView()

    // Persist assets table columns order and width
    historyColumnsPref.set(TableViews.getColumnsView(historyTable, historyColumns))

    // Persist SplitPane divider positions
    splitPaneDividerPositions.set(Panes.encodeDividerPositions(splitPane))
    splitPane2DividerPositions.set(Panes.encodeDividerPositions(splitPane_2))
  }

  /**
   * Creates a new history table row.
   *
   * Binds context menu when applicable.
   */
  private def newEventRow(): TreeTableRow[AssetEventItem] = {
    val row = new TreeTableRow[AssetEventItem]()

    row.itemProperty.listen { (_, oldItem, newItem) =>
      // Beware to check that the old item was still using our row before
      // resetting as sometimes it was already changed: upon sorting by
      // column(s), items are usually swapped, meaning we see item A being
      // replaced by B in one row then B being replaced by A in another row.
      Option(oldItem).filter(_.row.contains(row)).foreach(_.row = None)
      Option(newItem).foreach { item =>
        item.row = Some(row)
        // Set context menu to show savings on event date
        item.date match {
          case Some(date) =>
            val contextMenu = new ContextMenu()
            val savingsOnDate = new MenuItem(Strings.savingsOnDate,
              new ImageView(Images.iconCalendarDay))
            savingsOnDate.setOnAction { _ =>
              // Request main window to show savings on selected date
              mainController.onSavingsOnDate(date)
            }
            contextMenu.getItems.addAll(savingsOnDate)
            row.setContextMenu(contextMenu)

          case None =>
            row.setContextMenu(null)
        }
      }
    }

    row
  }

  private def onMarkEvent(event: ChartMarkEvent.Value, mark: HistoryMark): Unit = {
    // Do some animation (highlighting) when getting on the marker.
    if (event == ChartMarkEvent.Entered) {
      val items = historyTable.getRoot.getChildren.asScala.filter { item =>
        mark.items.contains(item.getValue)
      }.toList
      val rows = items.flatMap(_.getValue.row)

      // Make sure the table entry is visible.
      items.map(historyTable.getRow).filter(_ >= 0).sorted.headOption.foreach { index =>
        TableViews.scrollTo(historyTable, index, top = true, padding = 1)
      }

      animationHighlighter = Some(JFXStyles.highlightAnimation(rows, animationHighlighter))
    }
  }

  private def onMouseEvent(event: ChartEvent.Value, mouseEvent: MouseEvent, data: ChartSeriesData): Unit = {
    event match {
      case ChartEvent.Moved =>
        showAccountDetails(data.date)

      case ChartEvent.RightClicked =>
        // Refresh context menu with new item (for currently selected date)
        val header = new CustomMenuItem(new Label(data.date.toString), false)
        header.getStyleClass.addAll(JFXStyles.CLASS_HEADER, JFXStyles.CLASS_NO_SELECT)
        val savingsOnDate = new MenuItem(Strings.savingsOnDate,
          new ImageView(Images.iconCalendarDay))
        savingsOnDate.setOnAction { _ =>
          // Request main window to show savings on selected date
          mainController.onSavingsOnDate(data.date)
        }
        historyChartContextMenu.getItems.setAll(header, new SeparatorMenuItem, savingsOnDate)
        // Display context menu at current mouse position. Enable auto-hide and
        // call the 'show(...)' variant without anchor so that clicking anywhere
        // other than the popup node will make it disappear.
        historyChartContextMenu.setAutoHide(true)
        historyChartContextMenu.show(stage, mouseEvent.getScreenX, mouseEvent.getScreenY)

      case _ =>
        // We don't care
    }
  }

  private def buildHistory(events: List[Savings.Event], showIndicator: Cancelable): Unit = {
    // Cached data store NAVs, and index in sequence (for more efficient search)
    var assetsNAVs = Map[UUID, Seq[Savings.AssetValue]]()
    var assetsNAVIdxs = Map[UUID, Int]()
    // Known NAV dates from account history events
    val eventsNAVDates = eventsNAVs.values.flatten.map(_.date).toList.toSet
    // The oldest date we can display in account history
    val firstDate = eventsNAVDates.toList.sorted.headOption

    // Update progress as we go through first date to today.
    val progressEnd = LocalDate.now.toEpochDay
    val progressStart = firstDate.map(_.toEpochDay).getOrElse(progressEnd)
    val progressRange = progressEnd - progressStart

    def updateProgress(date: LocalDate): Unit = {
      val progress =
        if (progressRange <= 0) -1.0
        else (date.toEpochDay - progressStart).toDouble / progressRange
      JFXSystem.runLater {
        progressIndicator.setProgress(progress)
      }
    }

    // Reads NAV history from data store, and cache it
    def getAssetHistory(fundId: UUID): Seq[Savings.AssetValue] =
      assetsNAVs.get(fundId) match {
        case Some(navs) =>
          navs

        case None =>
          val navs = Awaits.readDataStoreNAVs(stage, fundId).getOrElse(Seq.empty)
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
      val r = if (validDate(date, navs)) {
        val (history2, historyData) = savings.assets.list.foldLeft((history, HistoryData(date))) { case ((acc1, acc2), asset) =>
          getNAV(asset.fundId, date, navs) match {
            case Some(nav) =>
              (acc1,
                acc2.addInvestedAmount(asset.investedAmount).addGrossAmount(asset.amount(nav.value)))

            case None =>
              // $1=fund $2=date
              (acc1.addIssue(Strings.accountHistoryIssuesNAV.format(savings.getFund(asset.fundId).name, date)),
                acc2.addInvestedAmount(asset.investedAmount))
          }
        }
        history2.addData(historyData)
      } else history

      updateProgress(date)

      r
    }

    // Updates history for given dates range.
    def atDates(history: History, savings: Savings, since: Option[LocalDate], to: LocalDate): History = {
      val from = since.orElse(firstDate).getOrElse(to)
      LazyList.from(0).map { i =>
        from.plusDays(i.toLong)
      }.takeWhile { date =>
        date <= to
      }.toList.foldLeft(history) { (acc, date) =>
        atDate(acc, savings, date)
      }
    }

    // Computes history data from history events.
    @scala.annotation.tailrec
    def loop(savings: Savings, events: List[Savings.Event], since: Option[LocalDate], history: History): History =
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

    // Note: we don't set account levies here as we don't need them.
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

    // TODO: handle more than one series in chart (invested + gross amounts) ?
    val grossHistory = history.data.map { data =>
      ChartSeriesData(data.date, data.grossAmount)
    }
    // Converts useful history events (root items with date) into marks.
    chartMarks = historyTable.getRoot.getChildren.asScala.toList.map(_.getValue).filter(_.date.isDefined).groupBy(_.date.get).map {
      case (date, items) => date -> HistoryMark(date, items)
    }
    val meta = ChartMeta(
      marks = chartMarks.filter(_._2.items.exists(!_.event.isInstanceOf[Savings.MakeTransfer])),
      marksHandler = onMarkEvent,
      mouseHandler = onMouseEvent
    )
    val chartHandler = new ChartHandler(
      seriesName = title,
      seriesValues = grossHistory,
      meta = meta,
      settings = ChartSettings.hidden.copy(
        xLabel = Strings.date,
        yLabel = Strings.grossAmount,
        ySuffix = Main.settings.currency.get,
        yPrecision = Main.settings.amountScale.get
      )
    )

    // It is better/required to add chart in scene through JavaFX thread
    JFXSystem.runLater {
      // Hide indicator and display chart
      showIndicator.cancel()
      progressIndicator.setVisible(false)
      val pane = chartHandler.chartPane
      historyPane.getChildren.setAll(pane)
      AnchorPane.setTopAnchor(pane, 0.0)
      AnchorPane.setRightAnchor(pane, 0.0)
      AnchorPane.setBottomAnchor(pane, 0.0)
      AnchorPane.setLeftAnchor(pane, 0.0)
      this.chartHandler = Some(chartHandler)
      grossHistory.lastOption.foreach { last =>
        chartHandler.centerOnDate(last.date, track = true)
      }
    }
  }

  private def getEventItems(savings: Savings, event: Savings.Event, index: Int): List[AssetEventItem] = event match {
    case e: Savings.MakePayment =>
      List(
        // $1=amount $2=fund $3=scheme
        AssetEventItem(event, index, e.date, Strings.assetEventPaymentMain.format(
          formatNumber(e.part.amount(e.part.value), currency),
          savings.getFund(e.part.fundId).name, savings.getScheme(e.part.schemeId).name), e.comment),
        // $1=units $2=NAV $3=availability
        AssetEventItem(event, index, Strings.assetEventPaymentDetails1.format(
          formatNumber(e.part.units), formatNumber(e.part.value, currency),
          Form.formatAvailability(e.part.availability, Some(e.date))))
      )

    case e: Savings.MakeTransfer =>
      List(
        // $1=src amount $2=src fund $3=src scheme $4=dst fund $5=dst scheme
        AssetEventItem(event, index, e.date, Strings.assetEventTransferMain.format(
          formatNumber(e.partSrc.amount(e.partSrc.value), currency),
          savings.getFund(e.partSrc.fundId).name, savings.getScheme(e.partSrc.schemeId).name,
          savings.getFund(e.partDst.fundId).name, savings.getScheme(e.partDst.schemeId).name), e.comment),
        // $1=src units $2=src NAV $3=src availability
        AssetEventItem(event, index, Strings.assetEventTransferDetails1.format(
          formatNumber(e.partSrc.units), formatNumber(e.partSrc.value, currency),
          Form.formatAvailability(e.partSrc.availability, Some(e.date))))
      ) :+ {
        e.dstDate match {
          case Some(date) =>
            // $1=dst units $2=dst NAV $3=dst amount $4=dst availability $5=date
            AssetEventItem(event, index, Strings.assetEventTransferDetails2Date.format(
              formatNumber(e.partDst.units), formatNumber(e.partDst.value, currency),
              formatNumber(e.partDst.amount(e.partDst.value), currency),
              Form.formatAvailability(e.partDst.availability, Some(e.date)),
              date))

          case None =>
            // $1=dst units $2=dst NAV $3=dst amount $4=dst availability
            AssetEventItem(event, index, Strings.assetEventTransferDetails2.format(
              formatNumber(e.partDst.units), formatNumber(e.partDst.value, currency),
              formatNumber(e.partDst.amount(e.partDst.value), currency),
              Form.formatAvailability(e.partDst.availability, Some(e.date))))
        }
      }

    case e: Savings.MakeRefund =>
      val part = e.part
      val totalUnits = savings.assets.units(part.id)
      val leviesPeriodsData = savings.computeLevies(part.id, e.date, part.value)
      val (refundLevies, _) = leviesPeriodsData.proportioned(part.units / totalUnits)
      val currency = Main.settings.currency.get
      val investedAmount = part.amount(savings.assets.vwaps(part.id))
      val grossAmount = part.amount(part.value)
      val grossGain = grossAmount - investedAmount
      val leviesAmount = refundLevies.amount
      val leviesPct = scalePercents(leviesAmount * 100 / grossGain)
      if (savings.hasLevies && Settings.debug(Debug.LeviesComputation))
        logger.info(s"action=<refund> date=<${e.date}> id=<${part.id}> nav=<${part.value}> totalUnits=<$totalUnits> units=<${part.units}> investedAmount=<$investedAmount> grossAmount=<$grossAmount> grossGain=<$grossGain> refundLevies=<$refundLevies> leviesAmount=<${refundLevies.amount}> leviesPct=<$leviesPct>")
      List(
        // $1=amount $2=fund $3=scheme
        AssetEventItem(event, index, e.date, Strings.assetEventRefundMain.format(
          formatNumber(part.amount(part.value), currency),
          savings.getFund(part.fundId).name, savings.getScheme(part.schemeId).name), e.comment),
        // $1=units $2=NAV
        AssetEventItem(event, index, Strings.assetEventRefundDetails1.format(
          formatNumber(part.units), formatNumber(part.value, currency))),
        // $1 = gross gain $2 = levies amount $3 = levies global rate
        AssetEventItem(event, index, Strings.leviesEstimation.format(
          formatNumber(grossGain, currency),
          formatNumber(leviesAmount, currency),
          formatNumber(leviesPct, "%")
        ))
      )

    case e: Savings.UpdateScheme =>
      val oldName = savings.getScheme(e.schemeId).name
      val newName = e.name
      if (newName != oldName) {
        // $1=old name $2=new name
        List(AssetEventItem(event, index, Strings.assetEventSchemeRenamed.format(oldName, newName)))
      } else Nil

    case e: Savings.UpdateFund =>
      val oldName = savings.getFund(e.fundId).name
      val newName = e.name
      if (newName != oldName) {
        // $1=old name $2=new name
        List(AssetEventItem(event, index, Strings.assetEventFundRenamed.format(oldName, newName)))
      } else Nil

    case _ =>
      Nil
  }

  private def onHistoryEntry(itemOpt: Option[TreeItem[AssetEventItem]]): Unit = {
    for {
      item <- itemOpt
      date <- item.getValue.date
      chartHandler <- chartHandler
      mark <- chartMarks.get(date)
    } {
      chartHandler.highlightMark(mark)
      showAccountDetails(date)
    }
  }

  private def showAccountDetails(date: LocalDate): Unit = {
    @scala.annotation.tailrec
    def findBaseDate(base: LocalDate, events: Seq[Savings.Event]): LocalDate =
      events.headOption match {
        case Some(head: Savings.AssetEvent) =>
          if (head.date > date) base
          else findBaseDate(head.date, events.tail)

        case Some(_) =>
          findBaseDate(base, events.tail)

        case None =>
          base
      }

    val baseDate = findBaseDate(LocalDate.now, events)
    if (baseDate != cachedSavingsBaseDate) {
      val history = events.takeWhile {
        case e: Savings.AssetEvent => e.date <= date
        case _                     => true
      }

      // Note: we don't set account levies here. User can get them by displaying
      // account savings on the requested date.
      cachedSavings = Savings().processEvents(history)
      cachedSavingsBaseDate = baseDate
    }
    val savings = cachedSavings

    case class AccountDetails(investedAmount: BigDecimal, grossAmount: BigDecimal) {
      def grossGain: BigDecimal = grossAmount - investedAmount
      def grossGainPct: BigDecimal =
        if (investedAmount == 0) BigDecimal(0)
        else scalePercents((grossGain * 100) / investedAmount)
    }

    // Gets a fund NAV at given date, from history events and data store NAV history.
    def getNAV(fundId: UUID): Option[Savings.AssetValue] =
      Savings.getNAV(Some(stage), fundId, date, eventsNAVs)

    val assets = savings.assets
    val details = assets.byId.keys.foldLeft(AccountDetails(0, 0)) { (details, assetId) =>
      details.copy(
        investedAmount = details.investedAmount + assets.investedAmount(assetId),
        grossAmount = details.grossAmount + getNAV(assetId.fundId).map(nav => assets.amount(assetId, nav.value)).getOrElse(0)
      )
    }

    def coloredAmount(label: Label, value: BigDecimal, suffix: String): Unit = {
      label.setText(formatNumber(value, suffix))
      if (value == 0) JFXStyles.toggleNeutral(label)
      else if (value > 0) JFXStyles.togglePositive(label)
      else JFXStyles.toggleNegative(label)
    }

    val currency = Main.settings.currency.get
    dateLabel.setText(date.toString)
    investedAmountLabel.setText(formatNumber(details.investedAmount, currency))
    grossAmountLabel.setText(formatNumber(details.grossAmount, currency))
    coloredAmount(grossGainLabel, details.grossGain, currency)
    coloredAmount(grossGainPctLabel, details.grossGainPct, "%")
  }

}

object AccountHistoryController {

  private val settingsKeyPrefix = "account-history"

  private val stageLocation = ConfigEntry.from[StageLocation](Main.settings.settings,
    Settings.prefix ++ Seq(Settings.KEY_STAGE, settingsKeyPrefix, Settings.KEY_LOCATION))

  private val splitPaneDividerPositions: ConfigEntry[String] = ConfigEntry.from[String](Main.settings.settings,
    Settings.prefix ++ Seq(Settings.KEY_STAGE, settingsKeyPrefix, "splitPane", "dividerPositions"))

  private val splitPane2DividerPositions: ConfigEntry[String] = ConfigEntry.from[String](Main.settings.settings,
    Settings.prefix ++ Seq(Settings.KEY_STAGE, settingsKeyPrefix, "splitPane", "2", "dividerPositions"))

  private val historyColumnsPref: ConfigEntry[String] = ConfigEntry.from[String](Main.settings.settings,
    Settings.prefix ++ Seq(Settings.KEY_STAGE, settingsKeyPrefix, "history", "columns"))

  def title: String = Strings.accountHistory

  case class AssetEventItem(event: Savings.Event, index: Int, date: Option[LocalDate], desc: String, comment: Option[String]) extends Ordered[AssetEventItem] {
    var row: Option[TreeTableRow[AssetEventItem]] = None

    // Define order for items: by index then date
    // As a side effect this gives Comparable implementation for Java sorting.
    import scala.math.Ordered.orderingToOrdered
    override def compare(that: AssetEventItem): Int = (index, date).compare((that.index, that.date))
  }

  object AssetEventItem {

    def apply(event: Savings.Event, index: Int, date: LocalDate, desc: String, comment: Option[String]): AssetEventItem =
      AssetEventItem(event, index, Some(date), desc, comment)

    def apply(event: Savings.Event, index: Int, desc: String): AssetEventItem =
      AssetEventItem(event, index, None, desc, None)

  }

  case class History(data: List[HistoryData] = Nil, issues: List[String] = Nil) {
    def addData(more: HistoryData): History = copy(data :+ more)
    def addIssue(issue: String): History = copy(issues = issues :+ issue)
  }

  case class HistoryData(date: LocalDate, investedAmount: BigDecimal = BigDecimal(0), grossAmount: BigDecimal = BigDecimal(0)) {
    def addInvestedAmount(v: BigDecimal): HistoryData = copy(investedAmount = investedAmount + v)
    def addGrossAmount(v: BigDecimal): HistoryData = copy(grossAmount = grossAmount + v)
  }

  case class HistoryMark(date: LocalDate, items: List[AssetEventItem]) extends ChartMark {
    override val comment = Some(items.map(_.desc).mkString("\n"))
  }

  /** Builds a stage out of this controller. */
  def buildStage(mainController: MainController, state: State): Stage = {
    val stage = new Stage()
    stage.getIcons.setAll(Images.iconClockHistory)
    stage.setTitle(title)

    val loader = new FXMLLoader(getClass.getResource("/fxml/account-history.fxml"), I18N.getResources)
    stage.setScene(new Scene(loader.load[Parent]()))
    stage.getScene.getStylesheets.add(getClass.getResource("/css/main.css").toExternalForm)
    SplitPaneSkinEx.addStylesheet(stage.getScene)
    val controller = loader.getController[AccountHistoryController]
    controller.initialize(mainController, stage, state)

    Stages.addPersistence(stage, controller)

    stage
  }

}
