package epsa.controllers

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.StrictLogging
import epsa.{I18N, Main, Settings}
import epsa.I18N.Strings
import epsa.charts.{ChartHandler, ChartMark, ChartMeta, ChartSettings}
import epsa.model._
import epsa.storage.DataStore
import epsa.util.Awaits
import java.io.PrintWriter
import java.nio.file.Path
import java.time.LocalDate
import java.util.UUID
import javafx.event.{ActionEvent, EventTarget}
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Node, Parent, Scene}
import javafx.scene.layout.{AnchorPane, GridPane}
import javafx.scene.control._
import javafx.scene.image.ImageView
import javafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import javafx.stage._
import scala.collection.JavaConverters._
import scala.util.Success
import suiryc.scala.RichOption._
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.scene.Graphics
import suiryc.scala.javafx.scene.control.skin.SplitPaneSkinEx
import suiryc.scala.javafx.scene.control.{Dialogs, Panes, TableViews}
import suiryc.scala.javafx.stage.{PathChoosers, StageLocationPersistentView, Stages}
import suiryc.scala.math.Ordered._
import suiryc.scala.settings.ConfigEntry

// TODO: smart deletion of funds ?
//         - keep the necessary data (NAV on some dates) used to compute levies
//         - way to determine if all levies of past fund assets were paid already, so that all NAVs can really be deleted ?
class MainController extends StageLocationPersistentView(MainController.stageLocation, first = true) with StrictLogging {

  import MainController._

  @FXML
  protected var fileCloseMenu: MenuItem = _

  @FXML
  protected var fileSaveMenu: MenuItem = _

  @FXML
  protected var fileSaveAsMenu: MenuItem = _

  @FXML
  protected var editUndoMenu: MenuItem = _

  @FXML
  protected var viewSavingsOnDateMenu: MenuItem = _

  @FXML
  protected var viewAccountHistoryMenu: MenuItem = _

  @FXML
  protected var viewNetAssetValueHistoryMenu: MenuItem = _

  @FXML
  protected var totalsPerSchemeMenu: CheckMenuItem = _

  @FXML
  protected var totalsPerFundMenu: CheckMenuItem = _

  @FXML
  protected var totalsPerAvailabilityMenu: CheckMenuItem = _

  @FXML
  protected var vwapPerAssetMenu: CheckMenuItem = _

  @FXML
  protected var upToDateAssetsMenu: CheckMenuItem = _

  @FXML
  protected var toolsExportRawAccountHistoryMenu: MenuItem = _

  @FXML
  protected var splitPane: SplitPane = _

  @FXML
  protected var assetDetails: GridPane = _

  @FXML
  protected var navHistoryPane: AnchorPane = _

  // TODO: allow to re-arrange tabs order
  // Wait for implementation in JavaFX ? https://bugs.openjdk.java.net/browse/JDK-8092098
  @FXML
  protected var tabPane: TabPane = _

  lazy protected val stage: Stage = splitPane.getScene.getWindow.asInstanceOf[Stage]

  lazy private val toDateSavingsViewTab: SavingsViewTab = addSavingsOnDateTab(None, init = true)

  private var chartHandler: ChartHandler[ChartMark] = _

  private var currentChartFund: Option[Savings.Fund] = None

  private var requestedChartFund: Option[Savings.Fund] = None

  private[epsa] var actor: ActorRef = _

  def initialize(state: State): Unit = {
    // Note: make the actor name unique (with timestamp) so that it can be
    // recreated later.
    actor = JFXSystem.newJFXActor(
      ControllerActor.props(state),
      s"epsa-main@${System.currentTimeMillis}"
    )

    // Note: in previous versions of JavaFx (before 8u60), it could be difficult
    // or impossible to ensure some changes (like scheme/fund name change) were
    // applied in current table being displayed. To circumvent this, a binding
    // could be created from the state property (table user data) to execute a
    // function which would retrieve table cells value.
    // It should not be necessary anymore as we reload the stage when necessary
    // (option changes) or can still ask to 'refresh' the table.
    // In any case, we sometimes need to get the current state. So store it as
    // user data.
    state.save()

    // Handle displaying settings
    for {
      (menu, pref) <- List(
        (totalsPerSchemeMenu, totalsPerScheme),
        (totalsPerFundMenu, totalsPerFund),
        (totalsPerAvailabilityMenu, totalsPerAvailability),
        (vwapPerAssetMenu, vwapPerAsset),
        (upToDateAssetsMenu, upToDateAssets)
      )
    } {
      // First select menu according to saved settings
      menu.setSelected(pref.get)
      // Then listen for changes to save new value in settings, then refresh the view
      menu.selectedProperty.listen { selected =>
        pref.set(selected)
        refresh()
      }
    }

    navHistoryPane.setVisible(false)
    val meta = ChartMeta[ChartMark]()
    chartHandler = new ChartHandler(
      seriesName = "",
      seriesValues = Nil,
      meta = meta,
      settings = ChartSettings.hidden.copy(
        xLabel = Strings.date,
        yLabel = Strings.nav,
        ySuffix = Main.settings.currency.get
      )
    )
    val chartPane = chartHandler.chartPane
    AnchorPane.setTopAnchor(chartPane, 0.0)
    AnchorPane.setRightAnchor(chartPane, 0.0)
    AnchorPane.setBottomAnchor(chartPane, 0.0)
    AnchorPane.setLeftAnchor(chartPane, 0.0)
    navHistoryPane.getChildren.add(chartPane)
    ()
  }

  /** Restores (persisted) view. */
  override protected def restoreView(): Unit = {
    super.restoreView()

    splitPaneDividerPositions.opt.foreach { dividerPositions =>
      Panes.restoreDividerPositions(splitPane, dividerPositions)
    }
  }

  /** Persists view (stage location, ...). */
  override protected def persistView(): Unit = {
    super.persistView()

    val savingsView = toDateSavingsViewTab.view

    // Persist assets table columns order and width
    assetsColumnsPref.set(TableViews.getColumnsView(savingsView.assetsTable, savingsView.assetsColumns))

    // Persist SplitPane divider positions
    splitPaneDividerPositions.set(Panes.encodeDividerPositions(splitPane))
  }

  protected def refreshTab(tab: TabWithState, state: State): Unit = {
    val refreshData = RefreshData(
      state = state,
      showTotalsPerScheme = totalsPerSchemeMenu.isSelected,
      showTotalsPerFund = totalsPerFundMenu.isSelected,
      showTotalsPerAvailability = totalsPerAvailabilityMenu.isSelected,
      vwapPerAsset = vwapPerAssetMenu.isSelected,
      upToDateAssets = upToDateAssetsMenu.isSelected
    )
    tab.refresh(refreshData)
  }

  protected def addSavingsOnDateTab(dateOpt: Option[LocalDate], init: Boolean = false): SavingsViewTab = {
    val state = getState
    val savingsViewTab = new SavingsViewTab(this, dateOpt)

    // Restore assets columns order and width
    val columnPrefs = if (init) {
      // Add labels to show details of selected asset
      // Note: setting constraints on each row does not seem necessary
      savingsViewTab.view.assetFields.values.groupBy(_.columnIdx).foreach {
        case (columnIdx, fields) =>
          fields.zipWithIndex.foreach {
            case (field, idx) =>
              val label = new Label(field.detailsLabel)
              assetDetails.getChildren.add(label)
              GridPane.setColumnIndex(label, columnIdx * 2)
              GridPane.setRowIndex(label, idx)
              val value = field.detailsValue
              assetDetails.getChildren.add(value)
              GridPane.setColumnIndex(value, columnIdx * 2 + 1)
              GridPane.setRowIndex(value, idx)
          }
      }

      // Update details to actual selected asset in selected tab
      tabPane.getSelectionModel.selectedItemProperty.listen { v =>
        Option(v).map(_.getUserData).foreach {
          case tab: SavingsViewTab => tab.view.updateDetailsValue()
          case _                   => toDateSavingsViewTab.view.updateDetailsValue(None)
        }
      }

      // Restore from preferences for main tab
      assetsColumnsPref.opt
    } else {
      // Apply main tab (current) view to new tabs
      Some(TableViews.getColumnsView(toDateSavingsViewTab.view.assetsTable, toDateSavingsViewTab.view.assetsColumns))
    }
    TableViews.setColumnsView(savingsViewTab.view.assetsTable, savingsViewTab.view.assetsColumns, columnPrefs)

    // Add the tab to the tab pane, and refresh its content
    val tab = savingsViewTab.tab
    tabPane.getTabs.add(tab)
    tabPane.getSelectionModel.select(tab)
    refreshTab(savingsViewTab, state)

    savingsViewTab
  }

  def refresh(reload: Boolean = false): Unit = {
    actor ! Refresh(reload = reload)
  }

  def onCloseRequest(event: WindowEvent): Unit = {
    actor ! OnExit
    // Note: consume the event, the actor is responsible for shutting down
    event.consume()
  }

  def onFileNew(@deprecated("unused","") event: ActionEvent): Unit = {
    actor ! OnFileNew
  }

  def onFileOpen(@deprecated("unused","") event: ActionEvent): Unit = {
    actor ! OnFileOpen
  }

  def onFileClose(@deprecated("unused","") event: ActionEvent): Unit = {
    actor ! OnFileClose
  }

  def onFileSave(@deprecated("unused","") event: ActionEvent): Unit = {
    actor ! OnFileSave(saveAs = false)
  }

  def onFileSaveAs(@deprecated("unused","") event: ActionEvent): Unit = {
    actor ! OnFileSave(saveAs = true)
  }

  def onExit(@deprecated("unused","") event: ActionEvent): Unit = {
    actor ! OnExit
  }

  def onEditUndo(@deprecated("unused","") event: ActionEvent): Unit = {
    actor ! OnEditUndo
  }

  def onEditSchemes(@deprecated("unused","") event: ActionEvent): Unit = {
    actor ! OnEditSchemes(getSelectedAsset.map(_.schemeId))
  }

  def onEditFunds(@deprecated("unused","") event: ActionEvent): Unit = {
    actor ! OnEditFunds(getSelectedAsset.map(_.fundId))
  }

  def onEditUnavailabilityPeriods(@deprecated("unused","") event: ActionEvent): Unit = {
    actor ! OnEditUnavailabilityPeriods
  }

  def onNewPayment(@deprecated("unused","") event: ActionEvent): Unit = {
    actor ! OnNewAssetAction(AssetActionKind.Payment, getSelectedAsset)
  }

  def onNewTransfer(@deprecated("unused","") event: ActionEvent): Unit = {
    actor ! OnNewAssetAction(AssetActionKind.Transfer, getSelectedAsset)
  }

  def onNewRefund(@deprecated("unused","") event: ActionEvent): Unit = {
    actor ! OnNewAssetAction(AssetActionKind.Refund, getSelectedAsset)
  }

  def onOptions(@deprecated("unused","") event: ActionEvent): Unit = {
    actor ! OnOptions
  }

  def onSavingsOnDate(@deprecated("unused","") event: ActionEvent): Unit = {
    actor ! OnSavingsOnDate(None)
  }

  def onAccountHistory(@deprecated("unused","") event: ActionEvent): Unit = {
    actor ! OnAccountHistory
  }

  def onNetAssetValueHistory(@deprecated("unused","") event: ActionEvent): Unit = {
    actor ! OnNetAssetValueHistory(getSelectedAsset.map(_.fundId))
  }

  def showNAVHistory(fund: Option[Savings.Fund]): Unit = {
    requestedChartFund = fund
    actor ! OnShowNAVHistory
  }

  def onLevies(@deprecated("unused","") event: ActionEvent): Unit = {
    actor ! OnLevies
  }

  def onSavingsOnDate(date: LocalDate): Unit = {
    // Prepare to show savings on request date
    actor ! OnSavingsOnDate(Some(date))
    // And bring back the window to front (as we expect another window - from
    // which we are called - to be in front).
    stage.toFront()
  }

  def onExportRawAccountHistory(@deprecated("unused","") event: ActionEvent = null): Unit = {
    actor ! OnExportRawAccountHistory
  }

  def onImportRawAccountHistory(@deprecated("unused","") event: ActionEvent = null): Unit = {
    actor ! OnImportRawAccountHistory
  }

  def onCleanupDataStore(@deprecated("unused","") event: ActionEvent): Unit = {
    actor ! OnCleanupDataStore(normalize = false)
  }

  def onCleanupDataStore(normalize: Boolean): Unit = {
    actor ! OnCleanupDataStore(normalize)
  }

  private def getState: State = {
    stage.getUserData.asInstanceOf[State]
  }

  private def getSelectedAsset: Option[Savings.Asset] =
    Option(tabPane.getSelectionModel.getSelectedItem).map(_.getUserData).flatMap {
      case tab: SavingsViewTab => tab.view.getSelectedAsset
      case _                   => None
    }

  object ControllerActor {
    def props(state: State) = Props(new ControllerActor(state))
  }

  class ControllerActor(state0: State) extends Actor {

    val savingsOnDateStage: Stage = {
      import javafx.scene.control.skin.DatePickerSkin
      import suiryc.scala.javafx.scene.control.Dialogs

      // Note:
      // We can create a DatePicker and associate a DatePickerSkin (calendar
      // popup Node) that we can use wherever we want while having access to
      // the DatePicker action callback.
      // See: http://stackoverflow.com/a/34684268
      // Then we can display the calendar as a modal Node to select the Date
      // to display the savings at.
      val picker = new DatePicker(LocalDate.now)
      val skin = new DatePickerSkin(picker)
      val popup = skin.getPopupContent
      popup.applyCss()

      def onDate(stage: Stage): Unit = {
        addSavingsOnDateTab(Some(picker.getValue))
        stage.close()
      }

      // Cancel request (close stage) on ESC
      // Show savings for selected date on ENTER
      def keyFilter(stage: Stage)(event: KeyEvent): Unit = {
        if (event.getCode == KeyCode.ESCAPE) {
          stage.close()
          event.consume()
        } else if (event.getCode == KeyCode.ENTER) {
          onDate(stage)
          event.consume()
        }
      }
      // Show savings for already selected date if clicked again
      def mouseFilter(stage: Stage)(event: MouseEvent): Unit = {
        @scala.annotation.tailrec
        def matches(target: EventTarget): Boolean =
          target match {
            case cell: DateCell =>
              // Currently selected date has the "selected" class style
              cell.getStyleClass.contains("selected")

            case node: Node =>
              // Since the cell has children, also pick clicks on them
              val parent = node.getParent
              (parent != null) && matches(parent)

            case _ =>
              false
          }
        if (matches(event.getTarget)) {
          onDate(stage)
          event.consume()
        }
      }
      Dialogs.modalNode(
        splitPane.getScene.getWindow,
        { stage: Stage =>
          picker.setOnAction { _: ActionEvent =>
            onDate(stage)
          }
          stage.addEventFilter(KeyEvent.KEY_PRESSED, keyFilter(stage) _)
          stage.addEventFilter(MouseEvent.MOUSE_CLICKED, mouseFilter(stage) _)
          popup
        }
      )
    }

    refresh(state0, updateAssetsValue = true)

    override def receive: Receive = receive(state0)

    // Wrap actual partial function in a try/catch to display unexpected issues.
    // Otherwise window becomes unusable (akka messages goes to dead letters).
    def receive(state: State): Receive =
      new PartialFunction[Any, Unit]() {
        val r: Receive = receive0(state)
        override def isDefinedAt(x: Any): Boolean = r.isDefinedAt(x)
        override def apply(x: Any): Unit = try {
          r.apply(x)
        } catch {
          case ex: Exception =>
            Dialogs.error(
              owner = Some(state.stage),
              title = Some(state.stage.getTitle),
              contentText = Some(Strings.unexpectedIssue),
              ex = Some(ex)
            )
            ()
        }
      }

    def receive0(state: State): Receive = {
      case Refresh(r)        => if (r) reload(state) else refresh(state, updateAssetsValue = true)
      case OnFileNew         => onFileNew(state)
      case OnFileOpen        => onFileOpen(state)
      case OnFileClose       => onFileClose(state)
      case OnFileSave(saveAs) => onFileSave(state, saveAs)
      case OnExit            => onExit(state)
      case OnEditUndo        => onEditUndo(state)
      case OnEditSchemes(id) => onEditSchemes(state, id.flatMap(state.savings.findScheme))
      case OnEditFunds(id)   => onEditFunds(state, id.flatMap(state.savings.findFund))
      case OnEditUnavailabilityPeriods => onEditUnavailabilityPeriods(state)
      case OnNewAssetAction(kind, asset) => onNewAssetAction(state, kind, asset)
      case OnOptions         => onOptions(state)
      case OnSavingsOnDate(date) => onSavingsOnDate(date)
      case OnAccountHistory  => onAccountHistory(state)
      case OnNetAssetValueHistory(fundId) => onNetAssetValueHistory(state, fundId)
      case OnShowNAVHistory  => onShowNAVHistory(state)
      case OnLevies          => onLevies(state)
      case OnExportRawAccountHistory => onExportRawAccountHistory(state)
      case OnImportRawAccountHistory => onImportRawAccountHistory(state)
      case OnCleanupDataStore(normalize) => onCleanupDataStore(state, normalize)
    }

    def processEvents(oldState: State, events: List[Savings.Event], updateAssetsValue: Boolean = false): State = {
      // Time to delete NAV history upon deleting fund
      events.collect {
        case Savings.DeleteFund(fundId) => DataStore.AssetHistory.deleteValues(fundId)
      }
      // Note: there is no need to sort events since it only concerns actions
      // and there may only be one given from caller. So we can directly write
      // the event(s) in the datastore.
      // But don't bother if there is actually no event to save&process.
      val savings =
        if (events.isEmpty) oldState.savings
        else {
          Awaits.writeDataStoreEvents(oldState.stage, events)
          oldState.savings.processEvents(events)
        }
      // Then update state
      val newAssetsValue =
        if (!updateAssetsValue) oldState.assetsValue
        else {
          val date = toDateSavingsViewTab.getDateOpt(savings, upToDateAssetsMenu.isSelected).getOrElse(LocalDate.now)
          savings.getNAVs(Some(oldState.stage), date)
        }
      val state = oldState.copy(savings = savings, assetsValue = newAssetsValue)

      refreshDirty(state)

      // First update state associated to assets table: takes care of
      // schemes/funds updated names if any.
      state.save()
      // Then update tabs table content: takes care of added/removed entries
      refreshTabs(state)

      context.become(receive(state))
      state
    }

    private def refreshDirty(state: State): Unit = {
      val dirty = state.hasPendingChanges

      fileCloseMenu.setDisable(DataStore.dbOpened.isEmpty)
      fileSaveMenu.setDisable(!dirty)
      // Don't enable 'Save as' if there is no physical DB yet.
      fileSaveAsMenu.setDisable(!dirty || DataStore.dbOpened.isEmpty)
      editUndoMenu.setDisable(!dirty)
      viewNetAssetValueHistoryMenu.setDisable(state.savings.funds.isEmpty)
      val hasHistory = Awaits.hasDataStoreEvents(state.stage).getOrElse(false)
      viewSavingsOnDateMenu.setDisable(!hasHistory)
      viewAccountHistoryMenu.setDisable(!hasHistory)
      toolsExportRawAccountHistoryMenu.setDisable(!hasHistory)

      val title = DataStore.dbOpened.map { name =>
        s"[$name${if (dirty) " *" else ""}] - "
      }.getOrElse(if (dirty) " * - " else "") +
        epsa.Main.versionedName
      state.stage.setTitle(title)
    }

    private def refresh(state: State, updateAssetsValue: Boolean = false): Unit = {
      // Cheap trick to fill fields with Savings data
      processEvents(state, Nil, updateAssetsValue)
      ()
    }

    def reload(state0: State): Unit = {
      val levies = Awaits.readAppSetting(state0.stage, DataStore.AppSettings.KEY_LEVIES).getOrElse(None).map { str =>
        import spray.json._
        import Levies.JsonProtocol._
        str.parseJson.convertTo[Levies].normalized
      }.getOrElse(Levies.empty)
      val state = Awaits.readDataStoreEvents(state0.stage) match {
        case Success(events0) =>
          val (events, modified) = Savings.normalizeEvents(events0)
          // Cleanup datastore if necessary
          MainController.this.onCleanupDataStore(modified)
          val savings = Savings(levies = levies).processEvents(events)
          State(
            stage = state0.stage,
            savings = savings
          )

        case _ =>
          val savings = Savings(levies = levies)
          State(
            stage = state0.stage,
            savings = savings
          )
      }

      // Note: applying state also refreshes tabs
      refresh(state, updateAssetsValue = true)
    }

    def refreshTabs(state: State): Unit = {
      tabPane.getTabs.asScala.map(_.getUserData).foreach {
        case tab: TabWithState => refreshTab(tab, state)
        case _                 =>
      }
    }

    def onFileNew(state: State): Unit = {
      // We actually do the same than when closing current file.
      // Code works even if no current file is opened (with or without pending
      // changes).
      onFileClose(state)
    }

    def onFileOpen(state: State): Unit = {
      if (checkPendingChanges(state)) open(state)
    }

    def onFileClose(state: State): Unit = {
      if (checkPendingChanges(state)) {
        DataStore.close()
        val newState = State(stage = state.stage)
        refresh(newState)
      }
    }

    def onFileSave(state: State, saveAs: Boolean): Unit = {
      // Note: upon successful saving, we need to refresh the view (so that the
      // visual hint about pending changes disappears).
      if (save(state, saveAs)) refreshDirty(state)
      // else: either it was cancelled by user, or it failed (and user was
      // notified).
    }

    def onExit(state: State): Unit = {
      if (checkPendingChanges(state)) {
        persistView()

        context.stop(self)
        epsa.Main.shutdown(state.stage)
      }
    }

    def onEditUndo(state: State): Unit = {
      val resp = Dialogs.confirmation(
        owner = Some(state.stage),
        title = None,
        headerText = Some(Strings.irreversibleAction),
        contentText = Some(Strings.undoPending)
      )

      if (resp.contains(ButtonType.OK)) {
        DataStore.undoChanges()
        reload(state)
      }
    }

    def onEditSchemes(state: State, edit: Option[Savings.Scheme]): Unit = {
      val dialog = EditSchemesController.buildDialog(state.stage, state.savings, edit)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.setResizable(true)
      val events = dialog.showAndWait().orElse(Nil)
      if (events.nonEmpty) processEvents(state, events)
      ()
    }

    def onEditFunds(state: State, edit: Option[Savings.Fund]): Unit = {
      val dialog = EditFundsController.buildDialog(state.stage, state.savings, edit)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.setResizable(true)
      val events = dialog.showAndWait().orElse(Nil)
      if (events.nonEmpty) processEvents(state, events)
      ()
    }

    def onEditUnavailabilityPeriods(state: State): Unit = {
      val dialog = EditUnavailabilityPeriodsController.buildDialog(state.stage)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.setResizable(true)
      if (dialog.showAndWait().orElse(false)) refreshDirty(state)
    }

    def onNewAssetAction(state: State, kind: AssetActionKind.Value, asset: Option[Savings.Asset]): Unit = {
      val dialog = NewAssetActionController.buildDialog(state.stage, MainController.this, state.savings, kind, asset)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.setResizable(true)
      val event = dialog.showAndWait().orElse(None)
      if (event.isDefined) {
        // Get back to main tab (so that we can see the new action result)
        tabPane.getSelectionModel.select(toDateSavingsViewTab.tab)
        if (state.savings.latestAssetAction.exists(event.get.date < _)) {
          // This new event predates latest asset action. We need to reorder
          // history as processing it may fail with current savings (assets not
          // existing anymore due to availability dates reached in-between).

          // First read (and order) history and apply new event
          val events = Awaits.getEventsHistory(state.stage, extra = event.toList)
          // Then process events to get up to date savings
          val savingsUpd = Savings(levies = state.savings.levies).processEvents(events)
          // Purge current history
          Awaits.purgeDataStoreEvents(state.stage)
          // And rewrite reordered history.
          Awaits.writeDataStoreEvents(state.stage, events)
          val state2 = state.copy(savings = savingsUpd)
          // Now we can refresh the view
          refresh(state2, updateAssetsValue = true)
        } else {
          // Refresh NAVs as there may be new assets
          processEvents(state, event.toList, updateAssetsValue = true)
          ()
        }
      }
    }

    def onOptions(state: State): Unit = {
      val dialog = OptionsController.buildDialog(state.stage)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.setResizable(true)
      val (reload, needRestart) = dialog.showAndWait().orElse((false, false))
      if (reload) {
        // Reset I18N cache to apply any language change
        I18N.reset()
        // Persist now to restore it when rebuilding the stage
        persistView()
        context.stop(self)
        MainController.build(state, needRestart)
      }
    }

    def onSavingsOnDate(dateOpt: Option[LocalDate]): Unit = {
      // Show the modal stage, which requires to select a date then creates
      // a new Tab to display savings on the selected date.
      dateOpt match {
        case Some(date) => addSavingsOnDateTab(Some(date))
        case None       => savingsOnDateStage.show()
      }
      ()
    }

    def onAccountHistory(state: State): Unit = {
      val stage = AccountHistoryController.buildStage(MainController.this, state)
      stage.initModality(Modality.NONE)
      stage.setResizable(true)
      stage.show()
    }

    def onNetAssetValueHistory(state: State, fundId: Option[UUID]): Unit = {
      val dialog = NetAssetValueHistoryController.buildDialog(MainController.this, state.savings, fundId, state.stage)
      // Notes:
      // Don't set as modal, since we wish to display the window while still
      // interacting with the main stage.
      // Don't set owner, otherwise the new windows remains in front of its
      // owner.
      dialog.initModality(Modality.NONE)
      dialog.setResizable(true)
      dialog.show()
    }

    def onShowNAVHistory(state: State): Unit = {
      if (requestedChartFund != currentChartFund) requestedChartFund match {
        case Some(fund) =>
          val values = Awaits.readDataStoreNAVs(state.stage, fund.id).getOrElse(Nil)
          // Only show non-empty series
          if (values.nonEmpty) {
            if (chartHandler.series.getData.isEmpty) {
              chartHandler.centerOnDate(values.last.date, track = true)
            }
            chartHandler.setSeriesName(fund.name)
            chartHandler.updateSeries(values, replace = true, keepCenter = false)
            navHistoryPane.setVisible(true)
          } else {
            navHistoryPane.setVisible(false)
          }

        case None =>
          navHistoryPane.setVisible(false)
      }
      currentChartFund = requestedChartFund
    }

    def onLevies(state: State): Unit = {
      val dialog = LeviesController.buildDialog(state.savings, state.stage)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.setResizable(true)
      if (dialog.showAndWait().orElse(false)) reload(state)
    }

    def onExportRawAccountHistory(state: State): Unit = {
      val events = Awaits.getEventsHistory(state.stage)
      import spray.json._
      import Savings.JsonProtocol._
      val eventsJson = events.map(_.toJson)
      val historyJson = JsObject(
        "history" -> JsArray(eventsJson:_*)
      )
      val history = historyJson.prettyPrint

      val fileChooser = new FileChooser()
      fileChooser.setTitle(Strings.exportRawAccountHistory)
      fileChooser.getExtensionFilters.addAll(
        new FileChooser.ExtensionFilter(Strings.jsonFiles, "*.json")
      )
      accountHistoryPath.opt.foreach { path =>
        PathChoosers.setInitialPath(fileChooser, path.toFile)
      }
      val selectedFile = fileChooser.showSaveDialog(state.stage)
      Option(selectedFile).foreach { file =>
        try {
          val writer = new PrintWriter(file, "UTF-8")
          writer.write(history)
          writer.close()
          accountHistoryPath.set(selectedFile.toPath)
          if (writer.checkError) {
            Dialogs.error(
              owner = Some(state.stage),
              title = Some(Strings.exportRawAccountHistory),
              contentText = Some(Strings.fileWriteError),
              ex = None
            )
          }
        } catch {
          case ex: Exception =>
            Dialogs.error(
              owner = Some(state.stage),
              title = Some(Strings.exportRawAccountHistory),
              contentText = Some(Strings.fileWriteError),
              ex = Some(ex)
            )
        }
      }
    }

    def onImportRawAccountHistory(state: State): Unit = {
      val pendingChanges = state.hasPendingChanges
      val doImport = if (!toolsExportRawAccountHistoryMenu.isDisable || pendingChanges) {
        // If there is history to export, or pending changes, ask confirmation as we will replace history/changes
        val resp = Dialogs.confirmation(
          owner = Some(state.stage),
          title = Some(Strings.importRawAccountHistory),
          headerText = if (pendingChanges) Some(Strings.irreversibleAction) else Some(Strings.confirmAction),
          contentText = Some(Strings.importRawAccountHistory)
        )

        resp.contains(ButtonType.OK)
      } else true

      if (doImport) {
        val fileChooser = new FileChooser()
        fileChooser.setTitle(Strings.importRawAccountHistory)
        fileChooser.getExtensionFilters.addAll(
          new FileChooser.ExtensionFilter(Strings.jsonFiles, "*.json")
        )
        accountHistoryPath.opt.foreach { path =>
          PathChoosers.setInitialPath(fileChooser, path.toFile)
        }
        val selectedFile = fileChooser.showOpenDialog(state.stage)
        Option(selectedFile).flatMap { file =>
          // We could try to use typesafe Config (HOCON), which would allow to
          // handle variables for shared values. But it does not handle decimal
          // values as BigDecimal so we could lose some precision.
          try {
            import scala.io.Source
            import spray.json._
            import Savings.JsonProtocol._
            val history = {
              val source = Source.fromFile(file, "UTF-8")
              try {
                source.mkString
              } finally {
                source.close()
              }
            }.parseJson.asJsObject.fields("history").asInstanceOf[JsArray]
            val events = history.elements.toList.map(_.convertTo[Savings.Event])
            // Note: there is no need to reorder events since 'processEvents' will do it
            Some(events)
          } catch {
            case ex: Exception =>
              Dialogs.error(
                owner = Some(state.stage),
                title = Some(Strings.importRawAccountHistory),
                contentText = Some(Strings.fileReadError),
                ex = Some(ex)
              )
              None
          }
        }.foreach { events =>
          accountHistoryPath.set(selectedFile.toPath)
          // Purge events history, and replay imported history
          Awaits.purgeDataStoreEvents(state.stage)
          val newState = processEvents(state.zero(Savings(levies = state.savings.levies)), events, updateAssetsValue = true)
          // Then cleanup data store (no need to reorder as we did it already)
          onCleanupDataStore(newState, normalize = false)
        }
      }
    }

    def onCleanupDataStore(state: State, normalize: Boolean): Unit = {
      Awaits.cleanupDataStore(state.stage, state.savings.funds.map(_.id), normalize)
      // Note: don't bother reloading or refreshing tabs since history was
      // already sorted when populating them.
      // We still need to refresh the dirty state for potential new pending
      // changes.
      refreshDirty(state)
    }

    /**
     * Checks pending changes (before losing them).
     *
     * If changes are pending, ask for user confirmation, allowing to save them
     * before continuing.
     *
     * @return whether we can continue (changes saved or can be lost)
     */
    private def checkPendingChanges(state: State): Boolean =
      if (state.hasPendingChanges) {
        // There are pending changes. Ask user for confirmation, allowing
        // to save before continuing.
        val buttonSaveType = new ButtonType(fileSaveMenu.getText, ButtonBar.ButtonData.OK_DONE)
        val alert = new Alert(Alert.AlertType.CONFIRMATION, "",
          ButtonType.OK, ButtonType.CANCEL, buttonSaveType)
        Stages.initOwner(alert, state.stage)
        alert.setHeaderText(Strings.pendingChanges)

        // Filter action on "Save" button to trigger saving and check result:
        // If saving failed (user was notified), consume event to get back to
        // confirmation dialog.
        val buttonSave = alert.getDialogPane.lookupButton(buttonSaveType)
        buttonSave.asInstanceOf[Button].setGraphic(new ImageView(Images.iconDisk))
        buttonSave.addEventFilter(ActionEvent.ACTION, (event: ActionEvent) => {
          if (!save(state, owner = Some(Stages.getStage(alert)))) event.consume()
        })

        val r = alert.showAndWait()
        r.contains(ButtonType.OK) || r.contains(buttonSaveType)
      }
      else true

    private def open(state: State): Unit = {
      Awaits.openDataStore(Some(state.stage), change = true) match {
        case Some(Success(())) => reload(state)
        case _                 =>
      }
    }

    private def save(state: State, saveAs: Boolean = false, owner: Option[Window] = None): Boolean = {
      // Notes:
      // Make sure to not both lock JavaFX (e.g. waiting for a Future) and try
      // to use it (e.g. Dialog to show upon issue).
      // For simplicity, we waits for result and display issue after receiving
      // it.
      // When 'saving as', we need to make sure the current real DB content has
      // been copied to tmp before opening the (new) physical DB to save the
      // content.
      val actualOwner = owner.getOrElse(state.stage)

      def save() =
        Awaits.saveDataStoreChanges(actualOwner, fullDb = saveAs).isSuccess

      DataStore.dbOpened match {
        case Some(_) if !saveAs => save()
        case _                  =>
          // Data store not opened yet: open then save
          Awaits.openDataStore(Some(actualOwner), change = true, save = true, loadTmp = true) match {
            case Some(Success(())) => save()
            case _                 => false
          }
      }
    }

  }

}

object MainController {

  import Stages.StageLocation

  private val settingsKeyPrefix = "main"

  private val stageLocation = ConfigEntry.from[StageLocation](Main.settings.settings,
    Settings.KEY_SUIRYC, Settings.KEY_EPSA, Settings.KEY_STAGE, settingsKeyPrefix, Settings.KEY_LOCATION)

  private val splitPaneDividerPositions: ConfigEntry[String] = ConfigEntry.from[String](Main.settings.settings,
    Settings.KEY_SUIRYC, Settings.KEY_EPSA, Settings.KEY_STAGE, settingsKeyPrefix, "splitPane", "dividerPositions")

  private val assetsColumnsPref: ConfigEntry[String] = ConfigEntry.from[String](Main.settings.settings,
    Settings.KEY_SUIRYC, Settings.KEY_EPSA, Settings.KEY_STAGE, settingsKeyPrefix, "assets", "columns")

  private val accountHistoryPath = ConfigEntry.from[Path](Main.settings.settings,
    Settings.KEY_SUIRYC, Settings.KEY_EPSA, "account", "history", "path")

  private val totalsPerScheme = ConfigEntry.from[Boolean](Main.settings.settings,
    Settings.KEY_SUIRYC, Settings.KEY_EPSA, "totals", "per-scheme")

  private val totalsPerFund = ConfigEntry.from[Boolean](Main.settings.settings,
    Settings.KEY_SUIRYC, Settings.KEY_EPSA, "totals", "per-fund")

  private val totalsPerAvailability = ConfigEntry.from[Boolean](Main.settings.settings,
    Settings.KEY_SUIRYC, Settings.KEY_EPSA, "totals", "per-availability")

  private val vwapPerAsset = ConfigEntry.from[Boolean](Main.settings.settings,
    Settings.KEY_SUIRYC, Settings.KEY_EPSA, "vwap", "per-asset")

  private val upToDateAssets = ConfigEntry.from[Boolean](Main.settings.settings,
    Settings.KEY_SUIRYC, Settings.KEY_EPSA, "up-to-date-assets")

  case class State(
    stage: Stage,
    savings: Savings = Savings(),
    assetsValue: Map[UUID, Savings.AssetValue] = Map.empty
  ) {

    def save(): Unit = stage.setUserData(this)

    def hasPendingChanges: Boolean =
      DataStore.hasPendingChanges

    /** Resets state to zero. */
    def zero(savings: Savings = Savings()): State =
      copy(
        savings = savings,
        assetsValue = Map.empty
      )

  }

  case class RefreshData(
    state: State,
    showTotalsPerScheme: Boolean,
    showTotalsPerFund: Boolean,
    showTotalsPerAvailability: Boolean,
    vwapPerAsset: Boolean,
    upToDateAssets: Boolean
  )

  trait TabWithState {
    def refresh(data: RefreshData)
  }

  case class Refresh(reload: Boolean)

  case object OnFileNew

  case object OnFileOpen

  case object OnFileClose

  case class OnFileSave(saveAs: Boolean)

  case object OnExit

  case object OnEditUndo

  case class OnEditSchemes(schemeId: Option[UUID])

  case class OnEditFunds(fundId: Option[UUID])

  case object OnEditUnavailabilityPeriods

  case class OnNewAssetAction(kind: AssetActionKind.Value, asset: Option[Savings.Asset])

  case object OnOptions

  case class OnSavingsOnDate(date: Option[LocalDate])

  case object OnAccountHistory

  case class OnNetAssetValueHistory(fundId: Option[UUID])

  case object OnShowNAVHistory

  case object OnLevies

  case object OnExportRawAccountHistory

  case object OnImportRawAccountHistory

  case class OnCleanupDataStore(normalize: Boolean)

  def build(state: State, needRestart: Boolean = false, applicationStart: Boolean = false): Unit = {
    val stage = state.stage
    val first = Option(stage.getScene).isEmpty
    stage.getIcons.setAll(Images.iconPiggyBank)

    val loader = new FXMLLoader(getClass.getResource("/fxml/main.fxml"), I18N.getResources)
    val root = loader.load[Parent]()
    val controller = loader.getController[MainController]
    controller.initialize(state)

    if (!first) stage.hide()
    // Delegate closing request to controller
    stage.setOnCloseRequest(controller.onCloseRequest _)
    stage.setScene(new Scene(root))
    stage.getScene.getStylesheets.add(getClass.getResource("/css/main.css").toExternalForm)
    SplitPaneSkinEx.addStylesheet(stage.getScene)
    Graphics.addStylesheet(stage.getScene)

    Stages.addPersistence(stage, controller, persist = false)
    stage.show()

    if (needRestart) {
      Dialogs.information(
        owner = Some(state.stage),
        title = None,
        contentText = Some(Strings.needRestart)
      )
    }

    if (applicationStart) controller.refresh(reload = true)
  }

}
