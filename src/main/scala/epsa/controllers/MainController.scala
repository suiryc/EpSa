package epsa.controllers

import akka.actor.{Actor, ActorRef, Props}
import epsa.I18N
import epsa.I18N.Strings
import epsa.Settings.scalePercents
import epsa.charts.ChartHandler
import epsa.model.Savings
import epsa.storage.DataStore
import epsa.tools.EsaliaInvestmentFundProber
import epsa.util.{Awaits, JFXStyles}
import grizzled.slf4j.Logging
import java.nio.file.Path
import java.time.LocalDate
import java.util.UUID
import javafx.beans.binding.Bindings
import javafx.beans.property.{SimpleObjectProperty, SimpleStringProperty}
import javafx.collections.FXCollections
import javafx.collections.transformation.SortedList
import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene}
import javafx.scene.layout.{GridPane, Region}
import javafx.scene.control._
import javafx.scene.image.{Image, ImageView}
import javafx.scene.input._
import javafx.stage._
import scala.collection.immutable.ListMap
import scala.util.Success
import suiryc.scala.RichOption._
import suiryc.scala.{javafx => jfx}
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.event.EventHandler._
import suiryc.scala.javafx.scene.control.{Dialogs, TableViews}
import suiryc.scala.javafx.stage.{FileChoosers, Stages}
import suiryc.scala.javafx.util.Callback
import suiryc.scala.settings.Preference

// TODO: display more information in assets table and details: net gain/loss (amount/percentage)
// TODO: change details pane position; set below table ? (then have NAV history graph on the right side of details)
// TODO: menu entries with latest datastore locations ?
// TODO: menu entry and dialog to display/edit events history ?
// TODO: when computing assets, order by scheme/fund/availability ?
// TODO: manage encryption of datastore ?
//         -> possible to read/write
//         -> how to determine beforehand ?
//         -> FileChooser does not allow to customize its content (to give password upon open)
class MainController extends Logging {

  import epsa.Settings.prefs
  import MainController._
  import Preference._
  import Stages.StageLocation

  private val stageLocation = Preference.from("stage.main.location", null:StageLocation)

  private val splitPaneDividerPositions = Preference.from("stage.main.splitPane.dividerPositions", null:String)

  private val assetsColumnsPref = Preference.from("stage.main.assets.columns", null:String)

  @FXML
  protected var fileCloseMenu: MenuItem = _

  @FXML
  protected var fileSaveMenu: MenuItem = _

  @FXML
  protected var editUndoMenu: MenuItem = _

  @FXML
  protected var viewNetAssetValueHistoryMenu: MenuItem = _

  @FXML
  protected var splitPane: SplitPane = _

  @FXML
  protected var assetDetails: GridPane = _

  @FXML
  protected var assetsTable: TableView[AssetDetails] = _

  private val clipboard = Clipboard.getSystemClipboard

  private val CTRL_C = new KeyCodeCombination(KeyCode.C, KeyCombination.CONTROL_DOWN)

  private var actor: ActorRef = _

  private val assetFields = AssetField.fields()

  private val assetsColumns = assetFields.mapValues(_.column).toList

  private val columnAmount = new TableColumn[AssetDetails, Nothing](Strings.amount)

  columnAmount.getColumns.addAll(assetFields(ASSET_KEY_INVESTED_AMOUNT).column, assetFields(ASSET_KEY_GROSS_AMOUNT).column)

  private val columnGain = new TableColumn[AssetDetails, Nothing](Strings.gain)

  columnGain.getColumns.addAll(assetFields(ASSET_KEY_GROSS_GAIN).column, assetFields(ASSET_KEY_GROSS_GAIN_PCT).column)

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
    val stateProperty = new SimpleObjectProperty[State](state)
    assetsTable.setTableMenuButtonVisible(true)
    assetsTable.setUserData(stateProperty)

    // Note: Asset gives scheme/fund UUID. Since State is immutable (and is
    // changed when applying events in controller) we must delegate scheme/fund
    // lookup to the controller.
    assetsTable.setRowFactory(Callback { newAssetRow() })

    // Show details of selected asset
    // Note: setting constraints on each row does not seem necessary
    assetFields.values.zipWithIndex.foreach {
      case (field, idx) =>
        val label = new Label(field.detailsLabel)
        assetDetails.getChildren.add(label)
        GridPane.setColumnIndex(label, 0)
        GridPane.setRowIndex(label, idx)
        val value = field.detailsValue
        assetDetails.getChildren.add(value)
        GridPane.setColumnIndex(value, 1)
        GridPane.setRowIndex(value, idx)
    }

    assetsTable.getSelectionModel.selectedItemProperty.listen { assetDetails =>
      val assetDetailsOpt = Option(assetDetails)
      assetFields.values.foreach(_.updateDetailsValue(assetDetailsOpt))
    }

    // Handle 'Ctrl-c' to copy asset information.
    assetsTable.addEventHandler(KeyEvent.KEY_PRESSED, { (event: KeyEvent) =>
      if (CTRL_C.`match`(event)) Option(assetsTable.getSelectionModel.getSelectedItem).foreach(copyAssetDetailsToClipboard)
    })
  }

  /** Restores (persisted) view. */
  private def restoreView(stage: Stage): Unit = {
    // Restore stage location
    Option(stageLocation()).foreach { loc =>
      Stages.setLocation(stage, loc, setSize = true)
    }

    // Restore assets columns order and width
    TableViews.setColumnsView(assetsTable, assetsColumns, Option(assetsColumnsPref()))

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
  private def persistView(state: State): Unit = {
    // Persist stage location
    // Note: if iconified, resets it
    stageLocation() = Stages.getLocation(state.stage).orNull

    // Persist assets table columns order and width
    assetsColumnsPref() = TableViews.getColumnsView(assetsTable, assetsColumns)

    // Persist SplitPane divider positions
    splitPaneDividerPositions() = splitPane.getDividerPositions.mkString(";")
  }

  def refresh(): Unit = {
    actor ! Refresh
  }

  def onCloseRequest(event: WindowEvent): Unit = {
    actor ! OnExit
    // Note: consume the event, the actor is responsible for shutting down
    event.consume()
  }

  def onFileNew(event: ActionEvent): Unit = {
    actor ! OnFileNew
  }

  def onFileOpen(event: ActionEvent): Unit = {
    actor ! OnFileOpen
  }

  def onFileClose(event: ActionEvent): Unit = {
    actor ! OnFileClose
  }

  def onFileSave(event: ActionEvent): Unit = {
    actor ! OnFileSave
  }

  def onExit(event: ActionEvent): Unit = {
    actor ! OnExit
  }

  def onEditUndo(event: ActionEvent): Unit = {
    actor ! OnEditUndo
  }

  def onEditSchemes(event: ActionEvent): Unit = {
    actor ! OnEditSchemes(getSelectedAsset.map(_.schemeId))
  }

  def onEditFunds(event: ActionEvent): Unit = {
    actor ! OnEditFunds(getSelectedAsset.map(_.fundId))
  }

  def onNewPayment(event: ActionEvent): Unit = {
    actor ! OnNewAssetAction(AssetActionKind.Payment, getSelectedAsset)
  }

  def onNewTransfer(event: ActionEvent): Unit = {
    actor ! OnNewAssetAction(AssetActionKind.Transfer, getSelectedAsset)
  }

  def onNewRefund(event: ActionEvent): Unit = {
    actor ! OnNewAssetAction(AssetActionKind.Refund, getSelectedAsset)
  }

  def onOptions(event: ActionEvent): Unit = {
    actor ! OnOptions
  }

  def onTest(event: ActionEvent): Unit = {
    actor ! OnTest(event.getSource.asInstanceOf[MenuItem].getText.substring(5).toInt)
  }

  def onFundGraph(event: ActionEvent): Unit = {
    actor ! OnFundGraph
  }

  def onCleanupDataStore(event: ActionEvent = null): Unit = {
    actor ! OnCleanupDataStore
  }

  def onNetAssetValueHistory(event: ActionEvent): Unit = {
    actor ! OnNetAssetValueHistory(getSelectedAsset.map(_.fundId))
  }

  def onUpToDateAssets(event: ActionEvent): Unit = {
    actor ! OnUpToDateAssets(event.getSource.asInstanceOf[CheckMenuItem].isSelected)
  }

  private def getState: SimpleObjectProperty[State] = {
    assetsTable.getUserData.asInstanceOf[SimpleObjectProperty[State]]
  }

  /**
   * Creates a new Asset table view row.
   *
   * Binds menu context to edit asset scheme/fund.
   */
  private def newAssetRow(): TableRow[AssetDetails] = {
    val row = new TableRow[AssetDetails]()

    // See: https://www.marshall.edu/genomicjava/2013/12/30/javafx-tableviews-with-contextmenus/
    val menu = new ContextMenu()
    // Note: Image(url, requestedWidth, requestedHeight, preserveRatio, smooth, backgroundLoading)
    val editScheme = new MenuItem(Strings.editScheme,
      new ImageView(new Image("/images/fugue-icons/tables.png", 0.0, 0.0, true, false, false)))
    editScheme.setOnAction { (event: ActionEvent) =>
      Option(row.getItem).foreach { details =>
        actor ! OnEditSchemes(Some(details.asset.schemeId))
      }
    }
    val editFund = new MenuItem(Strings.editFund,
      new ImageView(new Image("/images/fugue-icons/table.png", 0.0, 0.0, true, false, false)))
    editFund.setOnAction { (event: ActionEvent) =>
      Option(row.getItem).foreach { details =>
        actor ! OnEditFunds(Some(details.asset.fundId))
      }
    }

    val newPayment = new MenuItem(Strings.newPayment,
      new ImageView(new Image("/images/fugue-icons/table-import.png", 0.0, 0.0, true, false, false)))
    newPayment.setOnAction { (event: ActionEvent) =>
      Option(row.getItem).foreach { details =>
        actor ! OnNewAssetAction(AssetActionKind.Payment, Some(details.asset))
      }
    }
    val newArbitrage = new MenuItem(Strings.newTransfer,
      new ImageView(new Image("/images/fugue-icons/tables-relation.png", 0.0, 0.0, true, false, false)))
    newArbitrage.setOnAction { (event: ActionEvent) =>
      Option(row.getItem).foreach { details =>
        actor ! OnNewAssetAction(AssetActionKind.Transfer, Some(details.asset))
      }
    }
    val newRefund = new MenuItem(Strings.newRefund,
      new ImageView(new Image("/images/fugue-icons/table-export.png", 0.0, 0.0, true, false, false)))
    newRefund.setOnAction { (event: ActionEvent) =>
      Option(row.getItem).foreach { details =>
        actor ! OnNewAssetAction(AssetActionKind.Refund, Some(details.asset))
      }
    }

    val navHistory = new MenuItem(NetAssetValueHistoryController.title,
      new ImageView(new Image("/images/fugue-icons/chart-up.png", 0.0, 0.0, true, false, false)))
    navHistory.setOnAction { (event: ActionEvent) =>
      Option(row.getItem).foreach { details =>
        actor ! OnNetAssetValueHistory(Some(details.asset.fundId))
      }
    }

    menu.getItems.addAll(editScheme, editFund, new SeparatorMenuItem(),
      newPayment, newArbitrage, newRefund, new SeparatorMenuItem(),
      navHistory)

    row.contextMenuProperty().bind {
      Bindings.when(Bindings.isNotNull(row.itemProperty))
        .`then`(menu)
        .otherwise(null:ContextMenu)
    }

    row
  }

  private def getSelectedAsset: Option[Savings.Asset] =
    Option(assetsTable.getSelectionModel.getSelectedItem).map(_.asset)

  /** Gets (computes) given asset details. */
  private def getAssetDetails(asset: Savings.Asset): AssetDetails = {
    val state = getState.get()
    val savings = state.savingsUpd

    val grossGain = state.assetsValue.get(asset.fundId).map { assetValue =>
      asset.amount(assetValue.value) - asset.investedAmount
    }

    // Note: it is expected that we have an asset because there is an invested
    // amount. So there is no need to try to prevent division by 0.
    AssetDetails(
      asset = asset,
      scheme = savings.getScheme(asset.schemeId),
      fund = savings.getFund(asset.fundId),
      date = state.assetsValue.get(asset.fundId).map(_.date),
      nav = state.assetsValue.get(asset.fundId).map(_.value),
      grossAmount = state.assetsValue.get(asset.fundId).map { assetValue =>
        asset.amount(assetValue.value)
      },
      grossGain = grossGain,
      grossGainPct = grossGain.map(v => scalePercents((v * 100) / asset.investedAmount))
    )
  }

  /** Copy asset details to clipboard. */
  private def copyAssetDetailsToClipboard(details: AssetDetails): Unit = {
    val text = assetFields.values.map { field =>
      s"${field.detailsLabel} ${field.format(details, true)}"
    }.mkString("", "\n", "\n")

    val content = new ClipboardContent()
    content.putString(text)
    clipboard.setContent(content)
  }

  object ControllerActor {
    def props(state: State) = Props(new ControllerActor(state))
  }

  class ControllerActor(state0: State) extends Actor {

    applyState(state0, updateAssetsValue = true)

    override def receive: Receive = receive(state0)

    def receive(state: State): Receive = {
      case Refresh           => refresh(state)
      case OnFileNew         => onFileNew(state)
      case OnFileOpen        => onFileOpen(state)
      case OnFileClose       => onFileClose(state)
      case OnFileSave        => onFileSave(state)
      case OnExit            => onExit(state)
      case OnEditUndo        => onEditUndo(state)
      case OnEditSchemes(id) => onEditSchemes(state, id.map(state.savingsUpd.getScheme))
      case OnEditFunds(id)   => onEditFunds(state, id.map(state.savingsUpd.getFund))
      case OnNewAssetAction(kind, asset) => onNewAssetAction(state, kind, asset)
      case OnOptions         => onOptions(state)
      case OnTest(n)         => onTest(state, n)
      case OnFundGraph       => onFundGraph(state)
      case OnCleanupDataStore => onCleanupDataStore(state)
      case OnNetAssetValueHistory(fundId) => onNetAssetValueHistory(state, fundId)
      case OnUpToDateAssets(set) => onUpToDateAssets(state, set)
    }

    def processEvents(state: State, events: List[Savings.Event], updateAssetsValue: Boolean = false): Unit = {
      // Time to delete Net asset value history upon deleting fund
      events.collect {
        case Savings.DeleteFund(fundId) => DataStore.AssetHistory.deleteValues(fundId)
      }
      val newEvents = state.eventsUpd ::: events
      val newSavings = state.savingsUpd.processEvents(events)
      val newAssetsValue = if (updateAssetsValue) {
        newSavings.assets.map(_.fundId).distinct.flatMap { fundId =>
          val nav = Awaits.readDataStoreNAV(Some(state.window), fundId, LocalDate.now).getOrElse(None)
          nav.map(fundId -> _)
        }.toMap
      } else state.assetsValue
      val newState = state.copy(eventsUpd = newEvents, savingsUpd = newSavings, assetsValue = newAssetsValue)
      val dirty = newState.hasPendingChanges

      // First update state associated to assets table: takes care of
      // schemes/funds updated names if any.
      getState.set(newState)
      // Then update table content: takes care of added/removed entries
      import scala.collection.JavaConversions._
      val assets =
        if (!newState.viewUpToDateAssets) newSavings.assets
        else newSavings.computeAssets(LocalDate.now).assets
      val assetsDetails = assets.map(getAssetDetails)
      val sortedAssetsDetails = new SortedList(FXCollections.observableList(assetsDetails))
      sortedAssetsDetails.comparatorProperty.bind(assetsTable.comparatorProperty)
      assetsTable.setItems(sortedAssetsDetails)

      fileCloseMenu.setDisable(DataStore.dbOpened.isEmpty)
      fileSaveMenu.setDisable(!dirty)
      editUndoMenu.setDisable(!dirty)
      viewNetAssetValueHistoryMenu.setDisable(newState.savingsUpd.funds.isEmpty)

      val title = DataStore.dbOpened.map { name =>
        s"[$name${if (dirty) " *" else ""}] - "
      }.getOrElse(if (dirty) " * - " else "") + epsa.Main.name
      newState.stage.setTitle(title)

      context.become(receive(newState))
    }

    def refresh(state: State): Unit = {
      applyState(state, updateAssetsValue = true)
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
        applyState(newState)
      }
    }

    def onFileSave(state: State): Unit = {
      if (save(state)) {
        // Update state
        val newState = state.copy(savingsInit = state.savingsUpd, eventsUpd = Nil)

        applyState(newState)
      }
      // else: either it was cancelled by user, or it failed (and user was
      // notified).
    }

    def onExit(state: State): Unit = {
      if (checkPendingChanges(state)) {
        persistView(state)

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
        applyState(state.reset)
      }
    }

    def onEditSchemes(state: State, edit: Option[Savings.Scheme]): Unit = {
      val dialog = EditSchemesController.buildDialog(state.savingsUpd, edit)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.initOwner(state.window)
      dialog.setResizable(true)
      val events = dialog.showAndWait().orElse(Nil)
      processEvents(state, events)
    }

    def onEditFunds(state: State, edit: Option[Savings.Fund]): Unit = {
      val dialog = EditFundsController.buildDialog(state.savingsUpd, edit)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.initOwner(state.window)
      dialog.setResizable(true)
      val events = dialog.showAndWait().orElse(Nil)
      processEvents(state, events)
    }

    def onNewAssetAction(state: State, kind: AssetActionKind.Value, asset: Option[Savings.Asset]): Unit = {
      val dialog = NewAssetActionController.buildDialog(MainController.this, state.savingsUpd, kind, asset)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.initOwner(state.window)
      dialog.setResizable(true)
      val event = dialog.showAndWait().orElse(None)
      processEvents(state, event.toList)
    }

    def onOptions(state: State): Unit = {
      val dialog = OptionsController.buildDialog()
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.initOwner(state.window)
      dialog.setResizable(true)
      val (reload, needRestart) = dialog.showAndWait().orElse((false, false))
      if (reload) {
        // Reset I18N cache to apply any language change
        I18N.reset()
        // Persist now to restore it when rebuilding the stage
        persistView(state)
        context.stop(self)
        MainController.build(state, needRestart, applicationStart = false)
      }
    }

    def onTest(state: State, n: Int): Unit = {
      val savings = state.savingsUpd

      def getName(label: String, n: Int, values: Set[String]): String = {
        val name = s"$label $n"
        if (!values.contains(name.toLowerCase)) name
        else getName(label, n + 1, values)
      }

      def getSchemeName(events: Savings.Event*): String =
        getName("Scheme", 1, savings.processEvents(events:_*).schemes.map(_.name.toLowerCase).toSet)

      def getFundName(events: Savings.Event*): String =
        getName("Fund", 1, savings.processEvents(events:_*).funds.map(_.name.toLowerCase).toSet)

      n match {
        case 1 =>
          val s1 = savings.createSchemeEvent(getSchemeName())
          val s2 = savings.createSchemeEvent(getSchemeName(s1))
          val f1 = savings.createFundEvent(getFundName())
          val f2 = savings.createFundEvent(getFundName(f1))
          // Mixes many actions that ultimately gives:
          //   s1f1: 5 now
          //   s1f2: 10 now
          //   s1f2: 15 (+1 month)
          //   s2f2: 20 now
          // Purposely performs 2 last payments: last action leaves 2 assets which
          // shall be merged when computed now.
          processEvents(state, List(s1, s2, f1, f2,
            Savings.AssociateFund(s1.schemeId, f1.fundId),
            Savings.AssociateFund(s1.schemeId, f2.fundId),
            Savings.AssociateFund(s2.schemeId, f2.fundId),
            Savings.MakePayment(LocalDate.now.minusMonths(24),
              Savings.AssetPart(s1.schemeId, f1.fundId, Some(LocalDate.now.minusMonths(12)), 10.0, 1.0)),
            Savings.MakePayment(LocalDate.now.minusMonths(24),
              Savings.AssetPart(s1.schemeId, f1.fundId, Some(LocalDate.now.minusMonths(12)), 10.0, 2.0)),
            Savings.MakePayment(LocalDate.now.minusMonths(24),
              Savings.AssetPart(s1.schemeId, f2.fundId, Some(LocalDate.now.minusMonths(12)), 5.0, 1.0)),
            Savings.MakePayment(LocalDate.now.minusMonths(24),
              Savings.AssetPart(s1.schemeId, f1.fundId, Some(LocalDate.now.minusMonths(1)), 15.0, 2.0)),
            Savings.MakePayment(LocalDate.now.minusMonths(24),
              Savings.AssetPart(s1.schemeId, f2.fundId, Some(LocalDate.now.plusMonths(12)), 15.0, 1.0)),
            Savings.MakeRefund(LocalDate.now.minusMonths(12),
              Savings.AssetPart(s1.schemeId, f1.fundId, Some(LocalDate.now.minusMonths(1)), 10.0, 2.0)),
            Savings.MakeRefund(LocalDate.now.minusMonths(1),
              Savings.AssetPart(s1.schemeId, f1.fundId, None, 10.0, 2.5)),
            Savings.MakeTransfer(LocalDate.now.minusMonths(1),
              Savings.AssetPart(s1.schemeId, f1.fundId, None, 10.0, 0.5), Savings.AssetPart(s1.schemeId, f2.fundId, None, 5.0, 1.0)),
            Savings.MakePayment(LocalDate.now.minusMonths(1),
              Savings.AssetPart(s2.schemeId, f2.fundId, Some(LocalDate.now.minusMonths(1)), 10.0, 1.0)),
            Savings.MakePayment(LocalDate.now.minusMonths(1),
              Savings.AssetPart(s2.schemeId, f2.fundId, Some(LocalDate.now), 10.0, 1.0))
          ))

        case 2 =>
          val s1 = savings.createSchemeEvent(getSchemeName())
          val f1 = savings.createFundEvent(getFundName())
          // Resulting asset shall have its availability date reseted due to the second payment
          processEvents(state, List(s1, f1,
            Savings.AssociateFund(s1.schemeId, f1.fundId),
            Savings.MakePayment(LocalDate.now.minusMonths(24),
              Savings.AssetPart(s1.schemeId, f1.fundId, Some(LocalDate.now.minusMonths(12)), 5.0, 1.0)),
            Savings.MakePayment(LocalDate.now.minusMonths(12),
              Savings.AssetPart(s1.schemeId, f1.fundId, None, 5.0, 1.0))
          ))

        case 3 =>
          val s1 = savings.createSchemeEvent(getSchemeName())
          val f1 = savings.createFundEvent(getFundName())
          val f2 = savings.createFundEvent(getFundName(f1))
          // Originating asset shall keep its availability date (even if available for the transfer).
          processEvents(state, List(s1, f1, f2,
            Savings.AssociateFund(s1.schemeId, f1.fundId),
            Savings.AssociateFund(s1.schemeId, f2.fundId),
            Savings.MakePayment(LocalDate.now.minusMonths(24),
              Savings.AssetPart(s1.schemeId, f1.fundId, Some(LocalDate.now.minusMonths(12)), 10.0, 1.0)),
            Savings.MakeTransfer(LocalDate.now.minusMonths(12),
              Savings.AssetPart(s1.schemeId, f1.fundId, None, 5.0, 1.0), Savings.AssetPart(s1.schemeId, f2.fundId, None, 5.0, 1.0))
          ))
      }
    }

    def onFundGraph(state: State): Unit = {
      import Preference._

      val stage = new Stage()

      val fundPath = Preference.from("fund.path", null:Path)

      val fileChooser = new FileChooser()
      fileChooser.setTitle("Open Investment Fund File")
      fileChooser.getExtensionFilters.addAll(
        new FileChooser.ExtensionFilter("Excel Files", "*.xls", "*.xlsx"),
        new FileChooser.ExtensionFilter("All Files", "*.*")
      )
      fundPath.option.foreach { path =>
        FileChoosers.setInitialPath(fileChooser, path.toFile)
      }
      val selectedFile = fileChooser.showOpenDialog(stage)
      Option(selectedFile).flatMap { file =>
        EsaliaInvestmentFundProber.probe(file.toPath)
      } match {
        case Some(hist) =>
          // Save path in preferences
          fundPath() = selectedFile.toPath
          // Then build and display chart
          val chartHandler = new ChartHandler(
            fundName = hist.name.orNull,
            fundValues = hist.values
          )
          val chartPane = chartHandler.chartPane
          chartPane.setPrefSize(640, 480)
          val scene = new Scene(chartPane)
          stage.setScene(scene)
          stage.show()

        case None =>
      }
    }

    def onCleanupDataStore(state: State): Unit = {
      Awaits.cleanupDataStore(Some(state.window), state.savingsUpd.funds.map(_.id))
      refresh(state)
    }

    def onNetAssetValueHistory(state: State, fundId: Option[UUID]): Unit = {
      val dialog = NetAssetValueHistoryController.buildStage(MainController.this, state.savingsUpd, fundId, state.window)
      // Notes:
      // Don't set as modal, since we wish to display the window while still
      // interacting with the main stage.
      // Don't set owner, otherwise the new windows remains in front of its
      // owner.
      dialog.initModality(Modality.NONE)
      dialog.setResizable(true)
      dialog.show()
    }

    def onUpToDateAssets(state: State, set: Boolean): Unit = {
      applyState(state.copy(viewUpToDateAssets = set))
    }

    private def applyState(state: State, updateAssetsValue: Boolean = false): Unit = {
      // Cheap trick to fill fields with Savings data
      processEvents(state, Nil, updateAssetsValue)
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
        alert.initOwner(state.window)
        alert.setHeaderText(Strings.pendingChanges)

        // Filter action on "Save" button to trigger saving and check result:
        // If saving failed (user was notified), consume event to get back to
        // confirmation dialog.
        val buttonSave = alert.getDialogPane.lookupButton(buttonSaveType)
        // Note: Image(url, requestedWidth, requestedHeight, preserveRatio, smooth, backgroundLoading)
        val image = new Image("/images/fugue-icons/disk.png", 0.0, 0.0, true, false, false)
        buttonSave.asInstanceOf[Button].setGraphic(new ImageView(image))
        buttonSave.addEventFilter(ActionEvent.ACTION, { (event: ActionEvent) =>
          if (!save(state, Some(Stages.getStage(alert)))) event.consume()
        })

        val r = alert.showAndWait()
        r.contains(ButtonType.OK) || r.contains(buttonSaveType)
      }
      else true

    private def open(state: State): Unit = {
      val owner = Some(state.stage)

      def read() = Awaits.readDataStoreEvents(owner) match {
        case Success(events) =>
          val savingsInit = Savings().processEvents(events:_*)
          val newState = State(
            stage = state.stage,
            savingsInit = savingsInit,
            savingsUpd = savingsInit
          )
          applyState(newState)
          // Cleanup datastore if necessary
          self ! OnCleanupDataStore

        case _ =>
      }

      Awaits.openDataStore(owner, change = true, save = false) match {
        case Some(Success(())) => read()
        case _                 =>
      }
    }

    private def save(state: State, owner0: Option[Window] = None): Boolean = {
      // Note: make sure to not both lock JavaFX (e.g. waiting for a Future) and
      // try to use it (e.g. Dialog to show upon issue).
      // For simplicity, we waits for result and display issue after receiving
      // it.
      val owner = owner0.orElse(Some(state.window))

      def save() =
        Awaits.saveDataStoreChanges(owner, state.eventsUpd).isSuccess

      DataStore.dbOpened match {
        case Some(name) => save()
        case None       =>
          // Data store not opened yet: open then save
          Awaits.openDataStore(owner, change = true, save = true) match {
            case Some(Success(())) => save()
            case _                 => false
          }
      }
    }

  }

}

object MainController {

  case class State(
    stage: Stage,
    savingsInit: Savings = Savings(),
    eventsUpd: List[Savings.Event] = Nil,
    savingsUpd: Savings = Savings(),
    viewUpToDateAssets: Boolean = true,
    assetsValue: Map[UUID, Savings.AssetValue] = Map.empty
  ) {

    lazy val window = stage.getScene.getWindow

    def hasPendingChanges: Boolean =
      eventsUpd.nonEmpty || DataStore.hasPendingChanges

    def reset: State =
      copy(eventsUpd = Nil, savingsUpd = savingsInit)

  }

  case object Refresh

  case object OnFileNew

  case object OnFileOpen

  case object OnFileClose

  case object OnFileSave

  case object OnExit

  case object OnEditUndo

  case class OnEditSchemes(schemeId: Option[UUID])

  case class OnEditFunds(fundId: Option[UUID])

  case class OnNewAssetAction(kind: AssetActionKind.Value, asset: Option[Savings.Asset])

  case object OnOptions

  case class OnTest(n: Int)

  case object OnFundGraph

  case object OnCleanupDataStore

  case class OnNetAssetValueHistory(fundId: Option[UUID])

  case class OnUpToDateAssets(set: Boolean)

  private val ASSET_KEY_SCHEME = "scheme"

  private val ASSET_KEY_FUND = "fund"

  private val ASSET_KEY_AVAILABILITY = "availability"

  private val ASSET_KEY_UNITS = "units"

  private val ASSET_KEY_VWAP = "vwap"

  private val ASSET_KEY_DATE = "date"

  private val ASSET_KEY_NAV = "nav"

  private val ASSET_KEY_INVESTED_AMOUNT = "investedAmount"

  private val ASSET_KEY_GROSS_AMOUNT = "grossAmount"

  private val ASSET_KEY_GROSS_GAIN = "grossGain"

  private val ASSET_KEY_GROSS_GAIN_PCT = "grossGainPct"

  case class AssetDetails(asset: Savings.Asset, scheme: Savings.Scheme, fund: Savings.Fund,
    date: Option[LocalDate], nav: Option[BigDecimal], grossAmount: Option[BigDecimal],
    grossGain: Option[BigDecimal], grossGainPct: Option[BigDecimal])
  {

    private val currency = epsa.Settings.currency()

    def formatAvailability(long: Boolean) = Form.formatAvailability(asset.availability, date = None, long)
    val formatVWAP = Form.formatAmount(asset.vwap, currency)
    val formatDate = date.map(_.toString).getOrElse(Strings.na)
    val formatNAV = nav.map(Form.formatAmount(_, currency)).getOrElse(Strings.na)
    val formatInvestedAmount = Form.formatAmount(asset.investedAmount, currency)
    val formatGrossAmount = grossAmount.map(Form.formatAmount(_, currency)).getOrElse(Strings.na)
    val formatGrossGain = grossGain.map(Form.formatAmount(_, currency)).getOrElse(Strings.na)
    val formatGrossGainPct = grossGainPct.map(Form.formatAmount(_, "%")).getOrElse(Strings.na)

  }

  /** Asset field settings. */
  trait AssetField[A] {
    /** Field name in table. */
    val tableLabel: String
    /** Field name in details pane. */
    val detailsLabel: String
    /** Field comment if any. */
    val comment: (AssetDetails) => Option[String] = { _ => None }
    /** How to format field value. */
    val format: (AssetDetails, Boolean) => String

    /** The details pane label (where value is displayed). */
    val detailsValue = new Label
    detailsValue.setMinWidth(Region.USE_PREF_SIZE)
    detailsValue.setMinHeight(Region.USE_PREF_SIZE)

    /** The table column. */
    val column: TableColumn[AssetDetails, A]

    def updateDetailsValue(assetDetailsOpt: Option[AssetDetails]): Unit = {
      detailsValue.setText(assetDetailsOpt.map(format(_, true)).orNull)
      // TODO: visual hint there is a comment
      detailsValue.setTooltip(assetDetailsOpt.flatMap(comment).map(new Tooltip(_)).orNull)
    }
  }

  /** Asset field with formatted text value to display. */
  case class AssetTextField(tableLabel: String, detailsLabel: String,
    format: (AssetDetails, Boolean) => String,
    override val comment: (AssetDetails) => Option[String] = { _ => None }
  ) extends AssetField[String] {
    val column = new TableColumn[AssetDetails, String](tableLabel)
    column.setCellValueFactory(Callback { data =>
      new SimpleStringProperty(format(data.getValue, false))
    })
  }

  /** Asset field with amount to display. */
  case class AssetAmountField(tableLabel: String, detailsLabel: String,
    format: (AssetDetails, Boolean) => String,
    value: (AssetDetails) => Option[BigDecimal],
    suffix: String
  ) extends AssetField[Option[BigDecimal]] {
    val column = new TableColumn[AssetDetails, Option[BigDecimal]](tableLabel)
    column.setCellValueFactory(Callback { data =>
      new SimpleObjectProperty(value(data.getValue))
    })
    column.setCellFactory(Callback { new AmountCell[AssetDetails](suffix, Strings.na) with ColoredAmount })

    override def updateDetailsValue(assetDetailsOpt: Option[AssetDetails]): Unit = {
      super.updateDetailsValue(assetDetailsOpt)
      assetDetailsOpt.flatMap(value).find(_ != 0) match {
        case Some(v) =>
          if (v > 0) JFXStyles.togglePositive(detailsValue)
          else JFXStyles.toggleNegative(detailsValue)

        case None =>
          JFXStyles.toggleNeutral(detailsValue)
      }
    }
  }

  object AssetField {

    // Asset fields.
    // Note: declare the fields through a def so that changing language applies
    // upon reloading view.
    // Order here is the one the fields will appear in the asset details pane
    // and table columns.
    def fields() = ListMap(
      ASSET_KEY_SCHEME          -> AssetTextField(Strings.scheme, Strings.schemeColon, AssetField.formatScheme, AssetField.schemeComent),
      ASSET_KEY_FUND            -> AssetTextField(Strings.fund, Strings.fundColon, AssetField.formatFund),
      ASSET_KEY_AVAILABILITY    -> AssetTextField(Strings.availability, Strings.availabilityColon, AssetField.formatAvailability),
      ASSET_KEY_UNITS           -> AssetTextField(Strings.units, Strings.unitsColon, AssetField.formatUnits),
      ASSET_KEY_VWAP            -> AssetTextField(Strings.vwap, Strings.vwapColon, AssetField.formatVWAP),
      ASSET_KEY_NAV             -> AssetTextField(Strings.nav, Strings.navColon, AssetField.formatNAV),
      ASSET_KEY_DATE            -> AssetTextField(Strings.date, Strings.dateColon, AssetField.formatDate),
      ASSET_KEY_INVESTED_AMOUNT -> AssetTextField(Strings.invested, Strings.investedAmountColon, AssetField.formatInvestedAmount),
      ASSET_KEY_GROSS_AMOUNT    -> AssetTextField(Strings.gross, Strings.grossAmountColon, AssetField.formatGrossAmount),
      ASSET_KEY_GROSS_GAIN      -> AssetAmountField(Strings.gross, Strings.grossGainColon, AssetField.formatGrossGain, AssetField.grossGain, epsa.Settings.currency()),
      ASSET_KEY_GROSS_GAIN_PCT  -> AssetAmountField(Strings.grossPct, Strings.grossGainPctColon, AssetField.formatGrossGainPct, AssetField.grossGainPct, "%")
    )

    def formatScheme(details: AssetDetails, long: Boolean) = details.scheme.name
    def schemeComent(details: AssetDetails) = details.scheme.comment
    def formatFund(details: AssetDetails, long: Boolean) = details.fund.name
    def formatAvailability(details: AssetDetails, long: Boolean) = details.formatAvailability(long)
    def formatUnits(details: AssetDetails, long: Boolean) = details.asset.units.toString
    def formatVWAP(details: AssetDetails, long: Boolean) = details.formatVWAP
    def formatDate(details: AssetDetails, long: Boolean) = details.formatDate
    def formatNAV(details: AssetDetails, long: Boolean) = details.formatNAV
    def formatInvestedAmount(details: AssetDetails, long: Boolean) = details.formatInvestedAmount
    def formatGrossAmount(details: AssetDetails, long: Boolean) = details.formatGrossAmount
    def formatGrossGain(details: AssetDetails, long: Boolean) = details.formatGrossGain
    def grossGain(details: AssetDetails) = details.grossGain
    def formatGrossGainPct(details: AssetDetails, long: Boolean) = details.formatGrossGainPct
    def grossGainPct(details: AssetDetails) = details.grossGainPct

  }

  def build(state: State, needRestart: Boolean = false, applicationStart: Boolean = false): Unit = {
    val stage = state.stage

    val loader = new FXMLLoader(getClass.getResource("/fxml/main.fxml"), I18N.getResources)
    val root = loader.load[Parent]()
    val controller = loader.getController[MainController]
    controller.initialize(state)

    if (Option(stage.getScene).isDefined) stage.hide()
    // Delegate closing request to controller
    stage.setOnCloseRequest(controller.onCloseRequest _)
    stage.setScene(new Scene(root))
    stage.getScene.getStylesheets.add(getClass.getResource("/css/main.css").toExternalForm)
    stage.show()

    // It is important to restore view after showing the stage, at least for
    // some settings (which may slightly change or not be fully applied):
    //   - stage position/size
    //   - SplitPane dividers position
    controller.restoreView(stage)

    Stages.trackMinimumDimensions(stage)

    if (needRestart) {
      Dialogs.information(
        owner = Some(state.window),
        title = None,
        headerText = Some(Strings.needRestart)
      )
    }

    if (applicationStart) controller.onCleanupDataStore()
  }

}
