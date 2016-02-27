package epsa.controllers

import akka.actor.{Actor, ActorRef, Props}
import epsa.I18N
import epsa.charts.ChartHandler
import epsa.model.Savings
import epsa.storage.DataStore
import epsa.tools.EsaliaInvestmentFundProber
import epsa.util.Awaits
import grizzled.slf4j.Logging
import java.nio.file.Path
import java.time.LocalDate
import java.util.{ResourceBundle, UUID}
import javafx.beans.binding.Bindings
import javafx.beans.property.SimpleObjectProperty
import javafx.collections.FXCollections
import javafx.collections.transformation.SortedList
import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene}
import javafx.scene.control._
import javafx.scene.image.{Image, ImageView}
import javafx.stage._
import scala.util.Success
import suiryc.scala.RichOption._
import suiryc.scala.concurrent.Callable
import suiryc.scala.javafx
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.event.EventHandler._
import suiryc.scala.javafx.scene.control.{Dialogs, TableViews}
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.util.Callback
import suiryc.scala.settings.Preference

// TODO - change menu for OS integration ? (e.g. Ubuntu)
// TODO - display base and current (to date) amounts in assets table
// TODO - display asset gain/loss (amount/percentage) in assets table
// TODO - display more details for selected asset (values history graph, ...)
// TODO - menu entries with latest datastore locations ?
// TODO - menu entry and dialog to display/edit events history ?
// TODO - when computing assets, order by scheme/fund/availability ?
// TODO - manage encryption of datastore ?
// TODO - manual/automatic way to check NAV history values without existing fund ?
class MainController extends Logging {

  import epsa.Main.prefs
  import MainController._
  import Preference._
  import Stages.StageLocation

  private val stageLocation = Preference.from("stage.main.location", null:StageLocation)

  private val splitPaneDividerPositions = Preference.from("stage.main.splitPane.dividerPositions", null:String)

  private val assetsColumnsPref = Preference.from("stage.main.assets.columns", null:String)

  @FXML
  protected var resources: ResourceBundle = _

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
  protected var schemeField: Label = _

  @FXML
  protected var fundField: Label = _

  @FXML
  protected var availabilityField: Label = _

  @FXML
  protected var amountField: Label = _

  @FXML
  protected var unitsField: Label = _

  @FXML
  protected var assetsTable: TableView[Savings.Asset] = _

  private var actor: ActorRef = _

  lazy private val columnScheme =
    new TableColumn[Savings.Asset, String](resources.getString("Scheme"))

  lazy private val columnFund =
    new TableColumn[Savings.Asset, String](resources.getString("Fund"))

  lazy private val columnAvailability =
    new TableColumn[Savings.Asset, Option[LocalDate]](resources.getString("Availability"))

  lazy private val columnAmount =
    new TableColumn[Savings.Asset, BigDecimal](resources.getString("Amount"))

  lazy private val columnUnits =
    new TableColumn[Savings.Asset, BigDecimal](resources.getString("Units"))

  lazy private val assetsColumns = List(
    "scheme"       -> columnScheme,
    "fund"         -> columnFund,
    "availability" -> columnAvailability,
    "amount"       -> columnAmount,
    "units"        -> columnUnits
  )

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
    columnAvailability.setCellValueFactory(Callback { data =>
      new SimpleObjectProperty(data.getValue.availability)
    })
    columnAvailability.setCellFactory(Callback { new AvailabilityTableCell[Savings.Asset] })
    columnAmount.setCellValueFactory(Callback { data =>
      new SimpleObjectProperty(data.getValue.amount)
    })
    columnAmount.setCellFactory(Callback { new AmountCell[Savings.Asset](epsa.Main.currency()) })
    columnUnits.setCellValueFactory(Callback { data =>
      new SimpleObjectProperty(data.getValue.units)
    })

    // Note: Asset gives scheme/fund UUID. Since State is immutable (and is
    // changed when applying events in controller) we must delegate scheme/fund
    // lookup to the controller.
    assetsTable.setRowFactory(Callback { newAssetRow() })

    // Show details of selected asset
    assetsTable.getSelectionModel.selectedItemProperty.listen { asset0 =>
      val savings = getAssetsSavings.get()
      val assetOpt = Option(asset0)
      schemeField.setText(assetOpt.map { asset =>
        savings.getScheme(asset.schemeId).name
      }.orNull)
      fundField.setText(assetOpt.map { asset =>
        savings.getFund(asset.fundId).name
      }.orNull)
      availabilityField.setText(assetOpt.map { asset =>
        Form.formatAvailability(asset.availability, date = None, long = true)
      }.orNull)
      amountField.setText(assetOpt.map { asset =>
        Form.formatAmount(asset.amount, epsa.Main.currency())
      }.orNull)
      unitsField.setText(assetOpt.map { asset =>
        asset.units.toString()
      }.orNull)
    }
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
    if (!javafx.isLinux) restoreDividerPositions()
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
    actor ! OnEditSchemes(Option(assetsTable.getSelectionModel.getSelectedItem).map(_.schemeId))
  }

  def onEditFunds(event: ActionEvent): Unit = {
    actor ! OnEditFunds(Option(assetsTable.getSelectionModel.getSelectedItem).map(_.fundId))
  }

  def onNewPayment(event: ActionEvent): Unit = {
    actor ! OnNewAssetAction(AssetActionKind.Payment, Option(assetsTable.getSelectionModel.getSelectedItem))
  }

  def onNewTransfer(event: ActionEvent): Unit = {
    actor ! OnNewAssetAction(AssetActionKind.Transfer, Option(assetsTable.getSelectionModel.getSelectedItem))
  }

  def onNewRefund(event: ActionEvent): Unit = {
    actor ! OnNewAssetAction(AssetActionKind.Refund, Option(assetsTable.getSelectionModel.getSelectedItem))
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

  def onNetAssetValueHistory(event: ActionEvent): Unit = {
    actor ! OnNetAssetValueHistory(Option(assetsTable.getSelectionModel.getSelectedItem).map(_.fundId))
  }

  def onUpToDateAssets(event: ActionEvent): Unit = {
    actor ! OnUpToDateAssets(event.getSource.asInstanceOf[CheckMenuItem].isSelected)
  }

  private def getAssetsSavings: SimpleObjectProperty[Savings] = {
    assetsTable.getUserData.asInstanceOf[SimpleObjectProperty[Savings]]
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
    // Note: Image(url, requestedWidth, requestedHeight, preserveRatio, smooth, backgroundLoading)
    val editScheme = new MenuItem(resources.getString("Edit scheme"),
      new ImageView(new Image("/images/fugue-icons/tables.png", 0.0, 0.0, true, false, false)))
    editScheme.setOnAction { (event: ActionEvent) =>
      Option(row.getItem).foreach { asset =>
        actor ! OnEditSchemes(Some(asset.schemeId))
      }
    }
    val editFund = new MenuItem(resources.getString("Edit fund"),
      new ImageView(new Image("/images/fugue-icons/table.png", 0.0, 0.0, true, false, false)))
    editFund.setOnAction { (event: ActionEvent) =>
      Option(row.getItem).foreach { asset =>
        actor ! OnEditFunds(Some(asset.fundId))
      }
    }

    val newPayment = new MenuItem(resources.getString("New payment"),
      new ImageView(new Image("/images/fugue-icons/table-import.png", 0.0, 0.0, true, false, false)))
    newPayment.setOnAction { (event: ActionEvent) =>
      Option(row.getItem).foreach { asset =>
        actor ! OnNewAssetAction(AssetActionKind.Payment, Some(asset))
      }
    }
    val newArbitrage = new MenuItem(resources.getString("New transfer"),
      new ImageView(new Image("/images/fugue-icons/tables-relation.png", 0.0, 0.0, true, false, false)))
    newArbitrage.setOnAction { (event: ActionEvent) =>
      Option(row.getItem).foreach { asset =>
        actor ! OnNewAssetAction(AssetActionKind.Transfer, Some(asset))
      }
    }
    val newRefund = new MenuItem(resources.getString("New refund"),
      new ImageView(new Image("/images/fugue-icons/table-export.png", 0.0, 0.0, true, false, false)))
    newRefund.setOnAction { (event: ActionEvent) =>
      Option(row.getItem).foreach { asset =>
        actor ! OnNewAssetAction(AssetActionKind.Refund, Some(asset))
      }
    }

    val navHistory = new MenuItem(resources.getString("Net asset value history"),
      new ImageView(new Image("/images/fugue-icons/chart-up.png", 0.0, 0.0, true, false, false)))
    navHistory.setOnAction { (event: ActionEvent) =>
      Option(row.getItem).foreach { asset =>
        actor ! OnNetAssetValueHistory(Some(asset.fundId))
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

  object ControllerActor {
    def props(state: State) = Props(new ControllerActor(state))
  }

  class ControllerActor(state0: State) extends Actor {

    applyState(state0)

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
      case OnNetAssetValueHistory(fundId) => onNetAssetValueHistory(state, fundId)
      case OnUpToDateAssets(set) => onUpToDateAssets(state, set)
    }

    def processEvents(state: State, events: List[Savings.Event]): Unit = {
      // Time to delete Net asset value history upon deleting fund
      events.collect {
        case Savings.DeleteFund(fundId) => DataStore.AssetHistory.deleteValues(fundId)
      }
      val newEvents = state.eventsUpd ::: events
      val newSavings = state.savingsUpd.processEvents(events)
      val newState = state.copy(eventsUpd = newEvents, savingsUpd = newSavings)
      val dirty = newState.hasPendingChanges

      // First update savings associated to assets table: takes care of
      // schemes/funds updated names if any.
      getAssetsSavings.set(newSavings)
      // Then update table content: takes care of added/removed entries
      import scala.collection.JavaConversions._
      val assets =
        if (!newState.viewUpToDateAssets) newSavings.assets
        else newSavings.computeAssets(LocalDate.now).assets
      val sortedAssets = new SortedList(FXCollections.observableList(assets))
      sortedAssets.comparatorProperty.bind(assetsTable.comparatorProperty)
      assetsTable.setItems(sortedAssets)

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
      applyState(state)
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
        headerText = Some(resources.getString("confirmation.irreversible-action")),
        contentText = Some(resources.getString("Undo all pending changes"))
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
      val dialog = NewAssetActionController.buildDialog(state.savingsUpd, kind, asset)
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
        // Persist now to restore it when rebuilding the stage
        persistView(state)
        context.stop(self)
        MainController.build(state, needRestart)
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
            Savings.MakePayment(LocalDate.now.minusMonths(24), Savings.Asset(s1.schemeId, f1.fundId, Some(LocalDate.now.minusMonths(12)), 10.0, 10.0)),
            Savings.MakePayment(LocalDate.now.minusMonths(24), Savings.Asset(s1.schemeId, f1.fundId, Some(LocalDate.now.minusMonths(12)), 20.0, 10.0)),
            Savings.MakePayment(LocalDate.now.minusMonths(24), Savings.Asset(s1.schemeId, f2.fundId, Some(LocalDate.now.minusMonths(12)), 5.0, 5.0)),
            Savings.MakePayment(LocalDate.now.minusMonths(24), Savings.Asset(s1.schemeId, f1.fundId, Some(LocalDate.now.minusMonths(1)), 25.0, 15.0)),
            Savings.MakePayment(LocalDate.now.minusMonths(24), Savings.Asset(s1.schemeId, f2.fundId, Some(LocalDate.now.plusMonths(12)), 15.0, 15.0)),
            Savings.MakeRefund(LocalDate.now.minusMonths(12), Savings.Asset(s1.schemeId, f1.fundId, Some(LocalDate.now.minusMonths(1)), 20.0, 10.0)),
            Savings.MakeRefund(LocalDate.now.minusMonths(1), Savings.Asset(s1.schemeId, f1.fundId, None, 25.0, 10.0)),
            Savings.MakeTransfer(LocalDate.now.minusMonths(1), Savings.Asset(s1.schemeId, f1.fundId, None, 5.0, 10.0), Savings.Asset(s1.schemeId, f2.fundId, None, 5.0, 5.0)),
            Savings.MakePayment(LocalDate.now.minusMonths(1), Savings.Asset(s2.schemeId, f2.fundId, Some(LocalDate.now.minusMonths(1)), 10.0, 10.0)),
            Savings.MakePayment(LocalDate.now.minusMonths(1), Savings.Asset(s2.schemeId, f2.fundId, Some(LocalDate.now), 10.0, 10.0))
          ))

        case 2 =>
          val s1 = savings.createSchemeEvent(getSchemeName())
          val f1 = savings.createFundEvent(getFundName())
          // Resulting asset shall have its availability date reseted due to the second payment
          processEvents(state, List(s1, f1,
            Savings.AssociateFund(s1.schemeId, f1.fundId),
            Savings.MakePayment(LocalDate.now.minusMonths(24), Savings.Asset(s1.schemeId, f1.fundId, Some(LocalDate.now.minusMonths(12)), 5.0, 5.0)),
            Savings.MakePayment(LocalDate.now.minusMonths(12), Savings.Asset(s1.schemeId, f1.fundId, None, 5.0, 5.0))
          ))

        case 3 =>
          val s1 = savings.createSchemeEvent(getSchemeName())
          val f1 = savings.createFundEvent(getFundName())
          val f2 = savings.createFundEvent(getFundName(f1))
          // Originating asset shall keep its availability date (even if available for the transfer).
          processEvents(state, List(s1, f1, f2,
            Savings.AssociateFund(s1.schemeId, f1.fundId),
            Savings.AssociateFund(s1.schemeId, f2.fundId),
            Savings.MakePayment(LocalDate.now.minusMonths(24), Savings.Asset(s1.schemeId, f1.fundId, Some(LocalDate.now.minusMonths(12)), 10.0, 10.0)),
            Savings.MakeTransfer(LocalDate.now.minusMonths(12), Savings.Asset(s1.schemeId, f1.fundId, None, 5.0, 5.0), Savings.Asset(s1.schemeId, f2.fundId, None, 5.0, 5.0))
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
        fileChooser.setInitialDirectory(path.getParent.toFile)
        fileChooser.setInitialFileName(path.toFile.getName)
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

    def onNetAssetValueHistory(state: State, fundId: Option[UUID]): Unit = {
      val dialog = NetAssetValueHistoryController.buildStage(MainController.this, state, fundId)
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

    private def applyState(state: State): Unit = {
      // Cheap trick to fill fields with Savings data
      processEvents(state, Nil)
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
        alert.setHeaderText(resources.getString("confirmation.pending-changes"))

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
    viewUpToDateAssets: Boolean = true
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

  case class OnNetAssetValueHistory(fundId: Option[UUID])

  case class OnUpToDateAssets(set: Boolean)

  def build(state: State, needRestart: Boolean = false): Unit = {
    val stage = state.stage

    val resources = I18N.getResources
    val loader = new FXMLLoader(getClass.getResource("/fxml/main.fxml"), resources)
    val root = loader.load[Parent]()
    val controller = loader.getController[MainController]
    controller.initialize(state)

    if (Option(stage.getScene).isDefined) stage.hide()
    // Delegate closing request to controller
    stage.setOnCloseRequest(controller.onCloseRequest _)
    stage.setScene(new Scene(root))
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
        headerText = Some(resources.getString("information.need-restart"))
      )
    }
  }

}
