package epsa.controllers

import akka.actor.{Actor, ActorRef, Props}
import epsa.I18N
import epsa.charts.ChartHandler
import epsa.model.Savings
import epsa.storage.DataStore
import epsa.tools.EsaliaInvestmentFundProber
import epsa.util.Awaits
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
import javafx.stage._
import scala.util.Success
import suiryc.scala.RichOption._
import suiryc.scala.concurrent.Callable
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.event.EventHandler._
import suiryc.scala.javafx.scene.control.TableViews
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.util.Callback
import suiryc.scala.settings.Preference

// TODO - menu key shortcuts ?
// TODO - change menu for OS integration ? (e.g. Ubuntu)
// TODO - save fund value history in datastore
// TODO - display base and current (to date) amounts in assets table
// TODO - display asset gain/loss (amount/percentage) in assets table
// TODO - display more details for selected asset (values history graph, ...)
// TODO - use SplitPane to separate details and assets table; persist details width ?
// TODO - menu entries with latest datastore locations ?
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
  protected var fileCloseMenu: MenuItem = _

  @FXML
  protected var fileSaveMenu: MenuItem = _

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
    columnAvailability.setCellValueFactory(Callback { data =>
      new SimpleObjectProperty(data.getValue.availability)
    })
    columnAvailability.setCellFactory(Callback { new AvailabilityCell[Savings.Asset] })
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
        Form.formatAvailability(asset.availability)
      }.orNull)
      amountField.setText(assetOpt.map { asset =>
        Form.formatAmount(asset.amount)
      }.orNull)
      unitsField.setText(assetOpt.map { asset =>
        asset.units.toString()
      }.orNull)
    }
  }

  def onCloseRequest(event: WindowEvent): Unit = {
    actor ! OnExit
    // Note: consume the event, the actor is responsible for shutting down
    event.consume()
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

    applyState(state0)

    override def receive: Receive = receive(state0)

    def receive(state: State): Receive = {
      case OnFileOpen       => onFileOpen(state)
      case OnFileClose      => onFileClose(state)
      case OnFileSave       => onFileSave(state)
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
      val dirty = newState.eventsUpd.nonEmpty

      // First update savings associated to assets table: takes care of
      // schemes/funds updated names if any.
      getAssetsSavings.set(newSavings)
      // Then update table content: takes care of added/removed entries
      import scala.collection.JavaConversions._
      val sortedAssets = new SortedList(FXCollections.observableList(newSavings.assets))
      sortedAssets.comparatorProperty.bind(assetsTable.comparatorProperty)
      assetsTable.setItems(sortedAssets)

      fileCloseMenu.setDisable(newState.dbOpened.isEmpty)
      fileSaveMenu.setDisable(!dirty)

      val title = newState.dbOpened.map { name =>
        s"[$name${if (dirty) " *" else ""}] - "
      }.getOrElse(if (dirty) " * - " else "") + epsa.Main.name
      newState.stage.setTitle(title)

      context.become(receive(newState))
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
      save(state).foreach { name =>
        // Update state
        val newState = state.copy(savingsInit = state.savingsUpd, eventsUpd = Nil, dbOpened = Some(name))

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
        case Some(fund) =>
          // Save path in preferences
          fundPath() = selectedFile.toPath
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

    private def applyState(state: State): Unit = {
      // Cheap trick to fill fields with Savings data
      processEvents(state, Nil)
    }

    /** Persists view (stage location, ...). */
    private def persistView(state: State): Unit = {
      // Persist stage location
      // Note: if iconified, resets it
      stageLocation() = Stages.getLocation(state.stage).orNull

      // Persist assets table columns order and width
      assetsColumnsPref() = TableViews.getColumnsView(assetsTable, assetsColumns)
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
      if (state.eventsUpd.nonEmpty) {
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
        buttonSave.addEventFilter(ActionEvent.ACTION, { (event: ActionEvent) =>
          if (save(state, Some(Stages.getStage(alert))).isEmpty) event.consume()
        })

        val r = alert.showAndWait()
        r.contains(ButtonType.OK) || r.contains(buttonSaveType)
      }
      else true

    private def open(state: State): Unit = {
      val owner = Some(state.stage)

      def read(name: String) = Awaits.readDataStoreEvents(owner) match {
        case Success(events) =>
          val savingsInit = Savings.processEvents(new Savings(), events:_*)
          val newState = State(
            stage = state.stage,
            savingsInit = savingsInit,
            savingsUpd = savingsInit,
            dbOpened = Some(name)
          )
          applyState(newState)

        case _ =>
      }

      Awaits.openDataStore(owner, change = true, save = false) match {
        case Some(Success(name)) => read(name)
        case _                   =>
      }
    }

    private def save(state: State, owner0: Option[Window] = None): Option[String] = {
      // Note: make sure to not both lock JavaFX (e.g. waiting for a Future) and
      // try to use it (e.g. Dialog to show upon issue).
      // For simplicity, we waits for result and display issue after receiving
      // it.
      val owner = owner0.orElse(Some(state.window))

      def save(name: String) =
        Awaits.writeDataStoreEvents(owner, state.eventsUpd).toOption.map(_ => name)

      state.dbOpened match {
        case Some(name) =>
          save(name)

        case None =>
          // Data store not opened yet: open then save
          Awaits.openDataStore(owner, change = true, save = true) match {
            case Some(Success(name)) =>
              // Note: if we succeed, caller will change the state.
              // If we fail, caller will do nothing, but at least we can set
              // the db as opened. (provided caller does not change state)
              applyState(state.copy(dbOpened = Some(name)))
              save(name)

            case _ =>
              None
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
    dbOpened: Option[String] = None
  ) {
    lazy val window = stage.getScene.getWindow
  }

  case object OnFileOpen

  case object OnFileClose

  case object OnFileSave

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
