package epsa.controllers

import epsa.{I18N, Main, Settings}
import epsa.I18N.Strings
import epsa.storage.DataStore
import epsa.model.Savings
import epsa.util.{Awaits, JFXStyles}
import java.time.Month
import javafx.collections.FXCollections
import javafx.event.{ActionEvent, Event}
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.Node
import javafx.scene.control._
import javafx.scene.image.ImageView
import javafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import javafx.stage.{Stage, Window}
import scala.collection.JavaConverters._
import suiryc.scala.concurrent.RichFuture.Action
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.event.Events
import suiryc.scala.javafx.scene.control.Dialogs
import suiryc.scala.javafx.stage.{StageLocationPersistentView, Stages}
import suiryc.scala.javafx.stage.Stages.StageLocation
import suiryc.scala.settings.ConfigEntry

// TODO: handle user-defined displaying order ?

class EditUnavailabilityPeriodsController extends StageLocationPersistentView(EditUnavailabilityPeriodsController.stageLocation) {

  @FXML
  protected var periodsField: ListView[Savings.UnavailabilityPeriod] = _

  @FXML
  protected var nameField: TextField = _

  @FXML
  protected var yearsField: TextField = _

  @FXML
  protected var monthField: ComboBox[Option[Month]] = _

  @FXML
  protected var plusField: ImageView = _

  @FXML
  protected var minusField: ImageView = _

  @FXML
  protected var tickField: ImageView = _

  protected var edit: Option[Savings.UnavailabilityPeriod] = None

  protected var buttonOk: Node = _

  lazy protected val stage: Stage = nameField.getScene.getWindow.asInstanceOf[Stage]

  private var entries0: Seq[Savings.UnavailabilityPeriod] = Seq.empty

  private var entries: Seq[Savings.UnavailabilityPeriod] = Seq.empty

  private var dbActions: List[Action[Unit, AnyVal]] = Nil

  private var applyReady = false

  private var addReady = false

  private var deleteReady = false

  def initialize(dialog: Dialog[_]): Unit = {
    // Load css
    JFXStyles.addStylesheet(stage.getScene)

    // Lookup OK dialog button
    buttonOk = dialog.getDialogPane.lookupButton(ButtonType.OK)
    // Disable OK button unless there are changes to take into account
    buttonOk.setDisable(true)

    // Set months
    monthField.setItems(FXCollections.observableList((None :: Month.values.toList.map(Some(_))).asJava))
    monthField.setButtonCell(new MonthListCell)
    monthField.setCellFactory(_ => new MonthListCell)

    entries0 = Awaits.readDataStoreUnavailabilityPeriods(stage).getOrElse(Seq.empty).sortBy(_.id)
    entries = entries0

    // Initialize periods list view
    periodsField.setCellFactory(newPeriodCell _)
    updatePeriods()
    // Handle period selection changes
    periodsField.getSelectionModel.selectedItemProperty.listen(onSelectedPeriod())

    // Re-check form when changing things
    nameField.textProperty.listen(checkForm())
    yearsField.textProperty.listen(checkForm())
    monthField.getSelectionModel.selectedItemProperty.listen(checkForm())

    // Request confirmation if changes are pending.
    // We need to handle both 'OK' button and dialog window closing request
    // with non-applied changes.
    def confirmationFilter[A <: Event](close: Boolean)(event: A): Unit = {
      val name = nameField.getText.trim
      val years = yearsField.getText.trim
      val month = getMonth
      // Changes are pending if not editing but name is not empty, or editing
      // and having changed anything.
      val dirty = edit match {
        case Some(period) =>
          (period.id != name) ||
            (period.years.toString != years) ||
            period.month != month

        case None =>
          name.nonEmpty
      }

      persistView()
      val canClose =
        if (dirty) Form.confirmDiscardPendingChanges(Stages.getStage(dialog), event)
        else true
      if (close && canClose) dialog.close()
    }
    buttonOk.addEventFilter(ActionEvent.ACTION, confirmationFilter[ActionEvent](close = false) _)
    stage.setOnCloseRequest(confirmationFilter(close = true) _)

    // Filter keys pressed to trigger some actions if possible:
    //   ENTER applies pending period changes if any
    //   DELETE/'-' applies selected period deletion
    //   '+' applies selected period adding/copy
    def keyFilter(event: KeyEvent): Unit = {
      if (applyReady && (event.getCode == KeyCode.ENTER)) {
        onApply(event)
        event.consume()
      }
      else if (deleteReady && periodsField.isFocused &&
        ((event.getCode == KeyCode.DELETE) || (event.getCharacter == "-")))
      {
        onRemove(event)
        event.consume()
      }
      else if (addReady && (event.getCharacter == "+")
        && !nameField.isFocused && !yearsField.isFocused)
      {
        onAdd(event)
        event.consume()
      }
    }
    dialog.getDialogPane.addEventFilter(KeyEvent.KEY_PRESSED, keyFilter _)
    dialog.getDialogPane.addEventFilter(KeyEvent.KEY_TYPED, keyFilter _)

    // Initial form checking
    checkForm()

    // Initial focus goes to name field
    nameField.requestFocus()
  }

  private def getYears: Int =
    try { yearsField.getText.trim.toInt }
    catch { case _: Exception => 0}

  private def getMonth: Option[Month] =
    Option(monthField.getSelectionModel.getSelectedItem).flatten

  private def addDbAction(action: Action[Unit, AnyVal]): Unit =
    dbActions :+= action

  /**
   * Called when releasing mouse/touch on 'plus' image.
   *
   * Ensures event is on target node.
   * Generates and applies changes to create a new entry.
   * If editing, adds number suffix to ensure name is unique if necessary.
   */
  def onAdd(event: Event): Unit = {
    if (Events.isOnNode(event)) {
      val name = if (edit.isDefined) {
        val entryName = nameField.getText.trim

        @scala.annotation.tailrec
        def loop(n: Int): String = {
          val name =
            if (n == 1) entryName
            else s"$entryName - $n"

          if (!entries.exists(_.id.equalsIgnoreCase(name))) name
          else loop(n + 1)
        }

        loop(1)
      }
      else nameField.getText.trim
      val years = getYears
      val month = getMonth

      val period = Savings.UnavailabilityPeriod(name, years, month)
      entries +:= period
      addDbAction(Action(DataStore.UnavailabilityPeriods.writeEntry(period)))
      applyPeriods()
    }
  }

  /**
   * Called when releasing mouse/touch on 'minus' image.
   *
   * Ensures event is on target node.
   * Even though 'minus' image is not accessible in those conditions, also checks
   * an entry to delete is selected.
   * Applies deletion if conditions are met.
   */
  def onRemove(event: Event): Unit = {
    if (Events.isOnNode(event)) {
      // Make sure there is something to delete
      Option(periodsField.getSelectionModel.getSelectedItem).foreach { period =>
        entries = entries.filterNot(_.id == period.id)
        addDbAction(Action(DataStore.UnavailabilityPeriods.deleteEntry(period.id)))
        applyPeriods()
      }
    }
  }

  /**
   * Called when releasing mouse/touch on 'tick' image.
   *
   * Ensures event is on target node.
   * Generates and applies changes to update edited entry.
   */
  def onApply(event: Event): Unit = {
    if (Events.isOnNode(event)) {
      edit.foreach { period =>
        val name = nameField.getText.trim
        val years = getYears
        val month = getMonth
        val newPeriod = Savings.UnavailabilityPeriod(name, years, month)
        entries = entries.filterNot(_.id == period.id)
        entries +:= newPeriod
        addDbAction(Action(DataStore.UnavailabilityPeriods.updateEntry(period.id, newPeriod)))
        applyPeriods()
      }
    }
  }

  /** Handles period selection changes. */
  private def onSelectedPeriod(): Unit = {
    val newEdit = Option(periodsField.getSelectionModel.getSelectedItem)

    // Update editing fields if we are selecting a new period
    newEdit.filterNot(edit.contains).foreach { period =>
      nameField.setText(period.id)
      yearsField.setText(period.years.toString)
      monthField.getSelectionModel.select(period.month)
    }
    edit = newEdit

    // Finally, re-check form
    checkForm()
  }

  /**
   * Creates a new UnavailabilityPeriod list view cell.
   *
   * Cell has the appropriate value displaying.
   * An event filter is added on the generated cell to allow de-selecting an
   * entry in the list view by clicking on it a second time.
   */
  private def newPeriodCell(@deprecated("unused","") lv: ListView[Savings.UnavailabilityPeriod]): ListCell[Savings.UnavailabilityPeriod] = {
    // See: http://stackoverflow.com/questions/23622703/deselect-an-item-on-an-javafx-listview-on-click
    val cell = new UnavailabilityPeriodCell

    def eventFilter(event: MouseEvent): Unit = {
      if (periodsField.getSelectionModel.getSelectedIndices.contains(cell.getIndex)) {
        // De-select entry
        periodsField.getSelectionModel.clearSelection(cell.getIndex)
      }
      else if (cell.getItem != null) {
        // Select entry
        periodsField.getSelectionModel.select(cell.getIndex)
      }
      // In any case, consume the event so that ListView does not try to
      // process it: we are overriding its behaviour.
      event.consume()

      // At least re-focus on the entry field it not done
      if (!periodsField.isFocused) periodsField.requestFocus()
    }

    cell.addEventFilter(MouseEvent.MOUSE_PRESSED, eventFilter _)

    cell
  }

  /**
   * Applies periods.
   *
   * Resets editing fields and edited period.
   */
  private def applyPeriods(): Unit = {
    edit = None
    resetEditFields()
    entries = entries.sortBy(_.id)
    buttonOk.setDisable(entries == entries0)
    updatePeriods()
  }

  /** Updates the list of periods. */
  private def updatePeriods(): Unit = {
    periodsField.getSelectionModel.clearSelection()
    periodsField.setItems(FXCollections.observableList(entries.asJava))
  }

  /** Resets editing fields. */
  private def resetEditFields(): Unit = {
    nameField.clear()
    yearsField.clear()
    monthField.getSelectionModel.clearSelection()
  }

  /**
   * Checks form.
   *
   * Checks what state to apply on many fields: name, minus, plus, tick.
   */
  private def checkForm(): Unit = {
    val name = nameField.getText.trim
    val years = getYears
    val month = getMonth

    val yearsOk = years > 0
    JFXStyles.toggleError(yearsField, !yearsOk, Strings.positiveValue)

    // Edition is OK if anything was changed.
    val editOk = yearsOk && edit.exists { period =>
      (period.id != name) ||
        (period.years != years) ||
        period.month != month
    }
    val exists = entries.exists(_.id.equalsIgnoreCase(name)) && !edit.exists(_.id.equalsIgnoreCase(name))
    // Selected name is OK if it is not empty and does not already exists.
    val nameOk = !exists && name.nonEmpty

    // Apply name field status: set error style if name is not OK
    JFXStyles.toggleError(nameField, !nameOk,
      if (exists) Some(Strings.nameExists)
      else if (name.isEmpty) Some(Strings.nameEmpty)
      else None
    )

    // Minus field status: enable deletion if entry is selected
    deleteReady = Option(periodsField.getSelectionModel.getSelectedItem).isDefined
    JFXStyles.toggleImageButton(minusField, active = deleteReady)

    // Plus field status: enable if adding new entry which is OK, or
    // copying with non-empty name and other settings are OK.
    addReady =
      if (edit.isEmpty) nameOk && yearsOk
      else name.nonEmpty && yearsOk
    JFXStyles.toggleImageButton(plusField, addReady)

    // Tick field status: enable if name and edition are OK
    applyReady = nameOk && editOk
    JFXStyles.toggleImageButton(tickField, applyReady)
  }

}

object EditUnavailabilityPeriodsController {

  private val stageLocation = ConfigEntry.from[StageLocation](Main.settings.settings,
    Settings.prefix ++ Seq(Settings.KEY_STAGE, "edit-unavailability-periods", Settings.KEY_LOCATION))

  /** Builds a dialog out of this controller. */
  def buildDialog(owner: Window): Dialog[Boolean] = {
    val dialog = new Dialog[Boolean]()
    val title = Strings.editUnavailabilityPeriods
    // Note: initializing owner resets dialog icon, so set the icon afterwards
    Stages.initOwner(dialog, owner)
    Stages.getStage(dialog).getIcons.setAll(Images.iconCalendarMonth)
    dialog.setTitle(title)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val loader = new FXMLLoader(getClass.getResource("/fxml/edit-unavailability-periods.fxml"), I18N.getResources)
    dialog.getDialogPane.setContent(loader.load[Node]())
    val controller = loader.getController[EditUnavailabilityPeriodsController]
    controller.initialize(dialog)

    Dialogs.addPersistence(dialog, controller)

    dialog.setResultConverter(resultConverter(owner, controller) _)

    dialog
  }

  private def resultConverter(owner: Window, controller: EditUnavailabilityPeriodsController)(buttonType: ButtonType): Boolean = {
    val dbActions = controller.dbActions
    if ((buttonType == ButtonType.OK) && dbActions.nonEmpty) {
      Awaits.applyDataStoreChanges(owner, dbActions)
      true
    } else false
  }

}
