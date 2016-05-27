package epsa.controllers

import epsa.I18N
import epsa.I18N.Strings
import epsa.model.Savings
import epsa.util.JFXStyles
import javafx.beans.property.SimpleBooleanProperty
import javafx.collections.FXCollections
import javafx.event.{ActionEvent, Event}
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.Node
import javafx.scene.control._
import javafx.scene.image.ImageView
import javafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import javafx.stage.{Stage, Window}
import scala.collection.JavaConversions._
import suiryc.scala.RichOption._
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.event.EventHandler._
import suiryc.scala.javafx.event.Events
import suiryc.scala.javafx.scene.control.CheckBoxListCellEx
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.stage.Stages.StageLocation
import suiryc.scala.javafx.util.Callback
import suiryc.scala.settings.Preference

// TODO: possibility (checkbox) to disable a fund
//   - fund kept in db (for history) and accessible in fund editing form, but not useable elsewhere
//     => only allowed if fund is used in current savings
//   - stored as an Event in history (or in dedicated datastore table ?)
// TODO: when deleting a fund which was used in assets actions history, warn about loss of history details
class EditFundsController {

  import EditFundsController._

  @FXML
  protected var fundsField: ListView[Savings.Fund] = _

  @FXML
  protected var nameField: TextField = _

  @FXML
  protected var schemesField: ListView[SelectableScheme] = _

  @FXML
  protected var commentField: TextArea = _

  @FXML
  protected var plusField: ImageView = _

  @FXML
  protected var minusField: ImageView = _

  @FXML
  protected var tickField: ImageView = _

  protected var savings0: Savings = _

  protected var savings: Savings = _

  protected var events: List[Savings.Event] = Nil

  protected var edit: Option[Savings.Fund] = None

  protected var buttonOk: Node = _

  private lazy val stage = nameField.getScene.getWindow.asInstanceOf[Stage]

  private var applyReady = false

  private var addReady = false

  private var deleteReady = false

  def initialize(savings0: Savings, dialog: Dialog[_], edit0: Option[Savings.Fund]): Unit = {
    // Save initial state
    this.savings0 = savings0
    savings = savings0

    // Load css
    dialog.getDialogPane.getStylesheets.add(getClass.getResource("/css/form.css").toExternalForm)

    // Lookup OK dialog button
    buttonOk = dialog.getDialogPane.lookupButton(ButtonType.OK)
    // Disable OK button unless there are events to take into account
    buttonOk.setDisable(true)

    // Initialize funds list view
    fundsField.setCellFactory(Callback { newFundCell _ })
    updateFunds()
    // Handle fund selection changes
    fundsField.getSelectionModel.selectedItemProperty.listen(onSelectedFund())

    // Re-check form when fund name or comment is changed
    nameField.textProperty.listen(checkForm())
    commentField.textProperty.listen(checkForm())

    // Initialize schemes list view
    // Use CheckBox ListCell elements to populate its content.
    class CheckBoxSchemeCell extends CheckBoxListCellEx[SelectableScheme] {

      import CheckBoxListCellEx._

      override def getInfo(item: SelectableScheme): CellInfo =
        CellInfo(
          text = item.scheme.name,
          observable = item.check,
          checked = edit.exists(fund => item.scheme.funds.contains(fund.id)),
          locked = edit.exists(fund => savings.hasAsset(item.scheme.id, fund.id))
        )

      override protected def setLocked(locked: Boolean): Unit = {
        // If locked (i.e. cell is disabled) use a higher opacity than usual,
        // and display a tooltip explaining why the checkbox can't be changed.
        if (!locked) {
          setTooltip(null)
          setOpacity(1.0)
        }
        else {
          setTooltip(new Tooltip(Strings.unselectingNonEmptyResource))
          setOpacity(0.8)
        }
      }

      override protected def statusChanged(oldValue: Boolean, newValue: Boolean): Unit = {
        checkForm()
      }

    }

    schemesField.setCellFactory(Callback { new CheckBoxSchemeCell })
    schemesField.setItems(FXCollections.observableList(savings.schemes.map(SelectableScheme)))
    // Prevent item selection
    schemesField.getSelectionModel.selectedIndexProperty.listen {
      JFXSystem.runLater(schemesField.getSelectionModel.clearSelection())
    }

    // Select initial fund if any
    edit0.foreach(fundsField.getSelectionModel.select)

    // Request confirmation if changes are pending.
    // We need to handle both 'OK' button and dialog window closing request
    // with non-applied changes.
    def confirmationFilter[A <: Event](close: Boolean)(event: A): Unit = {
      val name = nameField.getText.trim
      val comment = Form.textOrNone(commentField.getText)
      // Changes are pending if not editing but name is not empty, or editing
      // and having changed anything.
      val dirty = edit match {
        case Some(fund) =>
          val oldSchemes = savings.schemes.filter(_.funds.contains(fund.id)).map(_.id).toSet
          val newSchemes = getSelectedSchemes.map(_.id).toSet
          (fund.name != name) ||
            (fund.comment != comment) ||
            oldSchemes != newSchemes

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
    //   ENTER applies pending fund changes if any (unless comment field has focus)
    //   DELETE/'-' applies selected fund deletion
    //   '+' applies selected fund adding/copy
    def keyFilter(event: KeyEvent): Unit = {
      if (applyReady && (event.getCode == KeyCode.ENTER)
        && !commentField.isFocused)
      {
        onApply(event)
        event.consume()
      }
      else if (deleteReady && fundsField.isFocused &&
        ((event.getCode == KeyCode.DELETE) || (event.getCharacter == "-")))
      {
        onRemove(event)
        event.consume()
      }
      else if (addReady && (event.getCharacter == "+")
        && !nameField.isFocused && !commentField.isFocused)
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

  /** Restores (persisted) view. */
  private def restoreView(): Unit = {
    // Restore stage location
    Option(stageLocation()).foreach { loc =>
      Stages.setLocation(stage, loc, setSize = true)
    }
  }

  /** Persists view (stage location, ...). */
  private def persistView(): Unit = {
    // Persist stage location
    // Note: if iconified, resets it
    stageLocation() = Stages.getLocation(stage).orNull
  }

  private def getSelectedSchemes: List[Savings.Scheme] =
    schemesField.getItems.toList.filter(_.check.get).map(_.scheme)

  /**
   * Called when releasing mouse/touch on 'plus' image.
   *
   * Ensures event is on target node.
   * Generates and applies events to create a new fund with associated schemes.
   * If editing, adds number suffix to ensure name is unique if necessary.
   */
  def onAdd(event: Event): Unit = {
    if (Events.isOnNode(event)) {
      val name = if (edit.isDefined) {
        val fundName = nameField.getText.trim

        @scala.annotation.tailrec
        def loop(n: Int): String = {
          val name =
            if (n == 1) fundName
            else s"$fundName - $n"

          if (!savings.funds.exists(_.name.equalsIgnoreCase(name))) name
          else loop(n + 1)
        }

        loop(1)
      }
      else nameField.getText.trim
      val comment = Form.textOrNone(commentField.getText)

      val event = savings.createFundEvent(name, comment)
      val newEvents = event ::
        getSelectedSchemes.map { scheme =>
          Savings.AssociateFund(scheme.id, event.fundId)
        }

      applyEvents(newEvents)
    }
  }

  /**
   * Called when releasing mouse/touch on 'minus' image.
   *
   * Ensures event is on target node.
   * Even though 'minus' image is not accessible in those conditions, also checks
   * a fund to delete is selected and has no remaining assets.
   * Applies deletion event if conditions are met.
   * Asks for associated schemes deletion.
   */
  def onRemove(event: Event): Unit = {
    if (Events.isOnNode(event)) {
      // Make sure there is something to delete, and that we can
      Option(fundsField.getSelectionModel.getSelectedItem).foreach { fund =>
        if (canDeleteFund(fund)) {
          val deleteSchemes = confirmSchemesDeletion(fund)
          // Note: don't forget to dissociate fund first
          val deleteFund = savings.schemes.filter { scheme =>
            scheme.funds.contains(fund.id) && !deleteSchemes.exists(_.schemeId == scheme.id)
          }.map(_.id).map { schemeId =>
            Savings.DissociateFund(schemeId, fund.id)
          } :+ Savings.DeleteFund(fund.id)

          applyEvents(deleteSchemes ::: deleteFund)
        }
      }
    }
  }

  private def confirmSchemesDeletion(fund: Savings.Fund): List[Savings.DeleteScheme] = {
    val schemes = savings.schemes.filter { scheme =>
      scheme.funds.contains(fund.id) && (scheme.funds.size == 1)
    }

    val events = if (schemes.nonEmpty) {
      val alert = new Alert(Alert.AlertType.CONFIRMATION)
      alert.initOwner(stage)

      val loader = new FXMLLoader(getClass.getResource("/fxml/select-resources.fxml"), I18N.getResources)
      val root = loader.load[Node]()
      alert.getDialogPane.setContent(root)
      val label = root.lookup("#labelField").asInstanceOf[Label]
      label.setText(Strings.deleteAssociatedSchemes)
      val resourcesField = root.lookup("#resourcesField").asInstanceOf[ListView[Savings.Scheme]]
      resourcesField.setCellFactory(Callback { new SchemeCell })
      resourcesField.getSelectionModel.setSelectionMode(SelectionMode.MULTIPLE)
      resourcesField.setItems(FXCollections.observableList(schemes))
      resourcesField.getSelectionModel.selectAll()

      if (!alert.showAndWait().contains(ButtonType.OK)) Nil
      else resourcesField.getSelectionModel.getSelectedItems.toList.map { scheme =>
        Savings.DeleteScheme(scheme.id)
      }
    }
    else Nil

    events
  }

  /**
   * Called when releasing mouse/touch on 'tick' image.
   *
   * Ensures event is on target node.
   * Generates and applies events to update edited fund:
   *   - name changing if any
   *   - funds association changes if any
   */
  def onApply(event: Event): Unit = {
    if (Events.isOnNode(event)) {
      edit.foreach { fund =>
        val name = nameField.getText.trim
        val comment = Form.textOrNone(commentField.getText)
        val event1 =
          if ((name == fund.name) && (comment == fund.comment)) None
          else Some(Savings.UpdateFund(fund.id, name, comment))

        val oldSchemes = savings.schemes.filter(_.funds.contains(fund.id)).map(_.id).toSet
        val newSchemes = getSelectedSchemes.map(_.id).toSet

        val newEvents = event1.toList ++ (oldSchemes -- newSchemes).toList.map { schemeId =>
          Savings.DissociateFund(schemeId, fund.id)
        } ++ (newSchemes -- oldSchemes).toList.sorted.map { schemeId =>
          Savings.AssociateFund(schemeId, fund.id)
        }

        applyEvents(newEvents)
      }
    }
  }

  /** Handles fund selection changes. */
  private def onSelectedFund(): Unit = {
    val newEdit = Option(fundsField.getSelectionModel.getSelectedItem)

    // Update editing fields if we are selecting a new fund
    newEdit.filterNot(edit.contains).foreach { fund =>
      // Note: we need to temporarily disable editing before updating selected
      // schemes due to some checking.
      edit = None
      updateEditFields(fund)
    }
    // If fund was de-selected, we still want refresh schemes view
    if (newEdit.isEmpty) {
      // Refresh ListView in order to re-create cells with appropriate content
      // (checkbox selection, etc).
      schemesField.refresh()
    }
    edit = newEdit

    // Finally, re-check form
    checkForm()
  }

  /**
   * Creates a new Fund list view cell.
   *
   * Cell has the appropriate value displaying.
   * An event filter is added on the generated cell to allow de-selecting a
   * Fund in the list view by clicking on it a second time.
   */
  private def newFundCell(lv: ListView[Savings.Fund]): ListCell[Savings.Fund] = {
    // See: http://stackoverflow.com/questions/23622703/deselect-an-item-on-an-javafx-listview-on-click
    val cell = new FundCell

    def eventFilter(event: MouseEvent): Unit = {
      if (fundsField.getSelectionModel.getSelectedIndices.contains(cell.getIndex)) {
        // De-select fund
        fundsField.getSelectionModel.clearSelection(cell.getIndex)
      }
      else if (cell.getItem != null) {
        // Select fund
        fundsField.getSelectionModel.select(cell.getIndex)
      }
      // In any case, consume the event so that ListView does not try to
      // process it: we are overriding its behaviour.
      event.consume()

      // At least re-focus on the schemes field it not done
      if (!fundsField.isFocused) fundsField.requestFocus()
    }

    cell.addEventFilter(MouseEvent.MOUSE_PRESSED, eventFilter _)

    cell
  }

  /**
   * Applies events.
   *
   * Resets editing fields and edited scheme.
   * Applies events on current Savings value, to compute new one.
   * Updates current list of events to take into account: flattens them to
   * filter unnecessary ones.
   */
  private def applyEvents(newEvents: Savings.Event*): Unit = {
    edit = None
    resetEditFields()
    savings = savings.processEvents(newEvents)
    events = savings0.flattenEvents(events ++ newEvents)
    buttonOk.setDisable(events.isEmpty)
    // Note: since we may change Scheme objects (fund association), it is
    // necessary to update the list view to be able to select them by instance.
    updateSchemes()
    updateFunds()
  }

  private def applyEvents(newEvents: List[Savings.Event]): Unit =
    applyEvents(newEvents:_*)

  /**
   * Whether a fund can be deleted.
   *
   * A fund can be deleted only if it has no assets.
   */
  private def canDeleteFund(fund: Savings.Fund): Boolean =
    !savings.assets.list.exists(_.fundId == fund.id)

  /** Updates the list of schemes. */
  private def updateSchemes(): Unit = {
    schemesField.setItems(FXCollections.observableList(savings.schemes.map(SelectableScheme)))
    // Refresh ListView in order to re-create cells with appropriate content
    // (checkbox selection, etc).
    schemesField.refresh()
  }

  /** Updates the list of funds. */
  private def updateFunds(): Unit = {
    fundsField.getSelectionModel.clearSelection()
    fundsField.setItems(FXCollections.observableList(savings.funds))
  }

  /**
   * Updates editing fields.
   *
   * Upon editing a fund, sets its name and selects its funds.
   */
  private def updateEditFields(fund: Savings.Fund): Unit = {
    nameField.setText(fund.name)
    commentField.setText(fund.comment.orNull)
    // Refresh ListView in order to re-create cells with appropriate content
    // (checkbox selection, etc).
    schemesField.refresh()
  }

  /**
   * Resets editing fields.
   *
   * Clears name and resets schemes selection.
   */
  private def resetEditFields(): Unit = {
    nameField.clear()
    commentField.clear()
  }

  /**
   * Checks form.
   *
   * Checks what state to apply on many fields: name, minus, plus, tick.
   */
  def checkForm(): Unit = {
    val name = nameField.getText.trim
    val comment = Form.textOrNone(commentField.getText)
    // Edition is OK if either name or schemes are changed.
    val editOk = edit.exists { fund =>
      val oldSchemes = savings.schemes.filter(_.funds.contains(fund.id)).map(_.id).toSet
      val newSchemes = getSelectedSchemes.map(_.id).toSet
      (fund.name != name) ||
        (fund.comment != comment) ||
        oldSchemes != newSchemes
    }
    val exists = savings.funds.exists(_.name.equalsIgnoreCase(name)) && !edit.exists(_.name.equalsIgnoreCase(name))
    // Selected name is OK if it is not empty and does not already exists.
    val nameOk = !exists && name.nonEmpty

    // Apply name field status: set error style if name is not OK
    JFXStyles.toggleError(nameField, !nameOk,
      if (exists) Some(Strings.nameExists)
      else if (name.isEmpty) Some(Strings.nameEmpty)
      else None
    )

    // Minus field status: enable deletion if selected fund can be deleted
    Option(fundsField.getSelectionModel.getSelectedItem) match {
      case None =>
        deleteReady = false
        JFXStyles.toggleImageButton(minusField, set = deleteReady)

      case Some(fund) =>
        deleteReady = canDeleteFund(fund)
        if (deleteReady) JFXStyles.toggleImageButton(minusField, set = deleteReady)
        else JFXStyles.toggleImageButton(minusField, set = deleteReady, Some(Strings.fundNotEmpty))
    }

    // Plus field status: enable if adding new fund which name is OK, or
    // copying with non-empty name.
    addReady =
      if (edit.isEmpty) nameOk
      else name.nonEmpty
    JFXStyles.toggleImageButton(plusField, addReady)

    // Tick field status: enable if name and edition are OK
    applyReady = nameOk && editOk
    JFXStyles.toggleImageButton(tickField, applyReady)
  }

}

object EditFundsController {

  import epsa.Settings.prefs

  private val stageLocation = Preference.from("stage.edit-funds.location", null:StageLocation)

  /** Builds a dialog out of this controller. */
  def buildDialog(owner: Option[Window], savings: Savings, edit: Option[Savings.Fund]): Dialog[List[Savings.Event]] = {
    val dialog = new Dialog[List[Savings.Event]]()
    val title = Strings.editFunds
    // Note: initializing owner resets dialog icon, so set the icon afterwards
    owner.foreach(dialog.initOwner)
    Stages.getStage(dialog).getIcons.setAll(Images.iconTable)
    dialog.setTitle(title)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val loader = new FXMLLoader(getClass.getResource("/fxml/edit-funds.fxml"), I18N.getResources)
    dialog.getDialogPane.setContent(loader.load())
    val controller = loader.getController[EditFundsController]
    controller.initialize(savings, dialog, edit)

    // Wait for dialog to be shown before restoring the view
    dialog.showingProperty().listen2 { cancellable =>
      cancellable.cancel()
      controller.restoreView()
    }

    dialog.setResultConverter(Callback { resultConverter(controller) _ })
    Stages.trackMinimumDimensions(Stages.getStage(dialog))

    dialog
  }

  def resultConverter(controller: EditFundsController)(buttonType: ButtonType): List[Savings.Event] = {
    if (buttonType != ButtonType.OK) Nil
    else controller.events
  }

  /** ListCell content associated to a CheckBox. */
  case class SelectableScheme(scheme: Savings.Scheme) {
    val check = new SimpleBooleanProperty(false)
  }

}
