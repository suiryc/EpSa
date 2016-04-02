package epsa.controllers

import epsa.I18N
import epsa.I18N.Strings
import epsa.model.Savings
import javafx.beans.property.SimpleBooleanProperty
import javafx.collections.FXCollections
import javafx.event.{ActionEvent, Event}
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.Node
import javafx.scene.control._
import javafx.scene.image.ImageView
import javafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import scala.collection.JavaConversions._
import suiryc.scala.RichOption._
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.event.EventHandler._
import suiryc.scala.javafx.event.Events
import suiryc.scala.javafx.scene.control.{CheckBoxListCellEx, Dialogs}
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.util.Callback

class EditSchemesController {

  import EditSchemesController._

  @FXML
  protected var schemesField: ListView[Savings.Scheme] = _

  @FXML
  protected var nameField: TextField = _

  @FXML
  protected var fundsField: ListView[SelectableFund] = _

  @FXML
  protected var plusField: ImageView = _

  @FXML
  protected var minusField: ImageView = _

  @FXML
  protected var tickField: ImageView = _

  protected var savings0: Savings = _

  protected var savings: Savings = _

  protected var events: List[Savings.Event] = Nil

  protected var edit: Option[Savings.Scheme] = None

  protected var buttonOk: Node = _

  protected lazy val window =
    nameField.getScene.getWindow

  private var applyReady = false

  private var addReady = false

  private var deleteReady = false

  def initialize(savings0: Savings, dialog: Dialog[_], edit0: Option[Savings.Scheme]): Unit = {
    // Save initial state
    this.savings0 = savings0
    savings = savings0

    // Load css
    dialog.getDialogPane.getStylesheets.add(getClass.getResource("/css/form.css").toExternalForm)

    // Lookup OK dialog button
    buttonOk = dialog.getDialogPane.lookupButton(ButtonType.OK)
    // Disable OK button unless there are events to take into account
    buttonOk.setDisable(true)

    // Initialize schemes list view
    schemesField.setCellFactory(Callback { newSchemeCell _ })
    updateSchemes()
    // Handle scheme selection changes
    schemesField.getSelectionModel.selectedItemProperty.listen(onSelectedScheme())

    // Re-check form when scheme name is changed
    nameField.textProperty.listen(checkForm())

    // Initialize funds list view
    // Use CheckBox ListCell elements to populate its content.
    class CheckBoxFundCell extends CheckBoxListCellEx[SelectableFund] {

      import CheckBoxListCellEx._

      override def getInfo(item: SelectableFund): CellInfo =
        CellInfo(
          text = item.fund.name,
          observable = item.check,
          checked = edit.exists(_.funds.contains(item.fund.id)),
          locked = edit.exists(scheme => savings.hasAsset(scheme.id, item.fund.id))
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

    fundsField.setCellFactory(Callback { new CheckBoxFundCell })
    fundsField.setItems(FXCollections.observableList(savings.funds.map(SelectableFund)))
    // Prevent item selection
    fundsField.getSelectionModel.selectedIndexProperty.listen {
      JFXSystem.runLater(fundsField.getSelectionModel.clearSelection())
    }

    // Select initial scheme if any
    edit0.foreach(schemesField.getSelectionModel.select)

    // Request confirmation if changes are pending
    def confirmationFilter(event: ActionEvent): Unit = {
      val name = nameField.getText.trim
      // Changes are pending if not editing but name is not empty, or editing
      // and having changed anything.
      val dirty = edit match {
        case Some(scheme) =>
          val newFunds = getSelectedFunds.map(_.id).toSet
          (scheme.name != name) ||
            scheme.funds.toSet != newFunds

        case None =>
          name.nonEmpty
      }

      if (dirty) {
        val resp = Dialogs.confirmation(
          owner = Some(Stages.getStage(dialog)),
          title = None,
          headerText = Some(Strings.pendingChanges)
        )

        if (!resp.contains(ButtonType.OK)) {
          event.consume()
        }
      }
    }
    buttonOk.addEventFilter(ActionEvent.ACTION, confirmationFilter _)

    // Filter keys pressed to trigger some actions if possible:
    //   ENTER applies pending scheme changes if any
    //   DELETE/'-' applies selected scheme deletion
    //   '+' applies selected scheme adding/copy
    def keyFilter(event: KeyEvent): Unit = {
      if (applyReady && (event.getCode == KeyCode.ENTER)) {
        onApply(event)
        event.consume()
      }
      else if (deleteReady && schemesField.isFocused &&
        ((event.getCode == KeyCode.DELETE) || (event.getCharacter == "-")))
      {
        onRemove(event)
        event.consume()
      }
      else if (addReady && (event.getCharacter == "+") && !nameField.isFocused) {
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

  private def getSelectedFunds: List[Savings.Fund] =
    fundsField.getItems.toList.filter(_.check.get).map(_.fund)

  /**
   * Called when releasing mouse/touch on 'plus' image.
   *
   * Ensures event is on target node.
   * Generates and applies events to create a new scheme with associated funds.
   * If editing, adds number suffix to ensure name is unique if necessary.
   */
  def onAdd(event: Event): Unit = {
    if (Events.isOnNode(event)) {
      val name = if (edit.isDefined) {
        val schemeName = nameField.getText.trim

        @scala.annotation.tailrec
        def loop(n: Int): String = {
          val name =
            if (n == 1) schemeName
            else s"$schemeName - $n"

          if (!savings.schemes.exists(_.name.equalsIgnoreCase(name))) name
          else loop(n + 1)
        }

        loop(1)
      }
      else nameField.getText.trim

      val event = savings.createSchemeEvent(name)
      val newEvents = event ::
        getSelectedFunds.map { fund =>
          Savings.AssociateFund(event.schemeId, fund.id)
        }

      applyEvents(newEvents)
    }
  }

  /**
   * Called when releasing mouse/touch on 'minus' image.
   *
   * Ensures event is on target node.
   * Even though 'minus' image is not accessible in those conditions, also checks
   * a scheme to delete is selected and has no remaining assets.
   * Applies deletion event if conditions are met.
   * Asks for associated funds deletion.
   */
  def onRemove(event: Event): Unit = {
    if (Events.isOnNode(event)) {
      // Make sure there is something to delete, and that we can
      Option(schemesField.getSelectionModel.getSelectedItem).foreach { scheme =>
        if (canDeleteScheme(scheme)) {
          applyEvents(Savings.DeleteScheme(scheme.id) :: confirmFundsDeletion(scheme))
        }
      }
    }
  }

  private def confirmFundsDeletion(scheme: Savings.Scheme): List[Savings.Event] = {
    val lone = scheme.funds.filterNot { fundId =>
      savings.schemes.filterNot(_.id == scheme.id).exists { otherScheme =>
        otherScheme.funds.contains(fundId)
      }
    }
    val funds = savings.funds.filter { fund =>
      lone.contains(fund.id)
    }

    val events = if (funds.nonEmpty) {
      val alert = new Alert(Alert.AlertType.CONFIRMATION)
      alert.initOwner(window)

      val loader = new FXMLLoader(getClass.getResource("/fxml/select-resources.fxml"), I18N.getResources)
      val root = loader.load[Node]()
      alert.getDialogPane.setContent(root)
      val label = root.lookup("#labelField").asInstanceOf[Label]
      label.setText(Strings.deleteAssociatedFunds)
      val resourcesField = root.lookup("#resourcesField").asInstanceOf[ListView[Savings.Fund]]
      resourcesField.setCellFactory(Callback { new FundCell })
      resourcesField.getSelectionModel.setSelectionMode(SelectionMode.MULTIPLE)
      resourcesField.setItems(FXCollections.observableList(funds))
      resourcesField.getSelectionModel.selectAll()

      if (!alert.showAndWait().contains(ButtonType.OK)) Nil
      else resourcesField.getSelectionModel.getSelectedItems.toList.map { fund =>
        Savings.DeleteFund(fund.id)
      }
    }
    else Nil

    events
  }

  /**
   * Called when releasing mouse/touch on 'tick' image.
   *
   * Ensures event is on target node.
   * Generates and applies events to update edited scheme:
   *   - name changing if any
   *   - funds association changes if any
   */
  def onApply(event: Event): Unit = {
    if (Events.isOnNode(event)) {
      edit.foreach { scheme =>
        val name = nameField.getText.trim
        val event1 =
          if (name == scheme.name) None
          else Some(Savings.UpdateScheme(scheme.id, name))

        val oldFunds = scheme.funds.toSet
        val newFunds = getSelectedFunds.map(_.id).toSet

        val newEvents = event1.toList ++ (oldFunds -- newFunds).toList.map { fundId =>
          Savings.DissociateFund(scheme.id, fundId)
        } ++ (newFunds -- oldFunds).toList.sorted.map { fundId =>
          Savings.AssociateFund(scheme.id, fundId)
        }

        applyEvents(newEvents)
      }
    }
  }

  /** Handles scheme selection changes. */
  private def onSelectedScheme(): Unit = {
    val newEdit = Option(schemesField.getSelectionModel.getSelectedItem)

    // Update editing fields if we are selecting a new scheme
    newEdit.filterNot(edit.contains).foreach { scheme =>
      // Note: we need to temporarily disable editing before updating selected
      // funds due to some checking.
      edit = None
      updateEditFields(scheme)
    }
    // If scheme was de-selected, we still want refresh funds view
    if (newEdit.isEmpty) {
      // Refresh ListView in order to re-create cells with appropriate content
      // (checkbox selection, etc).
      fundsField.refresh()
    }
    edit = newEdit

    // Finally, re-check form
    checkForm()
  }

  /**
   * Creates a new Scheme list view cell.
   *
   * Cell has the appropriate value displaying.
   * An event filter is added on the generated cell to allow de-selecting a
   * Scheme in the list view by clicking on it a second time.
   */
  private def newSchemeCell(lv: ListView[Savings.Scheme]): ListCell[Savings.Scheme] = {
    // See: http://stackoverflow.com/questions/23622703/deselect-an-item-on-an-javafx-listview-on-click
    val cell = new SchemeCell

    def eventFilter(event: MouseEvent): Unit = {
      if (schemesField.getSelectionModel.getSelectedIndices.contains(cell.getIndex)) {
        // De-select scheme
        schemesField.getSelectionModel.clearSelection(cell.getIndex)
      }
      else if (cell.getItem != null) {
        // Select scheme
        schemesField.getSelectionModel.select(cell.getIndex)
      }
      // In any case, consume the event so that ListView does not try to
      // process it: we are overriding its behaviour.
      event.consume()

      // At least re-focus on the schemes field it not done
      if (!schemesField.isFocused) schemesField.requestFocus()
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
    savings = savings.processEvents(newEvents:_*)
    events = savings0.flattenEvents(events ++ newEvents)
    buttonOk.setDisable(events.isEmpty)
    updateSchemes()
    // Note: we may delete associated funds, so refresh the list view too.
    updateFunds()
  }

  private def applyEvents(newEvents: List[Savings.Event]): Unit =
    applyEvents(newEvents:_*)

  /**
   * Whether a scheme can be deleted.
   *
   * A scheme can be deleted only if it has no assets.
   */
  private def canDeleteScheme(scheme: Savings.Scheme): Boolean =
    !savings.assets.exists(_.schemeId == scheme.id)

  /** Updates the list of schemes. */
  private def updateSchemes(): Unit = {
    schemesField.getSelectionModel.clearSelection()
    schemesField.setItems(FXCollections.observableList(savings.schemes))
  }

  /** Updates the list of funds. */
  private def updateFunds(): Unit = {
    fundsField.setItems(FXCollections.observableList(savings.funds.map(SelectableFund)))
    // Refresh ListView in order to re-create cells with appropriate content
    // (checkbox selection, etc).
    fundsField.refresh()
  }

  /**
   * Updates editing fields.
   *
   * Upon editing a scheme, sets its name and selects its funds.
   */
  private def updateEditFields(scheme: Savings.Scheme): Unit = {
    nameField.setText(scheme.name)
    // Refresh ListView in order to re-create cells with appropriate content
    // (checkbox selection, etc).
    fundsField.refresh()
  }

  /**
   * Resets editing fields.
   *
   * Clears name and resets funds selection.
   */
  private def resetEditFields(): Unit = {
    nameField.clear()
  }

  /**
   * Checks form.
   *
   * Checks what state to apply on many fields: name, minus, plus, tick.
   */
  private def checkForm(): Unit = {
    val name = nameField.getText.trim
    // Edition is OK if either name or funds are changed.
    val editOk = edit.exists { scheme =>
      val newFunds = getSelectedFunds.map(_.id).toSet
      (scheme.name != name) ||
        scheme.funds.toSet != newFunds
    }
    val exists = savings.schemes.exists(_.name.equalsIgnoreCase(name)) && !edit.exists(_.name.equalsIgnoreCase(name))
    // Selected name is OK if it is not empty and does not already exists.
    val nameOk = !exists && name.nonEmpty

    // Apply name field status: set error style if name is not OK
    Form.toggleError(nameField, !nameOk,
      if (exists) Some(Strings.nameExists)
      else if (name.isEmpty) Some(Strings.nameEmpty)
      else None
    )

    // Minus field status: enable deletion if selected scheme can be deleted
    Option(schemesField.getSelectionModel.getSelectedItem) match {
      case None =>
        deleteReady = false
        Form.toggleImageButton(minusField, set = deleteReady)

      case Some(scheme) =>
        deleteReady = canDeleteScheme(scheme)
        if (deleteReady) Form.toggleImageButton(minusField, set = deleteReady)
        else Form.toggleImageButton(minusField, set = deleteReady, Some(Strings.schemeNotEmpty))
    }

    // Plus field status: enable if adding new scheme which name is OK, or
    // copying with non-empty name.
    addReady =
      if (edit.isEmpty) nameOk
      else name.nonEmpty
    Form.toggleImageButton(plusField, addReady)

    // Tick field status: enable if name and edition are OK
    applyReady = nameOk && editOk
    Form.toggleImageButton(tickField, applyReady)
  }

}

object EditSchemesController {

  /** Builds a dialog out of this controller. */
  def buildDialog(savings: Savings, edit: Option[Savings.Scheme]): Dialog[List[Savings.Event]] = {
    val dialog = new Dialog[List[Savings.Event]]()
    val title = Strings.editSchemes
    dialog.setTitle(title)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val loader = new FXMLLoader(getClass.getResource("/fxml/edit-schemes.fxml"), I18N.getResources)
    dialog.getDialogPane.setContent(loader.load())
    val controller = loader.getController[EditSchemesController]
    controller.initialize(savings, dialog, edit)

    dialog.setResultConverter(Callback { resultConverter(controller) _ })
    Stages.trackMinimumDimensions(Stages.getStage(dialog))

    dialog
  }

  private def resultConverter(controller: EditSchemesController)(buttonType: ButtonType): List[Savings.Event] = {
    if (buttonType != ButtonType.OK) Nil
    else controller.events
  }

  /** ListCell content associated to a CheckBox. */
  case class SelectableFund(fund: Savings.Fund) {
    val check = new SimpleBooleanProperty(false)
  }

}
