package epsa.controllers

import epsa.I18N
import epsa.model.Savings
import java.util.ResourceBundle
import javafx.collections.FXCollections
import javafx.event.{ActionEvent, Event}
import javafx.fxml.{FXMLLoader, FXML}
import javafx.scene.Node
import javafx.scene.control._
import javafx.scene.image.ImageView
import javafx.scene.input.MouseEvent
import scala.collection.JavaConversions._
import suiryc.scala.RichOption._
import suiryc.scala.javafx.collections.RichObservableList._
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.event.EventHandler._
import suiryc.scala.javafx.event.Events
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.util.Callback

class EditSchemesController {

  //@FXML
  //protected var location: URL = _

  @FXML
  protected var resources: ResourceBundle = _

  @FXML
  protected var schemesField: ListView[Savings.Scheme] = _

  @FXML
  protected var nameField: TextField = _

  @FXML
  protected var fundsField: ListView[Savings.Fund] = _

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

  //def initialize(): Unit = { }

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
    // Re-check form when selected scheme changes
    schemesField.getSelectionModel.selectedItemProperty.listen(checkForm())

    // Re-check form when scheme name is changed
    nameField.textProperty.listen(checkForm())

    // Initialize funds list view
    fundsField.setCellFactory(Callback { new FundCell })
    fundsField.getSelectionModel.setSelectionMode(SelectionMode.MULTIPLE)
    fundsField.setItems(FXCollections.observableList(savings.funds))
    // Check selected funds upon change, triggers form re-check if necessary
    fundsField.getSelectionModel.getSelectedItems.listen(checkSelectedFunds())

    // Populate editing fields if a scheme is initially selected
    edit0.foreach(updateEditFields)
    // Note: we must first select funds before entering edition mode (due to
    // some checking).
    edit = edit0
    // Select initial scheme if any
    edit.foreach(schemesField.getSelectionModel.select)

    // Request confirmation if changes are pending
    def confirmationFilter(event: ActionEvent): Unit = {
      val name = nameField.getText.trim
      // Changes are pending if not editing but name is not empty, or editing
      // and having changed anything.
      val dirty = edit match {
        case Some(scheme) =>
          val newFunds = fundsField.getSelectionModel.getSelectedItems.toList.map(_.id).toSet
          (scheme.name != name) ||
            scheme.funds.toSet != newFunds

        case None =>
          name.nonEmpty
      }

      if (dirty) {
        val alert = new Alert(Alert.AlertType.CONFIRMATION)
        alert.initOwner(Stages.getStage(dialog))
        alert.setHeaderText(resources.getString("confirmation.pending-changes"))

        if (!alert.showAndWait().contains(ButtonType.OK)) {
          event.consume()
        }
      }
    }
    buttonOk.addEventFilter(ActionEvent.ACTION, confirmationFilter _)

    // Initial form checking
    checkForm()

    // Initial focus goes to name field
    nameField.requestFocus()
  }

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
        fundsField.getSelectionModel.getSelectedItems.toList.map { fund =>
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

      val loader = new FXMLLoader(getClass.getResource("/fxml/select-resources.fxml"), resources)
      val root = loader.load[Node]()
      alert.getDialogPane.setContent(root)
      val label = root.lookup("#labelField").asInstanceOf[Label]
      label.setText(resources.getString("confirmation.delete-associated-funds"))
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
        val newFunds = fundsField.getSelectionModel.getSelectedItems.toList.map(_.id).toSet

        val newEvents = event1.toList ++ (oldFunds -- newFunds).toList.map { fundId =>
          Savings.DissociateFund(scheme.id, fundId)
        } ++ (newFunds -- oldFunds).toList.sorted.map { fundId =>
          Savings.AssociateFund(scheme.id, fundId)
        }

        applyEvents(newEvents)
      }
    }
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
        // 1. We are not editing it anymore
        edit = None
        // 2. It is removed from selection
        schemesField.getSelectionModel.clearSelection(cell.getIndex)
        // 3. Re-check form
        checkForm()
      }
      else if (cell.getItem != null) {
        // Select scheme
        val scheme = cell.getItem
        // 1. Leave editing mode
        // Note: needed due to some checking when selecting funds.
        edit = None
        // 2. Select it
        schemesField.getSelectionModel.select(cell.getIndex)
        // 3. Update editing fields
        updateEditFields(scheme)
        // 4. We are editing it now
        // Note: need to be done after selecting funds due to some checking.
        edit = Option(scheme)
        // 5. Re-check form
        checkForm()
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
    savings = Savings.processEvents(savings, newEvents:_*)
    events = Savings.flattenEvents(savings0, events ++ newEvents)
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
    fundsField.getSelectionModel.clearSelection()
    fundsField.setItems(FXCollections.observableList(savings.funds))
  }

  /**
   * Updates editing fields.
   *
   * Upon editing a scheme, sets its name and selects its funds.
   */
  private def updateEditFields(scheme: Savings.Scheme): Unit = {
    nameField.setText(scheme.name)
    fundsField.getSelectionModel.clearSelection()
    scheme.funds.map { fundId =>
      savings.getFund(fundId)
    }.foreach(fundsField.getSelectionModel.select)
  }

  /**
   * Resets editing fields.
   *
   * Clears name and resets funds selection.
   */
  private def resetEditFields(): Unit = {
    nameField.clear()
    fundsField.getSelectionModel.clearSelection()
  }

  /**
   * Checks selected funds.
   *
   * If not editing, check form.
   * If editing, ensure funds with assets cannot be de-selected.
   */
  private def checkSelectedFunds(): Unit =
    edit match {
      case Some(scheme) =>
        // Note: on Linux selectedItems may contain a 'null' value the first
        // time it is called ...
        val selected = fundsField.getSelectionModel.getSelectedItems.toList.flatMap(Option(_)).map(_.id).toSet
        val unselected = scheme.funds.filterNot(selected.contains)
        val reselect = unselected.filter { fundId =>
          savings.hasAsset(scheme.id, fundId)
        }
        // If selected funds are OK, simply check form.
        // Otherwise, re-select needed funds, which will trigger a form checking.
        if (reselect.isEmpty) checkForm()
        else {
          val alert = new Alert(Alert.AlertType.WARNING)
          alert.initOwner(window)
          alert.setHeaderText(resources.getString("warning.unselecting-nonempty-resource"))
          alert.showAndWait()

          reselect.map(savings.getFund).foreach(fundsField.getSelectionModel.select)
        }

      case None =>
        checkForm()
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
      val newFunds = fundsField.getSelectionModel.getSelectedItems.toList.map(_.id).toSet
      (scheme.name != name) ||
        scheme.funds.toSet != newFunds
    }
    val exists = savings.schemes.exists(_.name.equalsIgnoreCase(name)) && !edit.exists(_.name.equalsIgnoreCase(name))
    // Selected name is OK if it is not empty and does not already exists.
    val nameOk = !exists && name.nonEmpty

    // Apply name field status:
    // 1. Set issue in tooltip if any
    if (exists) {
      // Name already exists
      nameField.setTooltip(new Tooltip(resources.getString("Name already exists")))
    } else if (name.isEmpty) {
      // Name is empty
      nameField.setTooltip(new Tooltip(resources.getString("Name cannot be empty")))
    } else {
      // No issue
      nameField.setTooltip(null)
    }
    // Set error style if name is not OK
    Form.setStyleError(nameField, !nameOk)

    // Minus field status: enable deletion if selected scheme can be deleted
    Option(schemesField.getSelectionModel.getSelectedItem) match {
      case None =>
        Form.toggleImageButton(minusField, set = false)

      case Some(scheme) =>
        if (canDeleteScheme(scheme)) Form.toggleImageButton(minusField, set = true)
        else Form.toggleImageButton(minusField, set = false, Some(resources.getString("Scheme is not empty")))
    }

    // Plus field status: enable if adding new scheme which name is OK, or
    // copying with non-empty name.
    val addOk =
      if (edit.isEmpty) nameOk
      else name.nonEmpty
    Form.toggleImageButton(plusField, addOk)

    // Tick field status: enable if name and edition are OK
    Form.toggleImageButton(tickField, nameOk && editOk)
  }

}

object EditSchemesController {

  /** Builds a dialog out of this controller. */
  def buildDialog(savings: Savings, edit: Option[Savings.Scheme]): Dialog[List[Savings.Event]] = {
    val resources = I18N.getResources

    val dialog = new Dialog[List[Savings.Event]]()
    val title = resources.getString("Edit schemes")
    dialog.setTitle(title)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val loader = new FXMLLoader(getClass.getResource("/fxml/edit-schemes.fxml"), resources)
    dialog.getDialogPane.setContent(loader.load())
    val controller = loader.getController[EditSchemesController]
    controller.initialize(savings, dialog, edit)

    dialog.setResultConverter(Callback(resultConverter(savings, edit, controller)))
    Stages.trackMinimumDimensions(Stages.getStage(dialog))

    dialog
  }

  private def resultConverter(savings: Savings, edit: Option[Savings.Scheme], controller: EditSchemesController)(buttonType: ButtonType): List[Savings.Event] = {
    if (buttonType != ButtonType.OK) Nil
    else controller.events
  }

}
