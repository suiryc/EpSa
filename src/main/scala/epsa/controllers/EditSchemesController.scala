package epsa.controllers

import epsa.I18N
import epsa.model.Savings
import java.util.{ResourceBundle, UUID}
import javafx.beans.property.{Property, SimpleObjectProperty}
import javafx.collections.FXCollections
import javafx.event.Event
import javafx.fxml.{FXMLLoader, FXML}
import javafx.scene.Node
import javafx.scene.control._
import javafx.scene.image.ImageView
import javafx.scene.input.MouseEvent
import scala.collection.JavaConversions._
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.collections.RichObservableList._
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.util.Callback._

class EditSchemesController {

  //@FXML
  //protected var location: URL = _

  @FXML
  protected var resources: ResourceBundle = _

  @FXML
  protected var nameField: TextField = _

  @FXML
  protected var schemesField: ListView[Savings.Scheme] = _

  @FXML
  protected var fundsField: ListView[Savings.Fund] = _

  @FXML
  protected var plusField: ImageView = _

  @FXML
  protected var minusField: ImageView = _

  @FXML
  protected var tickField: ImageView = _

  protected var savings: Savings = _

  protected var events: Property[List[Savings.Event]] =
    new SimpleObjectProperty(Nil)

  protected var edit: Option[Savings.Scheme] = None

  protected var buttonOk: Node = _

  //def initialize(): Unit = { }

  def initialize(savings0: Savings, dialog: Dialog[_], edit0: Option[Savings.Scheme]): Unit = {
    // Save initial state
    savings = savings0
    edit = edit0

    // Load css
    dialog.getDialogPane.getStylesheets.add(getClass.getResource("/css/form.css").toExternalForm)

    // Lookup OK dialog button
    buttonOk = dialog.getDialogPane.lookupButton(ButtonType.OK)
    // Disable OK button unless there are events to take into account
    buttonOk.setDisable(true)
    events.listen { events =>
      buttonOk.setDisable(events.isEmpty)
    }

    // Initialize schemes list view
    schemesField.setCellFactory { (lv: ListView[Savings.Scheme]) =>
      newSchemeCell(lv)
    }
    updateSchemes()
    // Re-check form when selected scheme changes
    schemesField.getSelectionModel.selectedItemProperty.listen(checkForm())

    // Re-check form when scheme name is changed
    nameField.textProperty.listen(checkForm())

    // Initialize funds list view
    fundsField.setCellFactory { (lv: ListView[Savings.Fund]) =>
      new FundCell
    }
    fundsField.getSelectionModel.setSelectionMode(SelectionMode.MULTIPLE)
    fundsField.setItems(FXCollections.observableList(savings.funds))
    // Re-check form when selected funds are changed
    fundsField.getSelectionModel.getSelectedItems.listen(checkForm())

    // Populate editing fields if a scheme is initially selected
    updateEditFields()
    // Select initial scheme if any
    edit.foreach(schemesField.getSelectionModel.select)

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
    if (isEventOnNode(event)) {
      val name = if (edit.isDefined) {
        val schemeName = nameField.getText

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
      else nameField.getText

      val event = savings.createSchemeEvent(name)
      val newEvents = event ::
        fundsField.getSelectionModel.getSelectedItems.toList.map { fund =>
          Savings.AssociateFund(event.schemeId, fund.id)
        }

      applyEvents(newEvents:_*)
    }
  }

  /**
   * Called when releasing mouse/touch on 'minus' image.
   *
   * Ensures event is on target node.
   * Even though 'minus' image is not accessible in those conditions, also checks
   * a scheme to delete is selected and has no remaining assets.
   * Applies deletion event if conditions are met.
   */
  def onRemove(event: Event): Unit = {
    if (isEventOnNode(event)) {
      // XXX - ask whether to also remove lone funds
      // Make sure there is something to delete, and that we can
      Option(schemesField.getSelectionModel.getSelectedItem).foreach { scheme =>
        if (canDeleteScheme(scheme)) {
          applyEvents(Savings.DeleteScheme(scheme.id))
        }
      }
    }
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
    if (isEventOnNode(event)) {
      edit.foreach { scheme =>
        val name = nameField.getText
        val event1 =
          if (name == scheme.name) None
          else Some(Savings.UpdateScheme(scheme.id, name))

        val oldFunds = scheme.funds.toSet
        val newFunds = fundsField.getSelectionModel.getSelectedItems.toList.map(_.id).toSet

        event1.toList ++ (oldFunds -- newFunds).toList.map { fundId =>
          Savings.DissociateFund(scheme.id, fundId)
        } ++ (newFunds -- oldFunds).toList.sorted.map { fundId =>
          Savings.AssociateFund(scheme.id, fundId)
        }
      }
    }
  }

  /**
   * Checks (mouse) event is no target mode.
   *
   * Allows to not take into account events for which (mouse) releasing is not
   * performed while still on target node.
   * Non-mouse events (usually touch) are always considered on target.
   */
  private def isEventOnNode(event: Event): Boolean =
    event match {
      case event: MouseEvent => event.getTarget.asInstanceOf[Node].contains(event.getX, event.getY)
      case _ => true
    }

  /**
   * Creates a new Scheme list view cell.
   *
   * Cell has the appropriate value displaying.
   * An event filter is added on the generated cell to allow de-selecting a
   * Scheme in the list view by clicking on it a second time.
   */
  private def newSchemeCell(lv: ListView[Savings.Scheme]): ListCell[Savings.Scheme] = {
    import suiryc.scala.javafx.event.EventHandler._

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
        // 1. We are editing it now
        edit = Option(cell.getItem)
        // 2. Select it
        schemesField.getSelectionModel.select(cell.getIndex)
        // 3. Update editing fields
        updateEditFields()
        // 4. Re-check form
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
   * Resets editing fields
   * Applies events on current Savings value, to compute new one.
   * Updates current list of events to take into account: flattens them to
   * filter unnecessary ones.
   */
  private def applyEvents(newEvents: Savings.Event*): Unit = {
    resetEditFields()
    savings = Savings.processEvents(savings, newEvents:_*)
    events.setValue(flattenEvents(events.getValue ++ newEvents))
    updateSchemes()
  }

  /**
   * Flattens events.
   *
   * Filters unnecessary events, e.g. when creating then deleting a scheme or
   * fund.
   */
  private def flattenEvents(events: List[Savings.Event]): List[Savings.Event] = {
    case class Data(schemesCreated: Set[UUID] = Set.empty, fundsCreated: Set[UUID] = Set.empty,
        schemesNop: Set[UUID] = Set.empty, fundsNop: Set[UUID] = Set.empty)

    val r = events.foldLeft(Data()) { (data, event) =>
      event match {
        case Savings.CreateScheme(id, _) =>
          data.copy(schemesCreated = data.schemesCreated + id)

        case Savings.DeleteScheme(id) =>
          if (!data.schemesCreated.contains(id)) data
          else data.copy(schemesNop = data.schemesNop + id,
            schemesCreated = data.schemesCreated - id)

        case Savings.CreateFund(id, _) =>
          data.copy(fundsCreated = data.fundsCreated + id)

        case Savings.DeleteFund(id) =>
          if (!data.fundsCreated.contains(id)) data
          else data.copy(fundsNop = data.fundsNop + id,
            fundsCreated = data.fundsCreated - id)

        case _ => data
      }
    }

    events.filterNot { event =>
      (event.isInstanceOf[Savings.SchemeEvent] && r.schemesNop.contains(event.asInstanceOf[Savings.SchemeEvent].schemeId)) ||
        (event.isInstanceOf[Savings.FundEvent] && r.fundsNop.contains(event.asInstanceOf[Savings.FundEvent].fundId))
    }
  }

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

  /**
   * Updates editing fields.
   *
   * Upon editing a scheme, sets its name and selects its funds.
   */
  private def updateEditFields(): Unit =
    edit.foreach { scheme =>
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
   * Checks form.
   *
   * Checks what state to apply on many fields: name, minus, plus, tick.
   */
  private def checkForm(): Unit = {
    val name = nameField.getText
    // Edition is OK if either name or funds are changed.
    val editOk = edit.exists { scheme =>
      import scala.collection.JavaConversions._

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
        toggleImageButton(minusField, set = false)

      case Some(scheme) =>
        if (canDeleteScheme(scheme)) toggleImageButton(minusField, set = true)
        else toggleImageButton(minusField, set = false, Some(resources.getString("Scheme is not empty")))
    }

    // Plus field status: enable if adding new scheme which name is OK, or
    // copying with non-empty name.
    val addOk =
      if (schemesField.getSelectionModel.isEmpty) nameOk
      else name.nonEmpty
    toggleImageButton(plusField, addOk)

    // Tick field status: enable if name and edition are OK
    toggleImageButton(tickField, nameOk && editOk)
  }

  /**
   * Toggles image status.
   *
   * When set, sets 'image-button' style class and have node opaque.
   * Otherwise unsets 'image-button' style class and have node 60% transparent.
   * Also installs/uninstalls tooltip message.
   */
  private def toggleImageButton(node: ImageView, set: Boolean, msgOpt: Option[String] = None): Unit = {
    // Note: do not disable node otherwise tooltip won't work
    if (set) {
      Form.setStyleImageButton(node, set = true)
      node.setOpacity(1.0)
    }
    else {
      Form.setStyleImageButton(node, set = false)
      node.setOpacity(0.4)
    }

    msgOpt match {
      case Some(msg) =>
        Tooltip.install(node, new Tooltip(msg))

      case None =>
        // Note: uninstall takes a Tooltip but does not use it (and we did not
        // keep the installed tooltip if any).
        Tooltip.uninstall(node, null)
    }
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

    dialog.setResultConverter(resultConverter(savings, edit, controller) _)
    Stages.trackMinimumDimensions(Stages.getStage(dialog))

    dialog
  }

  private def resultConverter(savings: Savings, edit: Option[Savings.Scheme], controller: EditSchemesController)(buttonType: ButtonType): List[Savings.Event] = {
    if (buttonType != ButtonType.OK) Nil
    else controller.events.getValue
  }

}
