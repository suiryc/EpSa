package epsa.controllers

import epsa.I18N
import epsa.model.Savings
import java.util.ResourceBundle
import javafx.beans.property.{Property, SimpleObjectProperty}
import javafx.collections.FXCollections
import javafx.event.Event
import javafx.fxml.{FXMLLoader, FXML}
import javafx.scene.Node
import javafx.scene.control._
import javafx.scene.image.ImageView
import javafx.scene.input.MouseEvent
import scala.collection.JavaConversions._
import suiryc.scala.javafx.collections.RichObservableList._
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.event.Events
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.util.Callback._

// TODO - prevent dialog closing if changes are pending
class EditFundsController {

  //@FXML
  //protected var location: URL = _

  @FXML
  protected var resources: ResourceBundle = _

  @FXML
  protected var fundsField: ListView[Savings.Fund] = _

  @FXML
  protected var nameField: TextField = _

  @FXML
  protected var schemesField: ListView[Savings.Scheme] = _

  @FXML
  protected var plusField: ImageView = _

  @FXML
  protected var minusField: ImageView = _

  @FXML
  protected var tickField: ImageView = _

  protected var savings: Savings = _

  protected var events: Property[List[Savings.Event]] =
    new SimpleObjectProperty(Nil)

  protected var edit: Option[Savings.Fund] = None

  protected var buttonOk: Node = _

  //def initialize(): Unit = { }

  def initialize(savings0: Savings, dialog: Dialog[_], edit0: Option[Savings.Fund]): Unit = {
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

    // Initialize funds list view
    fundsField.setCellFactory { (lv: ListView[Savings.Fund]) =>
      newFundCell(lv)
    }
    updateFunds()
    // Re-check form when selected scheme changes
    fundsField.getSelectionModel.selectedItemProperty.listen(checkForm())

    // Re-check form when scheme name is changed
    nameField.textProperty.listen(checkForm())

    // Initialize schemes list view
    schemesField.setCellFactory { (lv: ListView[Savings.Scheme]) =>
      new SchemeCell
    }
    schemesField.getSelectionModel.setSelectionMode(SelectionMode.MULTIPLE)
    schemesField.setItems(FXCollections.observableList(savings.schemes))
    // Re-check form when selected funds are changed
    schemesField.getSelectionModel.getSelectedItems.listen(checkForm())

    // Populate editing fields if a fund is initially selected
    updateEditFields()
    // Select initial fund if any
    edit.foreach(fundsField.getSelectionModel.select)

    // Initial form checking
    checkForm()

    // Initial focus goes to name field
    nameField.requestFocus()
  }

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

      val event = savings.createFundEvent(name)
      val newEvents = event ::
        schemesField.getSelectionModel.getSelectedItems.toList.map { scheme =>
          Savings.AssociateFund(scheme.id, event.fundId)
        }

      applyEvents(newEvents:_*)
    }
  }

  /**
   * Called when releasing mouse/touch on 'minus' image.
   *
   * Ensures event is on target node.
   * Even though 'minus' image is not accessible in those conditions, also checks
   * a fund to delete is selected and has no remaining assets.
   * Applies deletion event if conditions are met.
   */
  def onRemove(event: Event): Unit = {
    if (Events.isOnNode(event)) {
      // TODO - ask whether to also remove lone schemes
      // Make sure there is something to delete, and that we can
      Option(fundsField.getSelectionModel.getSelectedItem).foreach { fund =>
        if (canDeleteFund(fund)) {
          // Note: don't forget to dissociate fund first
          val newEvents = savings.schemes.filter { scheme =>
            scheme.funds.contains(fund.id)
          }.map(_.id).map { schemeId =>
            Savings.DissociateFund(schemeId, fund.id)
          } :+ Savings.DeleteFund(fund.id)

          applyEvents(newEvents:_*)
        }
      }
    }
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
        val event1 =
          if (name == fund.name) None
          else Some(Savings.UpdateFund(fund.id, name))

        val oldSchemes = savings.schemes.filter(_.funds.contains(fund.id)).map(_.id).toSet
        val newSchemes = schemesField.getSelectionModel.getSelectedItems.toList.map(_.id).toSet

        val newEvents = event1.toList ++ (oldSchemes -- newSchemes).toList.map { schemeId =>
          Savings.DissociateFund(schemeId, fund.id)
        } ++ (newSchemes -- oldSchemes).toList.sorted.map { schemeId =>
          Savings.AssociateFund(schemeId, fund.id)
        }

        applyEvents(newEvents:_*)
      }
    }
  }

  /**
   * Creates a new Fund list view cell.
   *
   * Cell has the appropriate value displaying.
   * An event filter is added on the generated cell to allow de-selecting a
   * Fund in the list view by clicking on it a second time.
   */
  private def newFundCell(lv: ListView[Savings.Fund]): ListCell[Savings.Fund] = {
    import suiryc.scala.javafx.event.EventHandler._

    // See: http://stackoverflow.com/questions/23622703/deselect-an-item-on-an-javafx-listview-on-click
    val cell = new FundCell

    def eventFilter(event: MouseEvent): Unit = {
      if (fundsField.getSelectionModel.getSelectedIndices.contains(cell.getIndex)) {
        // De-select fund
        // 1. We are not editing it anymore
        edit = None
        // 2. It is removed from selection
        fundsField.getSelectionModel.clearSelection(cell.getIndex)
        // 3. Re-check form
        checkForm()
      }
      else if (cell.getItem != null) {
        // Select scheme
        // 1. We are editing it now
        edit = Option(cell.getItem)
        // 2. Select it
        fundsField.getSelectionModel.select(cell.getIndex)
        // 3. Update editing fields
        updateEditFields()
        // 4. Re-check form
        checkForm()
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
   * Resets editing fields
   * Applies events on current Savings value, to compute new one.
   * Updates current list of events to take into account: flattens them to
   * filter unnecessary ones.
   */
  private def applyEvents(newEvents: Savings.Event*): Unit = {
    resetEditFields()
    savings = Savings.processEvents(savings, newEvents:_*)
    events.setValue(Savings.flattenEvents(events.getValue ++ newEvents))
    // Note: since we may change Scheme objects (fund association), it is
    // necessary to update the list view to be able to select them by instance.
    updateSchemes()
    updateFunds()
  }

  /**
   * Whether a fund can be deleted.
   *
   * A fund can be deleted only if it has no assets.
   */
  private def canDeleteFund(fund: Savings.Fund): Boolean =
    !savings.assets.exists(_.fundId == fund.id)

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
   * Upon editing a fund, sets its name and selects its funds.
   */
  private def updateEditFields(): Unit =
    edit.foreach { fund =>
      nameField.setText(fund.name)
      schemesField.getSelectionModel.clearSelection()
      savings.schemes.filter { scheme =>
        scheme.funds.contains(fund.id)
      }.foreach(schemesField.getSelectionModel.select)
    }

  /**
   * Resets editing fields.
   *
   * Clears name and resets schemes selection.
   */
  private def resetEditFields(): Unit = {
    nameField.clear()
    schemesField.getSelectionModel.clearSelection()
  }

  /**
   * Checks form.
   *
   * Checks what state to apply on many fields: name, minus, plus, tick.
   */
  def checkForm(): Unit = {
    val name = nameField.getText.trim
    // Edition is OK if either name or schemes are changed.
    val editOk = edit.exists { fund =>
      import scala.collection.JavaConversions._

      val oldSchemes = savings.schemes.filter(_.funds.contains(fund.id)).map(_.id).toSet
      val newSchemes = schemesField.getSelectionModel.getSelectedItems.toList.map(_.id).toSet
      (fund.name != name) ||
        oldSchemes != newSchemes
    }
    val exists = savings.funds.exists(_.name.equalsIgnoreCase(name)) && !edit.exists(_.name.equalsIgnoreCase(name))
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

    // Minus field status: enable deletion if selected fund can be deleted
    Option(fundsField.getSelectionModel.getSelectedItem) match {
      case None =>
        Form.toggleImageButton(minusField, set = false)

      case Some(fund) =>
        if (canDeleteFund(fund)) Form.toggleImageButton(minusField, set = true)
        else Form.toggleImageButton(minusField, set = false, Some(resources.getString("Fund is not empty")))
    }

    // Plus field status: enable if adding new fund which name is OK, or
    // copying with non-empty name.
    val addOk =
      if (fundsField.getSelectionModel.isEmpty) nameOk
      else name.nonEmpty
    Form.toggleImageButton(plusField, addOk)

    // Tick field status: enable if name and edition are OK
    Form.toggleImageButton(tickField, nameOk && editOk)
  }

}

object EditFundsController {

  /** Builds a dialog out of this controller. */
  def buildDialog(savings: Savings, edit: Option[Savings.Fund]): Dialog[List[Savings.Event]] = {
    val resources = I18N.getResources

    val dialog = new Dialog[List[Savings.Event]]()
    val title = resources.getString("Edit funds")
    dialog.setTitle(title)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val loader = new FXMLLoader(getClass.getResource("/fxml/edit-funds.fxml"), resources)
    dialog.getDialogPane.setContent(loader.load())
    val controller = loader.getController[EditFundsController]
    controller.initialize(savings, dialog, edit)

    dialog.setResultConverter(resultConverter(savings, edit, controller) _)
    Stages.trackMinimumDimensions(Stages.getStage(dialog))

    dialog
  }

  def resultConverter(savings: Savings, edit: Option[Savings.Fund], controller: EditFundsController)(buttonType: ButtonType): List[Savings.Event] = {
    if (buttonType != ButtonType.OK) Nil
    else controller.events.getValue
  }

}
