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
import javafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import scala.collection.JavaConversions._
import suiryc.scala.RichOption._
import suiryc.scala.javafx.collections.RichObservableList._
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.event.EventHandler._
import suiryc.scala.javafx.event.Events
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.util.Callback

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

  protected var savings0: Savings = _

  protected var savings: Savings = _

  protected var events: List[Savings.Event] = Nil

  protected var edit: Option[Savings.Fund] = None

  protected var buttonOk: Node = _

  protected lazy val window =
    nameField.getScene.getWindow

  private var applyReady = false

  private var addReady = false

  private var deleteReady = false

  //def initialize(): Unit = { }

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

    // Re-check form when fund name is changed
    nameField.textProperty.listen(checkForm())

    // Initialize schemes list view
    schemesField.setCellFactory(Callback { new SchemeCell })
    schemesField.getSelectionModel.setSelectionMode(SelectionMode.MULTIPLE)
    schemesField.setItems(FXCollections.observableList(savings.schemes))
    // Check selected schemes upon change, triggers form re-check if necessary
    schemesField.getSelectionModel.getSelectedItems.listen(checkSelectedSchemes())

    // Select initial fund if any
    edit0.foreach(fundsField.getSelectionModel.select)

    // Request confirmation if changes are pending
    def confirmationFilter(event: ActionEvent): Unit = {
      val name = nameField.getText.trim
      // Changes are pending if not editing but name is not empty, or editing
      // and having changed anything.
      val dirty = edit match {
        case Some(fund) =>
          val oldSchemes = savings.schemes.filter(_.funds.contains(fund.id)).map(_.id).toSet
          val newSchemes = schemesField.getSelectionModel.getSelectedItems.toList.map(_.id).toSet
          (fund.name != name) ||
            oldSchemes != newSchemes

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

    // Filter keys pressed to trigger some actions if possible:
    //   ENTER applies pending fund changes if any
    //   DELETE/'-' applies selected fund deletion
    //   '+' applies selected fund adding/copy
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
      alert.initOwner(window)

      val loader = new FXMLLoader(getClass.getResource("/fxml/select-resources.fxml"), resources)
      val root = loader.load[Node]()
      alert.getDialogPane.setContent(root)
      val label = root.lookup("#labelField").asInstanceOf[Label]
      label.setText(resources.getString("confirmation.delete-associated-schemes"))
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
    savings = Savings.processEvents(savings, newEvents:_*)
    events = Savings.flattenEvents(savings0, events ++ newEvents)
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
  private def updateEditFields(fund: Savings.Fund): Unit = {
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
   * Checks selected schemes.
   *
   * If not editing, check form.
   * If editing, ensure schemes with assets cannot be de-selected.
   */
  private def checkSelectedSchemes(): Unit =
    edit match {
      case Some(fund) =>
        // Note: on Linux selectedItems may contain a 'null' value the first
        // time it is called ...
        val selected = schemesField.getSelectionModel.getSelectedItems.toList.flatMap(Option(_)).map(_.id).toSet
        val unselected = savings.schemes.map(_.id).filterNot(selected.contains)
        val reselect = unselected.filter { schemeId =>
          savings.hasAsset(schemeId, fund.id)
        }
        // If selected schemes are OK, simply check form.
        // Otherwise, re-select needed schemes, which will trigger a form checking.
        if (reselect.isEmpty) checkForm()
        else {
          val alert = new Alert(Alert.AlertType.WARNING)
          alert.initOwner(window)
          alert.setHeaderText(resources.getString("warning.unselecting-nonempty-resource"))
          alert.showAndWait()

          reselect.map(savings.getScheme).foreach(schemesField.getSelectionModel.select)
        }

      case None =>
        checkForm()
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
        deleteReady = false
        Form.toggleImageButton(minusField, set = deleteReady)

      case Some(fund) =>
        deleteReady = canDeleteFund(fund)
        if (deleteReady) Form.toggleImageButton(minusField, set = deleteReady)
        else Form.toggleImageButton(minusField, set = deleteReady, Some(resources.getString("Fund is not empty")))
    }

    // Plus field status: enable if adding new fund which name is OK, or
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

    dialog.setResultConverter(Callback { resultConverter(savings, edit, controller) _ })
    Stages.trackMinimumDimensions(Stages.getStage(dialog))

    dialog
  }

  def resultConverter(savings: Savings, edit: Option[Savings.Fund], controller: EditFundsController)(buttonType: ButtonType): List[Savings.Event] = {
    if (buttonType != ButtonType.OK) Nil
    else controller.events
  }

}
