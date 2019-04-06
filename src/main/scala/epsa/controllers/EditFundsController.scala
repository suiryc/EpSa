package epsa.controllers

import epsa.{I18N, Main, Settings}
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
import scala.collection.JavaConverters._
import suiryc.scala.RichOption._
import suiryc.scala.javafx.beans.value.RichObservableValue
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.event.Events
import suiryc.scala.javafx.scene.control.{CheckBoxListCellWithInfo, CheckBoxListCellWithSeparator, Dialogs}
import suiryc.scala.javafx.stage.{StageLocationPersistentView, Stages}
import suiryc.scala.javafx.stage.Stages.StageLocation
import suiryc.scala.settings.ConfigEntry

class EditFundsController extends StageLocationPersistentView(EditFundsController.stageLocation) {

  import EditFundsController._

  @FXML
  protected var fundsField: ListView[Option[Savings.Fund]] = _

  @FXML
  protected var nameField: TextField = _

  @FXML
  protected var amfIdField: TextField = _

  @FXML
  protected var disabledCheckBox: CheckBox = _

  @FXML
  protected var schemesField: ListView[Option[SelectableScheme]] = _

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

  lazy protected val stage: Stage = nameField.getScene.getWindow.asInstanceOf[Stage]

  private var applyReady = false

  private var addReady = false

  private var deleteReady = false

  def initialize(savings0: Savings, dialog: Dialog[_], edit0: Option[Savings.Fund]): Unit = {
    // Save initial state
    this.savings0 = savings0
    savings = savings0

    // Load css
    JFXStyles.addStylesheet(stage.getScene)

    // Lookup OK dialog button
    buttonOk = dialog.getDialogPane.lookupButton(ButtonType.OK)
    // Disable OK button unless there are events to take into account
    buttonOk.setDisable(true)

    // Initialize funds list view
    fundsField.setCellFactory(newFundCell _)
    updateFunds()
    // Handle fund selection changes
    fundsField.getSelectionModel.selectedItemProperty.listen(onSelectedFund())

    // Re-check form when fund params are changed
    RichObservableValue.listen[AnyRef](
      nameField.textProperty, amfIdField.textProperty, disabledCheckBox.selectedProperty, commentField.textProperty
    ) {
      checkForm()
    }

    // Initialize schemes list view
    // Use CheckBox ListCell elements to populate its content.
    schemesField.setCellFactory(_ => newSchemeCell())
    // Prevent item selection
    schemesField.getSelectionModel.selectedIndexProperty.listen {
      JFXSystem.runLater(schemesField.getSelectionModel.clearSelection())
    }
    updateSchemes()

    // Select initial fund if any
    edit0.foreach(fund => fundsField.getSelectionModel.select(Some(fund)))

    // Request confirmation if changes are pending.
    // We need to handle both 'OK' button and dialog window closing request
    // with non-applied changes.
    def confirmationFilter[A <: Event](close: Boolean)(event: A): Unit = {
      // We require confirmation if either:
      //   - there are pending changes (selected fund) ready to apply
      //   - there is a pending fund ready to add
      //   - unless validating changes, there are pending events to apply
      val dirty = applyReady || (addReady && edit.isEmpty) || (close && events.nonEmpty)

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
        && !nameField.isFocused && !amfIdField.isFocused && !commentField.isFocused)
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

  /**
   * Creates a new Fund list view cell.
   *
   * Cell has the appropriate value displaying.
   * An event filter is added on the generated cell to allow de-selecting a
   * Fund in the list view by clicking on it a second time.
   */
  private def newFundCell(@deprecated("unused","") lv: ListView[Option[Savings.Fund]]): ListCell[Option[Savings.Fund]] = {
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
   * Creates a new Scheme list view checkbox cell.
   *
   * Cell has the appropriate value displaying.
   * Cell is locked (cannot be deselected) if there is an existing asset for the
   * concerned scheme and fund.
   */
  private def newSchemeCell(): CheckBoxListCellWithSeparator[SelectableScheme] = {
    class CheckBoxSchemeCell extends CheckBoxListCellWithSeparator[SelectableScheme] {

      import CheckBoxListCellWithInfo._

      override def getInfo(item0: Option[SelectableScheme]): CellInfo = {
        // Notes:
        // We are only called when there is an actual item.
        // The 'checked' status was set beforehand if applicable.
        val item = item0.get
        CellInfo(
          text = item.scheme.name,
          observable = item.check,
          checked = item.check.get,
          locked = edit.exists(fund => savings.hasAsset(item.scheme.id, fund.id))
        )
      }

      override protected def setLocked(locked: Boolean): Unit = {
        // If locked (i.e. cell is disabled) use a lower opacity than usual,
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

    new CheckBoxSchemeCell
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
      val amfId = Form.textOrNone(amfIdField.getText)
      val disabled = disabledCheckBox.isSelected
      val comment = Form.textOrNone(commentField.getText)

      val event = savings.createFundEvent(name, amfId, comment)
      // Note: take into account the fact that flattening events will remove
      // unnecessary events; we can push an 'UpdateFund' with the 'disabled'
      // state without checking whether it is needed.
      val newEvents = event ::
        Savings.UpdateFund(event.fundId, name, amfId, comment, disabled) ::
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
      getFund.foreach { fund =>
        if (canDeleteFund(fund) && confirmFundDeletion(fund)) {
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

  private def confirmFundDeletion(fund: Savings.Fund): Boolean =
    if (!fund.used) true
    else Dialogs.confirmation(
      owner = Some(stage),
      title = None,
      headerText = Some(Strings.confirmAction),
      contentText = Some(Strings.deleteUsedResource)
    ).contains(ButtonType.OK)

  private def confirmSchemesDeletion(fund: Savings.Fund): List[Savings.DeleteScheme] = {
    val schemes = savings.schemes.filter { scheme =>
      scheme.funds.contains(fund.id) && (scheme.funds.size == 1)
    }

    val events = if (schemes.nonEmpty) {
      val alert = new Alert(Alert.AlertType.CONFIRMATION, "", ButtonType.OK)
      Stages.initOwner(alert, stage)

      val loader = new FXMLLoader(getClass.getResource("/fxml/select-resources.fxml"), I18N.getResources)
      val root = loader.load[Node]()
      alert.getDialogPane.setContent(root)
      val label = root.lookup("#labelField").asInstanceOf[Label]
      label.setText(Strings.deleteAssociatedSchemes)
      val resourcesField = root.lookup("#resourcesField").asInstanceOf[ListView[Option[Savings.Scheme]]]
      resourcesField.setCellFactory(_ => new SchemeCell)
      resourcesField.getSelectionModel.setSelectionMode(SelectionMode.MULTIPLE)
      val entries = Form.buildOptions(schemes.filter(!_.disabled), schemes.filter(_.disabled))
      resourcesField.setItems(FXCollections.observableList(entries.asJava))
      resourcesField.getSelectionModel.selectAll()

      if (!alert.showAndWait().contains(ButtonType.OK)) Nil
      else resourcesField.getSelectionModel.getSelectedItems.asScala.toList.flatten.map { scheme =>
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
        val amfId = Form.textOrNone(amfIdField.getText)
        val disabled = disabledCheckBox.isSelected
        val comment = Form.textOrNone(commentField.getText)
        val event1 =
          if (fund.copy(name = name, amfId = amfId, comment = comment, disabled = disabled) == fund) None
          else Some(Savings.UpdateFund(fund.id, name, amfId, comment, disabled))

        val oldSchemes = savings.schemes.filter(_.funds.contains(fund.id)).map(_.id).toSet
        val newSchemes = getSelectedSchemes.map(_.id).toSet

        val newEvents = event1.toList ::: (oldSchemes -- newSchemes).toList.map { schemeId =>
          Savings.DissociateFund(schemeId, fund.id)
        } ::: (newSchemes -- oldSchemes).toList.sorted.map { schemeId =>
          Savings.AssociateFund(schemeId, fund.id)
        }

        applyEvents(newEvents)
      }
    }
  }

  /** Handles fund selection changes. */
  private def onSelectedFund(): Unit = {
    val newEdit = getFund

    // Update editing fields if we are selecting a new fund
    newEdit.filterNot(edit.contains).foreach { fund =>
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
   * Applies events.
   *
   * Resets editing fields and edited scheme.
   * Applies events on current Savings value, to compute new one.
   * Updates current list of events to take into account: flattens them to
   * filter unnecessary ones.
   */
  private def applyEvents(newEvents: List[Savings.Event]): Unit = {
    edit = None
    resetEditFields()
    savings = savings.processEvents(newEvents)
    events = savings0.flattenEvents(events ::: newEvents)
    buttonOk.setDisable(events.isEmpty)
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
    !savings.assets.list.exists(_.fundId == fund.id)

  /** Updates the list of funds. */
  private def updateFunds(): Unit = {
    fundsField.getSelectionModel.clearSelection()
    val funds = savings.getFunds(associated = false)
    val entries = Form.buildOptions(funds.filter(!_.disabled), funds.filter(_.disabled))
    fundsField.setItems(FXCollections.observableList(entries.asJava))
  }

  /** Updates the list of schemes. */
  private def updateSchemes(): Unit = {
    val schemes = savings.getSchemes(associated = None).map(SelectableScheme)
    val entries = Form.buildOptions(schemes.filter(!_.scheme.disabled), schemes.filter(_.scheme.disabled))
    schemesField.setItems(FXCollections.observableList(entries.asJava))
  }

  /**
   * Updates editing fields.
   *
   * Upon editing a fund, sets its name and selects its funds.
   */
  private def updateEditFields(fund: Savings.Fund): Unit = {
    nameField.setText(fund.name)
    amfIdField.setText(fund.amfId.orNull)
    disabledCheckBox.setSelected(fund.disabled)
    disabledCheckBox.setDisable(fund.active)
    commentField.setText(fund.comment.orNull)
    // Refresh ListView in order to re-create cells with appropriate content
    // (checkbox selection, etc).
    // Note: we also set the 'checked' status for each item, so that it is
    // properly taken into account (even for the cells not visible in the
    // current view).
    schemesField.getItems.asScala.toList.flatten.foreach { item =>
      item.check.set(item.scheme.funds.contains(fund.id))
    }
    schemesField.refresh()
  }

  /**
   * Resets editing fields.
   *
   * Clears name and resets schemes selection.
   */
  private def resetEditFields(): Unit = {
    nameField.clear()
    amfIdField.clear()
    commentField.clear()
  }

  /**
   * Checks form.
   *
   * Checks what state to apply on many fields: name, minus, plus, tick.
   */
  private def checkForm(): Unit = {
    val name = nameField.getText.trim
    val amfId = Form.textOrNone(amfIdField.getText)
    val disabled = disabledCheckBox.isSelected
    val comment = Form.textOrNone(commentField.getText)
    // Edition is OK if either name or schemes are changed.
    val editOk = edit.exists { fund =>
      val oldSchemes = savings.schemes.filter(_.funds.contains(fund.id)).map(_.id).toSet
      val newSchemes = getSelectedSchemes.map(_.id).toSet
      (fund.copy(name = name, amfId = amfId, comment = comment, disabled = disabled) != fund) ||
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
    getFund match {
      case None =>
        deleteReady = false
        JFXStyles.toggleImageButton(minusField, active = deleteReady)

      case Some(fund) =>
        deleteReady = canDeleteFund(fund)
        if (deleteReady) JFXStyles.toggleImageButton(minusField, active = deleteReady)
        else JFXStyles.toggleImageButton(minusField, active = deleteReady, Some(Strings.fundNotEmpty))
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

  private def getFund: Option[Savings.Fund] =
    Option(fundsField.getSelectionModel.getSelectedItem).flatten

  private def getSelectedSchemes: List[Savings.Scheme] =
    schemesField.getItems.asScala.toList.flatten.filter(_.check.get).map(_.scheme)

}

object EditFundsController {

  private val stageLocation = ConfigEntry.from[StageLocation](Main.settings.settings,
    Settings.prefix ++ Seq(Settings.KEY_STAGE, "edit-funds", Settings.KEY_LOCATION))

  /** Builds a dialog out of this controller. */
  def buildDialog(owner: Window, savings: Savings, edit: Option[Savings.Fund]): Dialog[List[Savings.Event]] = {
    val dialog = new Dialog[List[Savings.Event]]()
    val title = Strings.editFunds
    // Note: initializing owner resets dialog icon, so set the icon afterwards
    Stages.initOwner(dialog, owner)
    Stages.getStage(dialog).getIcons.setAll(Images.iconTable)
    dialog.setTitle(title)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val loader = new FXMLLoader(getClass.getResource("/fxml/edit-funds.fxml"), I18N.getResources)
    dialog.getDialogPane.setContent(loader.load())
    val controller = loader.getController[EditFundsController]
    controller.initialize(savings, dialog, edit)

    Dialogs.addPersistence(dialog, controller)

    dialog.setResultConverter(resultConverter(controller) _)

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
