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
import scala.jdk.CollectionConverters._
import suiryc.scala.RichOption._
import suiryc.scala.javafx.beans.value.RichObservableValue
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.event.Events
import suiryc.scala.javafx.scene.control.{CheckBoxListCellWithInfo, CheckBoxListCellWithSeparator, Dialogs}
import suiryc.scala.javafx.stage.{StageLocationPersistentView, Stages}
import suiryc.scala.javafx.stage.Stages.StageLocation
import suiryc.scala.settings.ConfigEntry

class EditSchemesController extends StageLocationPersistentView(EditSchemesController.stageLocation) {

  import EditSchemesController._

  @FXML
  protected var schemesField: ListView[Option[Savings.Scheme]] = _

  @FXML
  protected var nameField: TextField = _

  @FXML
  protected var disabledCheckBox: CheckBox = _

  @FXML
  protected var fundsField: ListView[Option[SelectableFund]] = _

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

  protected var edit: Option[Savings.Scheme] = None

  protected var buttonOk: Node = _

  lazy protected val stage: Stage = nameField.getScene.getWindow.asInstanceOf[Stage]

  private var applyReady = false

  private var addReady = false

  private var deleteReady = false

  def initialize(savings0: Savings, dialog: Dialog[_], edit0: Option[Savings.Scheme]): Unit = {
    // Save initial state
    this.savings0 = savings0
    savings = savings0

    // Load css
    JFXStyles.addStylesheet(stage.getScene)

    // Lookup OK dialog button
    buttonOk = dialog.getDialogPane.lookupButton(ButtonType.OK)
    // Disable OK button unless there are events to take into account
    buttonOk.setDisable(true)

    // Initialize schemes list view
    schemesField.setCellFactory(newSchemeCell _)
    updateSchemes()
    // Handle scheme selection changes
    schemesField.getSelectionModel.selectedItemProperty.listen(onSelectedScheme())

    // Re-check form when scheme params are changed
    RichObservableValue.listen[AnyRef](
      nameField.textProperty, disabledCheckBox.selectedProperty, commentField.textProperty
    ) {
      checkForm()
    }

    // Initialize funds list view
    // Use CheckBox ListCell elements to populate its content.
    fundsField.setCellFactory(_ => newFundCell())
    // Prevent item selection
    fundsField.getSelectionModel.selectedIndexProperty.listen {
      JFXSystem.runLater(fundsField.getSelectionModel.clearSelection())
    }
    updateFunds()

    // Select initial scheme if any
    edit0.foreach(scheme => schemesField.getSelectionModel.select(Some(scheme)))

    // Request confirmation if changes are pending.
    // We need to handle both 'OK' button and dialog window closing request
    // with non-applied changes.
    def confirmationFilter[A <: Event](close: Boolean)(event: A): Unit = {
      // We require confirmation if either:
      //   - there are pending changes (selected scheme) ready to apply
      //   - there is a pending scheme ready to add
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
    //   ENTER applies pending scheme changes if any (unless comment field has focus)
    //   DELETE/'-' applies selected scheme deletion
    //   '+' applies selected scheme adding/copy
    def keyFilter(event: KeyEvent): Unit = {
      if (applyReady && (event.getCode == KeyCode.ENTER)
        && !commentField.isFocused)
      {
        onApply(event)
        event.consume()
      }
      else if (deleteReady && schemesField.isFocused &&
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

  /**
   * Creates a new Scheme list view cell.
   *
   * Cell has the appropriate value displaying.
   * An event filter is added on the generated cell to allow de-selecting a
   * Scheme in the list view by clicking on it a second time.
   */
  private def newSchemeCell(@deprecated("unused","") lv: ListView[Option[Savings.Scheme]]): ListCell[Option[Savings.Scheme]] = {
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
   * Creates a new Fund list view checkbox cell.
   *
   * Cell has the appropriate value displaying.
   * Cell is locked (cannot be deselected) if there is an existing asset for the
   * concerned scheme and fund.
   */
  private def newFundCell(): CheckBoxListCellWithSeparator[SelectableFund] = {
    class CheckBoxFundCell extends CheckBoxListCellWithSeparator[SelectableFund] {

      import CheckBoxListCellWithInfo._

      override def getInfo(item0: Option[SelectableFund]): CellInfo = {
        // Notes:
        // We are only called when there is an actual item.
        // The 'checked' status was set beforehand if applicable.
        val item = item0.get
        CellInfo(
          text = item.fund.name,
          observable = item.check,
          checked = item.check.get,
          locked = edit.exists(scheme => savings.hasAsset(scheme.id, item.fund.id))
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

    new CheckBoxFundCell
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
      val disabled = disabledCheckBox.isSelected
      val comment = Form.textOrNone(commentField.getText)

      val event = savings.createSchemeEvent(name, comment)
      // Note: take into account the fact that flattening events will remove
      // unnecessary events; we can push an 'UpdateScheme' with the 'disabled'
      // state without checking whether it is needed.
      val newEvents = event ::
        Savings.UpdateScheme(event.schemeId, name, comment, disabled) ::
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
      getScheme.foreach { scheme =>
        if (canDeleteScheme(scheme) && confirmSchemeDeletion(scheme)) {
          applyEvents(Savings.DeleteScheme(scheme.id) :: confirmFundsDeletion(scheme))
        }
      }
    }
  }

  private def confirmSchemeDeletion(scheme: Savings.Scheme): Boolean =
    if (!scheme.used) true
    else Dialogs.confirmation(
      owner = Some(stage),
      title = None,
      headerText = Some(Strings.confirmAction),
      contentText = Some(Strings.deleteUsedResource)
    ).contains(ButtonType.OK)

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
      val alert = new Alert(Alert.AlertType.CONFIRMATION, "", ButtonType.OK)
      Stages.initOwner(alert, stage)

      val loader = new FXMLLoader(getClass.getResource("/fxml/select-resources.fxml"), I18N.getResources)
      val root = loader.load[Node]()
      alert.getDialogPane.setContent(root)
      val label = root.lookup("#labelField").asInstanceOf[Label]
      label.setText(Strings.deleteAssociatedFunds)
      val resourcesField = root.lookup("#resourcesField").asInstanceOf[ListView[Option[Savings.Fund]]]
      resourcesField.setCellFactory(_ => new FundCell)
      resourcesField.getSelectionModel.setSelectionMode(SelectionMode.MULTIPLE)
      val entries = Form.buildOptions(funds.filter(!_.disabled), funds.filter(_.disabled))
      resourcesField.setItems(FXCollections.observableList(entries.asJava))
      resourcesField.getSelectionModel.selectAll()

      if (!alert.showAndWait().contains(ButtonType.OK)) Nil
      else resourcesField.getSelectionModel.getSelectedItems.asScala.toList.flatten.map { fund =>
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
        val disabled = disabledCheckBox.isSelected
        val comment = Form.textOrNone(commentField.getText)
        val event1 =
          if (scheme.copy(name = name, comment = comment, disabled = disabled) == scheme) None
          else Some(Savings.UpdateScheme(scheme.id, name, comment, disabled))

        val oldFunds = scheme.funds.toSet
        val newFunds = getSelectedFunds.map(_.id).toSet

        val newEvents = event1.toList ::: (oldFunds -- newFunds).toList.map { fundId =>
          Savings.DissociateFund(scheme.id, fundId)
        } ::: (newFunds -- oldFunds).toList.sorted.map { fundId =>
          Savings.AssociateFund(scheme.id, fundId)
        }

        applyEvents(newEvents)
      }
    }
  }

  /** Handles scheme selection changes. */
  private def onSelectedScheme(): Unit = {
    val newEdit = getScheme

    // Update editing fields if we are selecting a new scheme
    newEdit.filterNot(edit.contains).foreach { scheme =>
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
    updateSchemes()
    // Note: we may delete associated funds, so refresh the list view too.
    updateFunds()
  }

  /**
   * Whether a scheme can be deleted.
   *
   * A scheme can be deleted only if it has no assets.
   */
  private def canDeleteScheme(scheme: Savings.Scheme): Boolean =
    !savings.assets.list.exists(_.schemeId == scheme.id)

  /** Updates the list of schemes. */
  private def updateSchemes(): Unit = {
    schemesField.getSelectionModel.clearSelection()
    val schemes = savings.getSchemes(associated = None)
    val entries = Form.buildOptions(schemes.filter(!_.disabled), schemes.filter(_.disabled))
    schemesField.setItems(FXCollections.observableList(entries.asJava))
  }

  /** Updates the list of funds. */
  private def updateFunds(): Unit = {
    val funds = savings.getFunds(associated = false).map(SelectableFund)
    val entries = Form.buildOptions(funds.filter(!_.fund.disabled), funds.filter(_.fund.disabled))
    fundsField.setItems(FXCollections.observableList(entries.asJava))
  }

  /**
   * Updates editing fields.
   *
   * Upon editing a scheme, sets its name and selects its funds.
   */
  private def updateEditFields(scheme: Savings.Scheme): Unit = {
    nameField.setText(scheme.name)
    disabledCheckBox.setSelected(scheme.disabled)
    disabledCheckBox.setDisable(scheme.active)
    commentField.setText(scheme.comment.orNull)
    // Refresh ListView in order to re-create cells with appropriate content
    // (checkbox selection, etc).
    // Note: we also set the 'checked' status for each item, so that it is
    // properly taken into account (even for the cells not visible in the
    // current view).
    fundsField.getItems.asScala.toList.flatten.foreach { item =>
      item.check.set(scheme.funds.contains(item.fund.id))
    }
    fundsField.refresh()
  }

  /**
   * Resets editing fields.
   *
   * Clears name and resets funds selection.
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
  private def checkForm(): Unit = {
    val name = nameField.getText.trim
    val disabled = disabledCheckBox.isSelected
    val comment = Form.textOrNone(commentField.getText)
    // Edition is OK if either name or funds are changed.
    val editOk = edit.exists { scheme =>
      val newFunds = getSelectedFunds.map(_.id).toSet
      (scheme.copy(name = name, comment = comment, disabled = disabled) != scheme) ||
        scheme.funds.toSet != newFunds
    }
    val exists = savings.schemes.exists(_.name.equalsIgnoreCase(name)) && !edit.exists(_.name.equalsIgnoreCase(name))
    // Selected name is OK if it is not empty and does not already exists.
    val nameOk = !exists && name.nonEmpty

    // Apply name field status: set error style if name is not OK
    JFXStyles.toggleError(nameField, !nameOk,
      if (exists) Some(Strings.nameExists)
      else if (name.isEmpty) Some(Strings.nameEmpty)
      else None
    )

    // Minus field status: enable deletion if selected scheme can be deleted
    getScheme match {
      case None =>
        deleteReady = false
        JFXStyles.toggleImageButton(minusField, active = deleteReady)

      case Some(scheme) =>
        deleteReady = canDeleteScheme(scheme)
        if (deleteReady) JFXStyles.toggleImageButton(minusField, active = deleteReady)
        else JFXStyles.toggleImageButton(minusField, active = deleteReady, Some(Strings.schemeNotEmpty))
    }

    // Plus field status: enable if adding new scheme which name is OK, or
    // copying with non-empty name.
    addReady =
      if (edit.isEmpty) nameOk
      else name.nonEmpty
    JFXStyles.toggleImageButton(plusField, addReady)

    // Tick field status: enable if name and edition are OK
    applyReady = nameOk && editOk
    JFXStyles.toggleImageButton(tickField, applyReady)
  }

  private def getScheme: Option[Savings.Scheme] =
    Option(schemesField.getSelectionModel.getSelectedItem).flatten

  private def getSelectedFunds: List[Savings.Fund] =
    fundsField.getItems.asScala.toList.flatten.filter(_.check.get).map(_.fund)

}

object EditSchemesController {

  private val stageLocation = ConfigEntry.from[StageLocation](Main.settings.settings,
    Settings.prefix ++ Seq(Settings.KEY_STAGE, "edit-schemes", Settings.KEY_LOCATION))

  /** Builds a dialog out of this controller. */
  def buildDialog(owner: Window, savings: Savings, edit: Option[Savings.Scheme]): Dialog[List[Savings.Event]] = {
    val dialog = new Dialog[List[Savings.Event]]()
    val title = Strings.editSchemes
    // Note: initializing owner resets dialog icon, so set the icon afterwards
    Stages.initOwner(dialog, owner)
    Stages.getStage(dialog).getIcons.setAll(Images.iconTables)
    dialog.setTitle(title)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val loader = new FXMLLoader(getClass.getResource("/fxml/edit-schemes.fxml"), I18N.getResources)
    dialog.getDialogPane.setContent(loader.load[Node]())
    val controller = loader.getController[EditSchemesController]
    controller.initialize(savings, dialog, edit)

    Dialogs.addPersistence(dialog, controller)

    dialog.setResultConverter(resultConverter(controller) _)

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
