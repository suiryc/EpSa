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
    savings = savings0
    edit = edit0
    dialog.getDialogPane.getStylesheets.add(getClass.getResource("/css/form.css").toExternalForm)
    buttonOk = dialog.getDialogPane.lookupButton(ButtonType.OK)
    buttonOk.setDisable(true)
    nameField.textProperty.listen(checkForm())

    schemesField.setCellFactory { (lv: ListView[Savings.Scheme]) =>
      newSchemeCell(lv)
    }
    updateSchemes()
    schemesField.getSelectionModel.selectedItemProperty.listen(checkForm())

    events.listen { events =>
      buttonOk.setDisable(events.isEmpty)
    }

    fundsField.setCellFactory { (lv: ListView[Savings.Fund]) =>
      new FundCell
    }
    fundsField.getSelectionModel.setSelectionMode(SelectionMode.MULTIPLE)
    fundsField.setItems(FXCollections.observableList(savings.funds))

    updateEditFields()
    fundsField.getSelectionModel.getSelectedItems.listen(checkForm())

    checkForm()

    nameField.requestFocus()
  }

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

  private def isEventOnNode(event: Event): Boolean =
    event match {
      case event: MouseEvent => event.getTarget.asInstanceOf[Node].contains(event.getX, event.getY)
      case _ => true
    }

  private def newSchemeCell(lv: ListView[Savings.Scheme]): ListCell[Savings.Scheme] = {
    import suiryc.scala.javafx.event.EventHandler._

    // See: http://stackoverflow.com/questions/23622703/deselect-an-item-on-an-javafx-listview-on-click
    val cell = new SchemeCell
    cell.addEventFilter(MouseEvent.MOUSE_PRESSED, { (event: MouseEvent) =>
      if (schemesField.getSelectionModel.getSelectedIndices.contains(cell.getIndex)) {
        edit = None
        schemesField.getSelectionModel.clearSelection(cell.getIndex)
        checkForm()
      }
      else if (cell.getItem != null) {
        edit = Option(cell.getItem)
        schemesField.getSelectionModel.select(cell.getIndex)
        nameField.setText(cell.getItem.name)
        updateEditFields()
        checkForm()
      }
      event.consume()
      if (!schemesField.isFocused) schemesField.requestFocus()
    })

    cell
  }

  private def applyEvents(newEvents: Savings.Event*): Unit = {
    resetEditFields()
    savings = Savings.processEvents(savings, newEvents:_*)
    events.setValue(flattenEvents(events.getValue ++ newEvents))
    updateSchemes()
  }

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

  private def canDeleteScheme(scheme: Savings.Scheme): Boolean =
    !savings.assets.exists(_.schemeId == scheme.id)

  private def updateSchemes(): Unit = {
    schemesField.getSelectionModel.clearSelection()
    schemesField.setItems(FXCollections.observableList(savings.schemes))
  }

  private def updateEditFields(): Unit =
    edit.foreach { scheme =>
      nameField.setText(scheme.name)
      fundsField.getSelectionModel.clearSelection()
      scheme.funds.map { fundId =>
        savings.getFund(fundId)
      }.foreach(fundsField.getSelectionModel.select)
    }

  private def resetEditFields(): Unit = {
    nameField.clear()
    fundsField.getSelectionModel.clearSelection()
  }

  private def checkForm(): Unit = {
    val name = nameField.getText
    val editOk = edit.exists { scheme =>
      import scala.collection.JavaConversions._

      val newFunds = fundsField.getSelectionModel.getSelectedItems.toList.map(_.id).toSet
      (scheme.name != name) ||
        scheme.funds.toSet != newFunds
    }
    val exists = savings.schemes.exists(_.name.equalsIgnoreCase(name)) && !edit.exists(_.name.equalsIgnoreCase(name))
    val nameOk = !exists && name.nonEmpty
    if (exists) {
      nameField.setTooltip(new Tooltip(resources.getString("Name already exists")))
    } else if (name.isEmpty) {
      nameField.setTooltip(new Tooltip(resources.getString("Name cannot be empty")))
    } else {
      nameField.setTooltip(null)
    }
    Form.setStyleError(nameField, !nameOk)

    Option(schemesField.getSelectionModel.getSelectedItem) match {
      case None =>
        toggleImageButton(minusField, set = false)

      case Some(scheme) =>
        if (canDeleteScheme(scheme)) toggleImageButton(minusField, set = true)
        else toggleImageButton(minusField, set = false, Some(resources.getString("Scheme is not empty")))
    }

    val addOk =
      if (schemesField.getSelectionModel.isEmpty) nameOk
      else name.nonEmpty
    toggleImageButton(plusField, addOk)

    toggleImageButton(tickField, nameOk && editOk)
  }

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

  def resultConverter(savings: Savings, edit: Option[Savings.Scheme], controller: EditSchemesController)(buttonType: ButtonType): List[Savings.Event] = {
    if (buttonType != ButtonType.OK) Nil
    else controller.events.getValue
  }

}
