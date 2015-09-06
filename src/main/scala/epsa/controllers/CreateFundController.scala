package epsa.controllers

import epsa.I18N
import epsa.model.Savings
import java.util.ResourceBundle
import javafx.collections.FXCollections
import javafx.fxml.{FXMLLoader, FXML}
import javafx.scene.Node
import javafx.scene.control._
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.util.Callback._

class CreateFundController {

  //@FXML
  //protected var location: URL = _

  @FXML
  protected var resources: ResourceBundle = _

  @FXML
  protected var nameField: TextField = _

  @FXML
  protected var schemesField: ListView[Savings.Scheme] = _

  protected var savings: Savings = _

  protected var buttonOk: Node = _

  //def initialize(): Unit = { }

  def initialize(savings: Savings, dialog: Dialog[_], edit: Option[Savings.Fund]): Unit = {
    this.savings = savings
    dialog.getDialogPane.getStylesheets.add(getClass.getResource("/css/form.css").toExternalForm)
    buttonOk = dialog.getDialogPane.lookupButton(ButtonType.OK)
    buttonOk.setDisable(true)
    nameField.textProperty.listen(checkForm())
    checkForm()

    schemesField.setCellFactory { (lv: ListView[Savings.Scheme]) =>
      new SchemeCell
    }
    schemesField.getSelectionModel.setSelectionMode(SelectionMode.MULTIPLE)
    import scala.collection.JavaConversions._
    schemesField.setItems(FXCollections.observableList(savings.schemes))

    edit.foreach { fund =>
      nameField.setText(fund.name)
      savings.schemes.filter(_.funds.contains(fund.id)).foreach(schemesField.getSelectionModel.select)
    }

    nameField.requestFocus()
  }

  def checkForm(): Unit = {
    val name = nameField.getText
    val exists = savings.funds.exists(_.name.equalsIgnoreCase(name))
    val nameOk = !exists && name.nonEmpty
    if (exists) {
      nameField.setTooltip(new Tooltip(resources.getString("Name already exists")))
    } else if (name.isEmpty) {
      nameField.setTooltip(new Tooltip(resources.getString("Name cannot be empty")))
    } else {
      nameField.setTooltip(null)
    }
    Form.setStyleError(nameField, !nameOk)
    buttonOk.setDisable(!nameOk)
  }

}

object CreateFundController {

  def buildDialog(savings: Savings, edit: Option[Savings.Fund]): Dialog[List[Savings.Event]] = {
    val resources = I18N.getResources

    val dialog = new Dialog[List[Savings.Event]]()
    val title = resources.getString(s"${if (edit.isDefined) "Edit" else "Add"} fund")
    dialog.setTitle(title)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val loader = new FXMLLoader(getClass.getResource("/fxml/fund-create.fxml"), resources)
    dialog.getDialogPane.setContent(loader.load())
    val controller = loader.getController[CreateFundController]
    controller.initialize(savings, dialog, edit)

    dialog.setResultConverter(resultConverter(savings, edit, controller) _)
    Stages.trackMinimumDimensions(Stages.getStage(dialog))

    dialog
  }

  def resultConverter(savings: Savings, edit: Option[Savings.Fund], controller: CreateFundController)(buttonType: ButtonType): List[Savings.Event] = {
    import scala.collection.JavaConversions._

    if (buttonType != ButtonType.OK) Nil
    else edit.map { edit =>
      val name = controller.nameField.getText
      val event1 =
        if (name == edit.name) None
        else Some(Savings.UpdateFund(edit.id, name))

      val oldSchemes = savings.schemes.filter(_.funds.contains(edit.id)).map(_.id).toSet
      val newSchemes = controller.schemesField.getSelectionModel.getSelectedItems.toList.map(_.id).toSet

      event1.toList ++ (oldSchemes -- newSchemes).toList.map { schemeId =>
        Savings.DissociateFund(schemeId, edit.id)
      } ++ (newSchemes -- oldSchemes).toList.sorted.map { schemeId =>
        Savings.AssociateFund(schemeId, edit.id)
      }
    }.getOrElse {
      val event = savings.createFundEvent(controller.nameField.getText)
      event :: controller.schemesField.getSelectionModel.getSelectedItems.toList.map { scheme =>
        Savings.AssociateFund(scheme.id, event.fundId)
      }
    }
  }

}
