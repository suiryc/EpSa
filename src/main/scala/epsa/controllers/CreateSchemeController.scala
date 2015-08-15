package epsa.controllers

import epsa.model.Savings
import javafx.collections.FXCollections
import javafx.fxml.{FXMLLoader, FXML}
import javafx.scene.Node
import javafx.scene.control._
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.collections.RichObservableList._
import suiryc.scala.javafx.util.Callback._

class CreateSchemeController {

  //@FXML
  //protected var location: URL = _

  //@FXML
  //protected var resources: ResourceBundle = _

  @FXML
  protected var nameField: TextField = _

  @FXML
  protected var fundsField: ListView[Savings.Fund] = _

  protected var savings: Savings = _

  protected var edit: Option[Savings.Scheme] = None

  protected var buttonOk: Node = _

  //def initialize(): Unit = { }

  def initialize(savings: Savings, dialog: Dialog[_], edit: Option[Savings.Scheme]): Unit = {
    this.savings = savings
    this.edit = edit
    dialog.getDialogPane.getStylesheets.add(getClass.getResource("/css/form.css").toExternalForm)
    buttonOk = dialog.getDialogPane.lookupButton(ButtonType.OK)
    buttonOk.setDisable(true)
    nameField.textProperty.listen(checkForm())

    fundsField.setCellFactory { (lv: ListView[Savings.Fund]) =>
      new FundCell
    }
    fundsField.getSelectionModel.setSelectionMode(SelectionMode.MULTIPLE)
    import scala.collection.JavaConversions._
    fundsField.setItems(FXCollections.observableList(savings.funds))

    edit.foreach { scheme =>
      nameField.setText(scheme.name)
      scheme.funds.flatMap { fundId =>
        savings.getFund(fundId)
      }.foreach(fundsField.getSelectionModel.select)

      fundsField.getSelectionModel.getSelectedItems.listen(checkForm())
    }

    checkForm()

    nameField.requestFocus()
  }

  def checkForm(): Unit = {
    val name = nameField.getText
    val editOk = edit.map { scheme =>
      import scala.collection.JavaConversions._

      val newFunds = fundsField.getSelectionModel.getSelectedItems.toList.map(_.id).toSet
      (scheme.name != name) ||
        scheme.funds.toSet != newFunds
    }.getOrElse(true)
    val exists = savings.schemes.exists(_.name.equalsIgnoreCase(name)) && !edit.exists(_.name.equalsIgnoreCase(name))
    val nameOk = !exists && name.nonEmpty
    if (exists) {
      nameField.setTooltip(new Tooltip("Name already exists"))
    } else if (name.isEmpty) {
      nameField.setTooltip(new Tooltip("Name cannot be empty"))
    } else {
      nameField.setTooltip(null)
    }
    Form.setStyleError(nameField, !nameOk)
    buttonOk.setDisable(!nameOk || !editOk)
  }

}

object CreateSchemeController {

  def buildDialog(savings: Savings, edit: Option[Savings.Scheme]): Dialog[List[Savings.Event]] = {
    val dialog = new Dialog[List[Savings.Event]]()
    dialog.setTitle(s"${if (edit.isDefined) "Edit" else "Create"} Scheme")
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val loader = new FXMLLoader(getClass.getResource("/fxml/scheme-create.fxml"))
    dialog.getDialogPane.setContent(loader.load())
    val controller = loader.getController[CreateSchemeController]
    controller.initialize(savings, dialog, edit)

    dialog.setResultConverter(resultConverter(savings, controller) _)

    dialog
  }

  def resultConverter(savings: Savings, controller: CreateSchemeController)(buttonType: ButtonType): List[Savings.Event] = {
    import scala.collection.JavaConversions._

    if (buttonType != ButtonType.OK) Nil
    else {
      val createScheme = savings.createScheme(controller.nameField.getText)
      List(
        createScheme
      ) ::: controller.fundsField.getSelectionModel.getSelectedItems.toList.flatMap { fund =>
        val createFund = savings.createFund(fund.name)
        val associateFund = savings.associateFund(createScheme.schemeId, createFund.fundId)
        List(createFund, associateFund)
      }
    }
  }

}
