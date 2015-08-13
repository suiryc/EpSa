package epsa

import javafx.collections.FXCollections

import akka.actor.{Actor, ActorRef, Props}
import java.io.File
import java.util.Locale
import java.util.prefs.Preferences
import javafx.application.{Application, Platform}
import javafx.css.PseudoClass
import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control._
import javafx.scene.{Node, Scene, Parent}
import javafx.stage.FileChooser.ExtensionFilter
import javafx.stage.{Modality, FileChooser, Stage, Window, WindowEvent}
import suiryc.scala.javafx.beans.property.RichReadOnlyProperty._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.event.EventHandler._
import suiryc.scala.javafx.util.Callback._
import suiryc.scala.settings.Preference

object Main {

  /* Set locale to english as application is not i18n */
  Locale.setDefault(Locale.ENGLISH)

  var stage: Stage = _

  /** Settings. */
  implicit val prefs = Preferences.userRoot.node("suiryc.epsa").node("epsa")

  def main(args: Array[String]): Unit = {
    (new Main).launch()
  }

}

class Main extends Application {

  import Main._

  def launch(): Unit = {
    Application.launch()
  }

  override def start(primaryStage: Stage) {
    stage = primaryStage

    stage.setOnCloseRequest(onCloseRequest _)

    val root = FXMLLoader.load[Parent](getClass.getResource("/fxml/main.fxml"))

    /*pane.getChildren.setAll(root)
    AnchorPane.setTopAnchor(root, 0)
    AnchorPane.setRightAnchor(root, 0)
    AnchorPane.setBottomAnchor(root, 0)
    AnchorPane.setLeftAnchor(root, 0)*/

    stage.setScene(new Scene(root))

    stage.setTitle("EpSa")
    stage.show()
  }

  private def onCloseRequest(event: WindowEvent): Unit = {
    stage.close()
    Platform.exit()
  }

}

class MainController {

  import Main.prefs
  import MainController._

  //@FXML
  //protected var location: URL = _

  //@FXML
  //protected var resources: ResourceBundle = _

  @FXML
  protected var editSchemeButton: Button = _

  @FXML
  protected var schemesField: ListView[Savings.Scheme] = _

  @FXML
  protected var editFundButton: Button = _

  @FXML
  protected var fundsField: ListView[Savings.Fund] = _

  private var actor: ActorRef = _

  def initialize(): Unit = {
    // XXX - append random value
    // XXX - terminate actor upon leaving
    actor = JFXSystem.newJFXActor(ControllerActor.props(new Savings()), "epsa-main")

    schemesField.setCellFactory { (lv: ListView[Savings.Scheme]) =>
      new SchemeCell
    }
    editSchemeButton.setDisable(true)
    schemesField.getSelectionModel.selectedItemProperty.listen {
      editSchemeButton.setDisable(Option(schemesField.getSelectionModel.getSelectedItem).isEmpty)
    }

    fundsField.setCellFactory { (lv: ListView[Savings.Fund]) =>
      new FundCell
    }
    editFundButton.setDisable(true)
    fundsField.getSelectionModel.selectedItemProperty.listen {
      editFundButton.setDisable(Option(fundsField.getSelectionModel.getSelectedItem).isEmpty)
    }
  }

  def onCreateScheme(event: ActionEvent): Unit = {
    actor ! OnCreateScheme(event.getSource.asInstanceOf[Node].getScene.getWindow)
  }

  def onEditScheme(event: ActionEvent): Unit = {
    Option(schemesField.getSelectionModel.getSelectedItem).foreach { scheme =>
      actor ! OnEditScheme(event.getSource.asInstanceOf[Node].getScene.getWindow, scheme)
    }
  }

  def onCreateFund(event: ActionEvent): Unit = {
    actor ! OnCreateFund(event.getSource.asInstanceOf[Node].getScene.getWindow)
  }

  def onEditFund(event: ActionEvent): Unit = {
    Option(fundsField.getSelectionModel.getSelectedItem).foreach { fund =>
      actor ! OnEditFund(event.getSource.asInstanceOf[Node].getScene.getWindow, fund)
    }
  }

  def onFundGraph(event: ActionEvent): Unit = {
    val stage = new Stage()

    val fundPathFolder = Preference.forString("fund.path.folder", null)
    val fundPathFile = Preference.forString("fund.path.file", null)

    val fileChooser = new FileChooser()
    fileChooser.setTitle("Open Investment Fund File")
    fileChooser.getExtensionFilters.addAll(
      new ExtensionFilter("Excel Files", "*.xls", "*.xlsx"),
      new ExtensionFilter("All Files", "*.*")
    )
    fundPathFolder.option.foreach { folder =>
      fileChooser.setInitialDirectory(new File(folder))
      fundPathFile.option.foreach(fileChooser.setInitialFileName)
    }
    val selectedFile = fileChooser.showOpenDialog(stage)
    Option(selectedFile).flatMap { file =>
      EsaliaInvestmentFundProber.probe(file.toPath)
    } match {
      case Some(fund) =>
        // Save path in preferences
        fundPathFolder() = selectedFile.getParent
        fundPathFile() = selectedFile.getName
        // Then build and display chart
        val chartHandler = new ChartHandler(fund)
        val chartPane = chartHandler.chartPane
        chartPane.setPrefSize(640, 480)
        val scene = new Scene(chartPane)
        stage.setScene(scene)
        stage.initModality(Modality.WINDOW_MODAL)
        stage.initOwner(event.getSource.asInstanceOf[Node].getScene.getWindow)
        stage.show()

      case None =>
    }
  }

  object ControllerActor {
    def props(savings: Savings) = Props(new ControllerActor(savings))
  }

  class ControllerActor(_savings: Savings) extends Actor {

    override def receive: Receive = receive(_savings)

    def receive(savings: Savings): Receive = {
      case OnCreateScheme(owner) => onCreateScheme(savings, owner, None)
      case OnEditScheme(owner, scheme) => onCreateScheme(savings, owner, Some(scheme))
      case OnCreateFund(owner) => onCreateFund(savings, owner, None)
      case OnEditFund(owner, fund) => onCreateFund(savings, owner, Some(fund))
    }

    def processEvents(savings: Savings, events: List[Savings.Event]): Unit = {
      val newSavings = Savings.processEvents(savings, events:_*)

      import scala.collection.JavaConversions._
      fundsField.setItems(FXCollections.observableList(newSavings.funds))
      schemesField.setItems(FXCollections.observableList(newSavings.schemes))

      context.become(receive(newSavings))
    }

    def onCreateScheme(savings: Savings, owner: Window, edit: Option[Savings.Scheme]): Unit = {
      val dialog = CreateSchemeController.buildDialog(savings, edit)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.initOwner(owner)
      val events = dialog.showAndWait().orElse(Nil)
      processEvents(savings, events)
    }

    def onCreateFund(savings: Savings, owner: Window, edit: Option[Savings.Fund]): Unit = {
      val dialog = CreateFundController.buildDialog(savings, edit)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.initOwner(owner)
      val events = dialog.showAndWait().orElse(Nil)
      processEvents(savings, events)
    }

  }

}

object MainController {

  case class OnCreateScheme(owner: Window)

  case class OnEditScheme(owner: Window, scheme: Savings.Scheme)

  case class OnCreateFund(owner: Window)

  case class OnEditFund(owner: Window, fund: Savings.Fund)

}

object Form {

  val errorClass = PseudoClass.getPseudoClass("error")

  def setStyleError(node: Node, set: Boolean): Unit =
  // See: http://stackoverflow.com/a/24231728
    node.pseudoClassStateChanged(errorClass, set)

}

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
    dialog.getDialogPane.getStylesheets.add(getClass.getResource("form.css").toExternalForm)
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
    }
    // XXX - handle 'Map/Set/Array/List'ChangeListener in suiryc-scala
    fundsField.getSelectionModel.getSelectedItems.listen(checkForm())

    checkForm()

    nameField.requestFocus()
  }

  def checkForm(): Unit = {
    val name = nameField.getText
    val editOk = edit.map { scheme =>
      import scala.collection.JavaConversions._

      val newFunds = fundsField.getSelectionModel.getSelectedItems.toList.map(_.id).toSet
      println(scheme)
      println(newFunds)
      (scheme.name != name) ||
        scheme.funds.toSet.diff(newFunds).nonEmpty
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

class FundCell
  extends ListCell[Savings.Fund]
{

  override protected def updateItem(item: Savings.Fund, empty: Boolean) {
    super.updateItem(item, empty)
    if (empty) setText(null)
    else setText(item.name)
  }

}

class CreateFundController {

  //@FXML
  //protected var location: URL = _

  //@FXML
  //protected var resources: ResourceBundle = _

  @FXML
  protected var nameField: TextField = _

  @FXML
  protected var schemesField: ListView[Savings.Scheme] = _

  protected var savings: Savings = _

  protected var buttonOk: Node = _

  //def initialize(): Unit = { }

  def initialize(savings: Savings, dialog: Dialog[_], edit: Option[Savings.Fund]): Unit = {
    this.savings = savings
    dialog.getDialogPane.getStylesheets.add(getClass.getResource("form.css").toExternalForm)
    buttonOk = dialog.getDialogPane.lookupButton(ButtonType.OK)
    buttonOk.setDisable(true)
    nameField.textProperty.listen(checkForm())
    checkName()

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
      nameField.setTooltip(new Tooltip("Name already exists"))
    } else if (name.isEmpty) {
      nameField.setTooltip(new Tooltip("Name cannot be empty"))
    } else {
      nameField.setTooltip(null)
    }
    Form.setStyleError(nameField, !nameOk)
    buttonOk.setDisable(!nameOk)
  }

}

object CreateFundController {

  def buildDialog(savings: Savings, edit: Option[Savings.Fund]): Dialog[List[Savings.Event]] = {
    val dialog = new Dialog[List[Savings.Event]]()
    dialog.setTitle(s"${if (edit.isDefined) "Edit" else "Create"} Fund")
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val loader = new FXMLLoader(getClass.getResource("/fxml/fund-create.fxml"))
    dialog.getDialogPane.setContent(loader.load())
    val controller = loader.getController[CreateFundController]
    controller.initialize(savings, dialog, edit)

    dialog.setResultConverter(resultConverter(savings, controller) _)

    dialog
  }

  def resultConverter(savings: Savings, controller: CreateFundController)(buttonType: ButtonType): List[Savings.Event] = {
    import scala.collection.JavaConversions._

    if (buttonType != ButtonType.OK) Nil
    else {
      val createFund = savings.createFund(controller.nameField.getText)
      List(
        createFund
      ) ::: controller.schemesField.getSelectionModel.getSelectedItems.toList.map { scheme =>
        savings.associateFund(scheme.id, createFund.fundId)
      }
    }
  }

}

class SchemeCell
  extends ListCell[Savings.Scheme]
{

  override protected def updateItem(item: Savings.Scheme, empty: Boolean) {
    super.updateItem(item, empty)
    if (empty) setText(null)
    else setText(item.name)
  }

}
