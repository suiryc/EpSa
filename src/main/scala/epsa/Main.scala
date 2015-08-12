package epsa

import akka.actor.{Actor, ActorRef, Props}
import java.io.File
import java.util.prefs.Preferences
import java.util.Locale
import javafx.application.{Platform, Application}
import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.{ButtonType, Dialog, ListView, TextField}
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

  private var actor: ActorRef = _

  def initialize(): Unit = {
    // XXX - append random value
    // XXX - terminate actor upon leaving
    actor = JFXSystem.newJFXActor(ControllerActor.props(new Savings()), "epsa-main")
  }

  def onCreateScheme(event: ActionEvent): Unit = {
    actor ! OnCreateScheme(event.getSource.asInstanceOf[Node].getScene.getWindow)
  }

  def onEditScheme(event: ActionEvent): Unit = {
  }

  def onCreateFund(event: ActionEvent): Unit = {
  }

  def onEditFund(event: ActionEvent): Unit = {
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
      case OnCreateScheme(owner) => onCreateScheme(savings, owner)
    }

    def onCreateScheme(savings: Savings, owner: Window): Unit = {
      val dialog = CreateSchemeController.buildDialog(savings)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.initOwner(owner)
      val events = dialog.showAndWait().orElse(Nil)
      context.become(receive(Savings.processEvents(savings, events:_*)))
    }

  }

}

object MainController {

  case class OnCreateScheme(owner: Window)

}

class CreateSchemeController {

  //@FXML
  //protected var location: URL = _

  //@FXML
  //protected var resources: ResourceBundle = _

  @FXML
  protected var nameField: TextField = _

  @FXML
  protected var fundsField: ListView[String] = _

  protected var savings: Savings = _

  protected var buttonOk: Node = _

  //def initialize(): Unit = { }

  def initialize(savings: Savings, dialog: Dialog[_]): Unit = {
    this.savings = savings
    buttonOk = dialog.getDialogPane.lookupButton(ButtonType.OK)
    buttonOk.setDisable(true)
    nameField.textProperty.listen {
      // XXX - set green/red icon (with tooltip) to show which parameters are ok or not
      val name = nameField.getText
      val exists = savings.schemes.exists(_.name.equalsIgnoreCase(name))
      // XXX - also check for funds (at least one selected ?) / or are funds created after schemes ?
      buttonOk.setDisable(exists || name.isEmpty)
    }
    nameField.requestFocus()
  }

}

object CreateSchemeController {

  def buildDialog(savings: Savings): Dialog[List[Savings.Event]] = {
    val dialog = new Dialog[List[Savings.Event]]()
    dialog.setTitle("Create Scheme")
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val loader = new FXMLLoader(getClass.getResource("/fxml/scheme-create.fxml"))
    dialog.getDialogPane.setContent(loader.load())
    val controller = loader.getController[CreateSchemeController]
    controller.initialize(savings, dialog)

    dialog.setResultConverter(resultConverter(savings, controller) _)

    dialog
  }

  def resultConverter(savings: Savings, controller: CreateSchemeController)(buttonType: ButtonType): List[Savings.Event] = {
    if (buttonType != ButtonType.OK) Nil
    else {
      List(
        savings.createScheme(controller.nameField.getText)
      )
      // XXX - manage funds
      //controller.fundsField
    }
  }

}
