package epsa.controllers

import akka.actor.{Actor, ActorRef, Props}
import epsa.charts.ChartHandler
import epsa.model.Savings
import epsa.tools.EsaliaInvestmentFundProber
import java.nio.file.Path
import javafx.collections.FXCollections
import javafx.event.ActionEvent
import javafx.fxml.FXML
import javafx.scene.{Node, Scene}
import javafx.scene.control.{Button, ListView}
import javafx.stage.FileChooser.ExtensionFilter
import javafx.stage.{FileChooser, Modality, Stage, Window}
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.util.Callback._
import suiryc.scala.settings.Preference

class MainController {

  import epsa.Main.prefs
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

  def onTest(event: ActionEvent): Unit = {
    actor ! OnTest
  }

  def onFundGraph(event: ActionEvent): Unit = {
    import Preference._

    val stage = new Stage()

    val fundPathFolder = Preference.from("fund.path.folder", null:Path)
    val fundPathFile = Preference.from("fund.path.file", null:String)

    val fileChooser = new FileChooser()
    fileChooser.setTitle("Open Investment Fund File")
    fileChooser.getExtensionFilters.addAll(
      new ExtensionFilter("Excel Files", "*.xls", "*.xlsx"),
      new ExtensionFilter("All Files", "*.*")
    )
    fundPathFolder.option.foreach { path =>
      fileChooser.setInitialDirectory(path.toFile)
      fundPathFile.option.foreach(fileChooser.setInitialFileName)
    }
    val selectedFile = fileChooser.showOpenDialog(stage)
    Option(selectedFile).flatMap { file =>
      EsaliaInvestmentFundProber.probe(file.toPath)
    } match {
      case Some(fund) =>
        // Save path in preferences
        fundPathFolder() = selectedFile.getParentFile.toPath
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
      case OnCreateScheme(owner)       => onCreateScheme(savings, owner, None)
      case OnEditScheme(owner, scheme) => onCreateScheme(savings, owner, Some(scheme))
      case OnCreateFund(owner)         => onCreateFund(savings, owner, None)
      case OnEditFund(owner, fund)     => onCreateFund(savings, owner, Some(fund))
      case OnTest                      => onTest(savings)
    }

    def processEvents(savings: Savings, events: List[Savings.Event]): Unit = {
      val newSavings = Savings.processEvents(savings, events)

      import scala.collection.JavaConversions._
      fundsField.setItems(FXCollections.observableList(newSavings.funds))
      schemesField.setItems(FXCollections.observableList(newSavings.schemes))

      context.become(receive(newSavings))
    }

    def onCreateScheme(savings: Savings, owner: Window, edit: Option[Savings.Scheme]): Unit = {
      val dialog = CreateSchemeController.buildDialog(savings, edit)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.initOwner(owner)
      dialog.setResizable(true)
      val events = dialog.showAndWait().orElse(Nil)
      processEvents(savings, events)
    }

    def onCreateFund(savings: Savings, owner: Window, edit: Option[Savings.Fund]): Unit = {
      val dialog = CreateFundController.buildDialog(savings, edit)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.initOwner(owner)
      dialog.setResizable(true)
      val events = dialog.showAndWait().orElse(Nil)
      processEvents(savings, events)
    }

    def onTest(savings: Savings): Unit = {
      val savings1 = Savings.processActions(savings,
        _.createSchemeEvent("Atos"),
        _.createSchemeEvent("Worldline"),
        _.createFundEvent("ARCANCIA MONETAIRE 257 - 2007"),
        _.createFundEvent("ARCANCIA TEMPERE 357 - 2357"),
        _.createFundEvent("ARCANCIA HARMONIE 453 - 2453"),
        _.createFundEvent("SPRINT DYNAMIC - 6010"),
        _.createFundEvent("WORLDLINE STOCK PLAN PART C - 868")
      )
      processEvents(savings1, Nil)
    }

  }

}

object MainController {

  case class OnCreateScheme(owner: Window)

  case class OnEditScheme(owner: Window, scheme: Savings.Scheme)

  case class OnCreateFund(owner: Window)

  case class OnEditFund(owner: Window, fund: Savings.Fund)

  case object OnTest

}
