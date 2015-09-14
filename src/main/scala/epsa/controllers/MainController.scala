package epsa.controllers

import akka.actor.{Actor, ActorRef, Props}
import epsa.I18N
import epsa.charts.ChartHandler
import epsa.model.Savings
import epsa.tools.EsaliaInvestmentFundProber
import java.nio.file.Path
import java.util.ResourceBundle
import javafx.beans.property.{SimpleObjectProperty, SimpleStringProperty}
import javafx.collections.FXCollections
import javafx.collections.transformation.SortedList
import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene}
import javafx.scene.control.{Button, ListView, TableColumn, TableView}
import javafx.stage.{FileChooser, Modality, Stage, WindowEvent}
import javafx.stage.FileChooser.ExtensionFilter
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.event.EventHandler._
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.util.Callback._
import suiryc.scala.settings.Preference

// TODO - add "Edit schemes"/"Edit funds" menu items
// TODO - add mouse context menu over scheme/fund cell in assets table to edit the scheme/fund
// TODO - remove schemes/funds list view in main window
// TODO - remove add/edit scheme/fund buttons in main window
// TODO - move other buttons into menu items
// TODO - change menu for OS integration ? (e.g. Ubuntu)
class MainController {

  import epsa.Main.prefs
  import MainController._

  //@FXML
  //protected var location: URL = _

  @FXML
  protected var resources: ResourceBundle = _

  @FXML
  protected var editSchemeButton: Button = _

  @FXML
  protected var schemesField: ListView[Savings.Scheme] = _

  @FXML
  protected var editFundButton: Button = _

  @FXML
  protected var fundsField: ListView[Savings.Fund] = _

  @FXML
  protected var assetsTable: TableView[Savings.Asset] = _

  private var actor: ActorRef = _

  lazy private val columnScheme =
    new TableColumn[Savings.Asset, String](resources.getString("Scheme"))

  lazy private val columnFund =
    new TableColumn[Savings.Asset, String](resources.getString("Fund"))

  lazy private val columnAmount =
    new TableColumn[Savings.Asset, BigDecimal](resources.getString("Amount"))

  lazy private val columnUnits =
    new TableColumn[Savings.Asset, BigDecimal](resources.getString("Units"))

  //def initialize(): Unit = { }

  def initialize(state: State): Unit = {
    // Note: make the actor name unique (with timestamp) so that it can be
    // recreated later.
    actor = JFXSystem.newJFXActor(
      ControllerActor.props(state),
      s"epsa-main@${System.currentTimeMillis}"
    )

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

    // Note: scheme and fund columns cell value factory relies on the current
    // Savings instance, and are thus defined/updated in the actor
    columnAmount.setCellValueFactory { (data: TableColumn.CellDataFeatures[Savings.Asset, BigDecimal]) =>
      new SimpleObjectProperty(data.getValue.amount)
    }
    columnAmount.setCellFactory { (column: TableColumn[Savings.Asset, BigDecimal]) =>
      new AmountCell[Savings.Asset]
    }
    columnUnits.setCellValueFactory { (data: TableColumn.CellDataFeatures[Savings.Asset, BigDecimal]) =>
      new SimpleObjectProperty(data.getValue.units)
    }

    assetsTable.getColumns.addAll(columnScheme, columnFund, columnAmount, columnUnits)
  }

  def onExit(event: ActionEvent): Unit = {
    actor ! OnExit
  }

  def onOptions(event: ActionEvent): Unit = {
    actor ! OnOptions
  }

  def onCreateScheme(event: ActionEvent): Unit = {
    actor ! OnCreateScheme
  }

  def onEditScheme(event: ActionEvent): Unit = {
    Option(schemesField.getSelectionModel.getSelectedItem).foreach { scheme =>
      actor ! OnEditScheme(scheme)
    }
  }

  def onCreateFund(event: ActionEvent): Unit = {
    actor ! OnCreateFund
  }

  def onEditFund(event: ActionEvent): Unit = {
    Option(fundsField.getSelectionModel.getSelectedItem).foreach { fund =>
      actor ! OnEditFund(fund)
    }
  }

  def onTest(event: ActionEvent): Unit = {
    actor ! OnTest
  }

  def onFundGraph(event: ActionEvent): Unit = {
    actor ! OnFundGraph
  }

  object ControllerActor {
    def props(state: State) = Props(new ControllerActor(state))
  }

  class ControllerActor(state0: State) extends Actor {

    // Cheap trick to fill fields with Savings data
    processEvents(state0, Nil)

    override def receive: Receive = receive(state0)

    def receive(state: State): Receive = {
      case OnExit               => onExit(state)
      case OnOptions            => onOptions(state)
      case OnCreateScheme       => onCreateScheme(state, None)
      case OnEditScheme(scheme) => onCreateScheme(state, Some(scheme))
      case OnCreateFund         => onCreateFund(state, None)
      case OnEditFund(fund)     => onCreateFund(state, Some(fund))
      case OnTest               => onTest(state)
      case OnFundGraph          => onFundGraph(state)
    }

    def processEvents(state: State, events: List[Savings.Event]): Unit = {
      val newSavings = Savings.processEvents(state.savings, events)

      // We use the savings instance to get a scheme/fund name by id in the
      // assets table
      columnScheme.setCellValueFactory { (data: TableColumn.CellDataFeatures[Savings.Asset, String]) =>
        new SimpleStringProperty(newSavings.getScheme(data.getValue.schemeId).name)
      }
      columnFund.setCellValueFactory { (data: TableColumn.CellDataFeatures[Savings.Asset, String]) =>
        new SimpleStringProperty(newSavings.getFund(data.getValue.fundId).name)
      }

      import scala.collection.JavaConversions._
      fundsField.setItems(FXCollections.observableList(newSavings.funds))
      schemesField.setItems(FXCollections.observableList(newSavings.schemes))

      val sortedAssets = new SortedList(FXCollections.observableList(newSavings.assets))
      sortedAssets.comparatorProperty.bind(assetsTable.comparatorProperty)
      assetsTable.setItems(sortedAssets)

      context.become(receive(state.copy(savings = newSavings)))
    }

    def onExit(state: State): Unit = {
      context.stop(self)
      epsa.Main.shutdown(state.stage)
    }

    def onOptions(state: State): Unit = {
      val dialog = OptionsController.buildDialog()
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.initOwner(state.window)
      dialog.setResizable(true)
      val reload = dialog.showAndWait().orElse(false)
      if (reload) {
        context.stop(self)
        MainController.build(state)
      }
    }

    def onCreateScheme(state: State, edit: Option[Savings.Scheme]): Unit = {
      val dialog = EditSchemesController.buildDialog(state.savings, edit)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.initOwner(state.window)
      dialog.setResizable(true)
      val events = dialog.showAndWait().orElse(Nil)
      processEvents(state, events)
    }

    def onCreateFund(state: State, edit: Option[Savings.Fund]): Unit = {
      val dialog = EditFundsController.buildDialog(state.savings, edit)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.initOwner(state.window)
      dialog.setResizable(true)
      val events = dialog.showAndWait().orElse(Nil)
      processEvents(state, events)
    }

    def onTest(state: State): Unit = {
      import java.time.LocalDate

      val s1 = Savings().createSchemeEvent("Scheme 1")
      val s2 = Savings().createSchemeEvent("Scheme 2")
      val f1 = Savings().createFundEvent("Fund 1")
      val f2 = Savings().createFundEvent("Fund 2")
      processEvents(state, List(s1, s2, f1, f2,
        Savings.AssociateFund(s1.schemeId, f1.fundId),
        Savings.AssociateFund(s1.schemeId, f2.fundId),
        Savings.AssociateFund(s2.schemeId, f2.fundId),
        Savings.MakePayment(LocalDate.now, Savings.Asset(s1.schemeId, f1.fundId, None, 10.0, 10.0)),
        Savings.MakePayment(LocalDate.now, Savings.Asset(s1.schemeId, f1.fundId, None, 20.0, 10.0)),
        Savings.MakePayment(LocalDate.now, Savings.Asset(s1.schemeId, f1.fundId, Some(LocalDate.now.plusMonths(12)), 20.0, 10.0)),
        Savings.MakeRefund(LocalDate.now, Savings.Asset(s1.schemeId, f1.fundId, None, 10.0, 5.0)),
        Savings.MakeTransfer(LocalDate.now, Savings.Asset(s1.schemeId, f1.fundId, None, 10.0, 10.0), Savings.Asset(s1.schemeId, f2.fundId, None, 10.0, 20.0))
      ))
    }

    def onFundGraph(state: State): Unit = {
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
          stage.initOwner(state.window)
          stage.show()

        case None =>
      }
    }

  }

}

object MainController {

  case class State(stage: Stage, savings: Savings) {
    lazy val window = stage.getScene.getWindow
  }

  case object OnExit

  case object OnOptions

  case object OnCreateScheme

  case class OnEditScheme(scheme: Savings.Scheme)

  case object OnCreateFund

  case class OnEditFund(fund: Savings.Fund)

  case object OnTest

  case object OnFundGraph

  def build(state: State): Unit = {
    val stage = state.stage

    val loader = new FXMLLoader(getClass.getResource("/fxml/main.fxml"), I18N.getResources)
    val root = loader.load[Parent]()
    val controller = loader.getController[MainController]
    controller.initialize(state)

    /*pane.getChildren.setAll(root)
    AnchorPane.setTopAnchor(root, 0)
    AnchorPane.setRightAnchor(root, 0)
    AnchorPane.setBottomAnchor(root, 0)
    AnchorPane.setLeftAnchor(root, 0)*/

    Option(stage.getScene) match {
      case Some(scene) =>
        stage.hide()

      case None =>
        stage.setOnCloseRequest(onCloseRequest(controller) _)
    }

    stage.setScene(new Scene(root))
    stage.show()

    Stages.trackMinimumDimensions(stage)
  }

  private def onCloseRequest(controller: MainController)(event: WindowEvent): Unit =
    // Delegate closing request to controller
    controller.onExit(new ActionEvent())

}
