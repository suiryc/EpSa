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
import javafx.stage.{FileChooser, Modality, Stage, Window, WindowEvent}
import javafx.stage.FileChooser.ExtensionFilter
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.event.EventHandler._
import suiryc.scala.javafx.util.Callback._
import suiryc.scala.settings.Preference

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

  private var stage: Stage = _

  private lazy val window = stage.getScene.getWindow

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

  def initialize(stage: Stage, savings: Savings): Unit = {
    this.stage = stage

    // Note: make the actor name unique (with timestamp) so that it can be
    // recreated later.
    actor = JFXSystem.newJFXActor(ControllerActor.props(savings), s"epsa-main@${System.currentTimeMillis}")

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
    columnUnits.setCellValueFactory { (data: TableColumn.CellDataFeatures[Savings.Asset, BigDecimal]) =>
      new SimpleObjectProperty(data.getValue.units)
    }

    assetsTable.getColumns.addAll(columnScheme, columnFund, columnAmount, columnUnits)
  }

  def onExit(event: ActionEvent): Unit = {
    actor ! OnExit
  }

  def onOptions(event: ActionEvent): Unit = {
    actor ! OnOptions(window)
  }

  def onCreateScheme(event: ActionEvent): Unit = {
    actor ! OnCreateScheme(window)
  }

  def onEditScheme(event: ActionEvent): Unit = {
    Option(schemesField.getSelectionModel.getSelectedItem).foreach { scheme =>
      actor ! OnEditScheme(window, scheme)
    }
  }

  def onCreateFund(event: ActionEvent): Unit = {
    actor ! OnCreateFund(window)
  }

  def onEditFund(event: ActionEvent): Unit = {
    Option(fundsField.getSelectionModel.getSelectedItem).foreach { fund =>
      actor ! OnEditFund(window, fund)
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
        stage.initOwner(window)
        stage.show()

      case None =>
    }
  }

  object ControllerActor {
    def props(savings: Savings) = Props(new ControllerActor(savings))
  }

  class ControllerActor(_savings: Savings) extends Actor {

    // Cheap trick to fill fields with Savings data
    processEvents(_savings, Nil)

    override def receive: Receive = receive(_savings)

    def receive(savings: Savings): Receive = {
      case OnExit                      => onExit()
      case OnOptions(owner)            => onOptions(savings, owner)
      case OnCreateScheme(owner)       => onCreateScheme(savings, owner, None)
      case OnEditScheme(owner, scheme) => onCreateScheme(savings, owner, Some(scheme))
      case OnCreateFund(owner)         => onCreateFund(savings, owner, None)
      case OnEditFund(owner, fund)     => onCreateFund(savings, owner, Some(fund))
      case OnTest                      => onTest(savings)
    }

    def processEvents(savings: Savings, events: List[Savings.Event]): Unit = {
      val newSavings = Savings.processEvents(savings, events)

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

      context.become(receive(newSavings))
    }

    def onExit(): Unit = {
      context.stop(self)
      epsa.Main.shutdown()
    }

    def onOptions(savings: Savings, owner: Window): Unit = {
      val dialog = OptionsController.buildDialog()
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.initOwner(owner)
      dialog.setResizable(true)
      val reload = dialog.showAndWait().orElse(false)
      if (reload) {
        context.stop(self)
        MainController.build(stage, savings)
      }
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
      import java.time.LocalDate

      val s1 = Savings().createSchemeEvent("Scheme 1")
      val s2 = Savings().createSchemeEvent("Scheme 2")
      val f1 = Savings().createFundEvent("Fund 1")
      val f2 = Savings().createFundEvent("Fund 2")
      processEvents(savings, List(s1, s2, f1, f2,
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

  }

}

object MainController {

  case object OnExit

  case class OnOptions(owner: Window)

  case class OnCreateScheme(owner: Window)

  case class OnEditScheme(owner: Window, scheme: Savings.Scheme)

  case class OnCreateFund(owner: Window)

  case class OnEditFund(owner: Window, fund: Savings.Fund)

  case object OnTest

  def build(stage: Stage, savings: Savings): Unit = {
    val loader = new FXMLLoader(getClass.getResource("/fxml/main.fxml"), I18N.getResources)
    val root = loader.load[Parent]()
    val controller = loader.getController[MainController]
    controller.initialize(stage, savings)

    stage.setOnCloseRequest(onCloseRequest(controller) _)

    /*pane.getChildren.setAll(root)
    AnchorPane.setTopAnchor(root, 0)
    AnchorPane.setRightAnchor(root, 0)
    AnchorPane.setBottomAnchor(root, 0)
    AnchorPane.setLeftAnchor(root, 0)*/

    // XXX - manage window reloading
    // Changing scene: re-display window at initial (preferred) size
    // Changing scene root: keeps window size, even if content is bigger

    // Note: when re-creating the scene, we could only change its root. But that
    // does not go well if content computed size changes (window keep the same size)
    Option(stage.getScene).foreach { scene =>
      println(s"${scene.getRoot.getBoundsInLocal} ${scene.getWidth}x${scene.getHeight}")
    }
    Option(stage.getScene) match {
      case Some(scene) => scene.setRoot(root)
      case None => stage.setScene(new Scene(root))
    }
    Option(stage.getScene).foreach { scene =>
      println(s"${scene.getRoot.getBoundsInLocal} ${scene.getWidth}x${scene.getHeight}")
    }
    /*stage.setScene(new Scene(root))
    if (stage.isMaximized) {
      stage.setMaximized(false)
      stage.setMaximized(true)
    }*/
  }

  private def onCloseRequest(controller: MainController)(event: WindowEvent): Unit =
    // Delegate closing request to controller
    controller.onExit(new ActionEvent())

}
