package epsa.controllers

import akka.actor.{Actor, ActorRef, Props}
import epsa.I18N
import epsa.charts.ChartHandler
import epsa.model.Savings
import epsa.tools.EsaliaInvestmentFundProber
import java.nio.file.Path
import java.util.{ResourceBundle, UUID}
import javafx.beans.binding.Bindings
import javafx.beans.property.{SimpleObjectProperty, SimpleStringProperty}
import javafx.collections.FXCollections
import javafx.collections.transformation.SortedList
import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene}
import javafx.scene.control._
import javafx.stage.{FileChooser, Modality, Stage, WindowEvent}
import javafx.stage.FileChooser.ExtensionFilter
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.event.EventHandler._
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.util.Callback._
import suiryc.scala.settings.Preference

// TODO - menu key shortcuts ?
// TODO - change menu for OS integration ? (e.g. Ubuntu)
// TODO - persist main window size/position; restart at last size/position
// TODO - persist table view columns positions/width
class MainController {

  import epsa.Main.prefs
  import MainController._

  //@FXML
  //protected var location: URL = _

  @FXML
  protected var resources: ResourceBundle = _

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
    // Note: Asset gives scheme/fund UUID. Since State is immutable (and is
    // changed when applying events in controller) we must delegate scheme/fund
    // lookup to the controller.
    assetsTable.setRowFactory { (tv: TableView[Savings.Asset]) =>
      newAssetRow()
    }
  }

  def onExit(event: ActionEvent): Unit = {
    actor ! OnExit
  }

  def onEditSchemes(event: ActionEvent): Unit = {
    actor ! OnEditSchemes
  }

  def onEditFunds(event: ActionEvent): Unit = {
    actor ! OnEditFunds
  }

  def onOptions(event: ActionEvent): Unit = {
    actor ! OnOptions
  }

  def onTest(event: ActionEvent): Unit = {
    actor ! OnTest
  }

  def onFundGraph(event: ActionEvent): Unit = {
    actor ! OnFundGraph
  }

  /**
   * Creates a new Asset table view row.
   *
   * Binds menu context to edit asset scheme/fund.
   */
  private def newAssetRow(): TableRow[Savings.Asset] = {
    val row = new TableRow[Savings.Asset]()

    // See: https://www.marshall.edu/genomicjava/2013/12/30/javafx-tableviews-with-contextmenus/
    val menu = new ContextMenu()
    val editScheme = new MenuItem(resources.getString("Edit scheme"))
    editScheme.setOnAction { (event: ActionEvent) =>
      Option(row.getItem).foreach { asset =>
        actor ! OnEditScheme(asset.schemeId)
      }
    }
    val editFund = new MenuItem(resources.getString("Edit fund"))
    editFund.setOnAction { (event: ActionEvent) =>
      Option(row.getItem).foreach { asset =>
        actor ! OnEditFund(asset.fundId)
      }
    }
    menu.getItems.addAll(editScheme, editFund)

    row.contextMenuProperty().bind {
      Bindings.when(Bindings.isNotNull(row.itemProperty))
        .`then`(menu)
        .otherwise(null:ContextMenu)
    }

    row
  }

  object ControllerActor {
    def props(state: State) = Props(new ControllerActor(state))
  }

  class ControllerActor(state0: State) extends Actor {

    // Cheap trick to fill fields with Savings data
    processEvents(state0, Nil)

    override def receive: Receive = receive(state0)

    def receive(state: State): Receive = {
      case OnExit           => onExit(state)
      case OnEditSchemes    => onEditSchemes(state, None)
      case OnEditScheme(id) => onEditSchemes(state, Some(state.savings.getScheme(id)))
      case OnEditFunds      => onEditFunds(state, None)
      case OnEditFund(id)   => onEditFunds(state, Some(state.savings.getFund(id)))
      case OnOptions        => onOptions(state)
      case OnTest           => onTest(state)
      case OnFundGraph      => onFundGraph(state)
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
      val sortedAssets = new SortedList(FXCollections.observableList(newSavings.assets))
      sortedAssets.comparatorProperty.bind(assetsTable.comparatorProperty)
      assetsTable.setItems(sortedAssets)

      context.become(receive(state.copy(savings = newSavings)))
    }

    def onExit(state: State): Unit = {
      context.stop(self)
      epsa.Main.shutdown(state.stage)
    }

    def onEditSchemes(state: State, edit0: Option[Savings.Scheme]): Unit = {
      val edit = edit0.orElse(Option(assetsTable.getSelectionModel.getSelectedItem).map { asset =>
        state.savings.getScheme(asset.schemeId)
      })
      val dialog = EditSchemesController.buildDialog(state.savings, edit)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.initOwner(state.window)
      dialog.setResizable(true)
      val events = dialog.showAndWait().orElse(Nil)
      processEvents(state, events)
    }

    def onEditFunds(state: State, edit0: Option[Savings.Fund]): Unit = {
      val edit = edit0.orElse(Option(assetsTable.getSelectionModel.getSelectedItem).map { asset =>
        state.savings.getFund(asset.fundId)
      })
      val dialog = EditFundsController.buildDialog(state.savings, edit)
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.initOwner(state.window)
      dialog.setResizable(true)
      val events = dialog.showAndWait().orElse(Nil)
      processEvents(state, events)
    }

    def onOptions(state: State): Unit = {
      val dialog = OptionsController.buildDialog()
      dialog.initModality(Modality.WINDOW_MODAL)
      dialog.initOwner(state.window)
      dialog.setResizable(true)
      val reload = dialog.showAndWait().orElse(false)
      if (reload) {
        context.stop(self)
        MainController.build(state, needRestart = true)
      }
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

  case object OnEditSchemes

  case object OnEditFunds

  case object OnOptions

  case class OnEditScheme(schemeId: UUID)

  case class OnEditFund(fundId: UUID)

  case object OnTest

  case object OnFundGraph

  def build(state: State, needRestart: Boolean = false): Unit = {
    val stage = state.stage

    val resources = I18N.getResources
    val loader = new FXMLLoader(getClass.getResource("/fxml/main.fxml"), resources)
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

    if (needRestart) {
      val alert = new Alert(Alert.AlertType.INFORMATION)
      alert.initOwner(state.window)
      alert.setHeaderText(resources.getString("information.need-restart"))
      alert.showAndWait()
    }
  }

  private def onCloseRequest(controller: MainController)(event: WindowEvent): Unit =
    // Delegate closing request to controller
    controller.onExit(new ActionEvent())

}
