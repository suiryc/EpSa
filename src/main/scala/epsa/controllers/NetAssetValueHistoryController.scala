package epsa.controllers

import javafx.scene.Scene
import javafx.stage.Stage

import epsa.I18N
import epsa.charts.{ChartSettings, ChartHandler}
import epsa.model.Savings
import epsa.storage.DataStore
import java.util.{ResourceBundle, UUID}
import javafx.collections.FXCollections
import javafx.event.ActionEvent
import javafx.fxml.{FXMLLoader, FXML}
import javafx.scene.control.{ComboBox, ProgressIndicator}
import javafx.scene.layout.{AnchorPane, VBox}
import scala.collection.JavaConversions._
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import suiryc.scala.concurrent.RichFuture._
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.scene.control.Dialogs
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.util.Callback

// TODO - check data store is open
// TODO - action to load values from excel file and save them in data store
// TODO - prevent saving if changes are pending in main view ?
// TODO - be notified (by main view) if funds are added/removed ?
class NetAssetValueHistoryController {

  import NetAssetValueHistoryController._

  // Note: visible progress indicator seems to steal focus (or at least
  // prevent correct handling) from ComboBox if the latter is displayed
  // above the former. So have the ComboBox at the bottom.

  @FXML
  protected var resources: ResourceBundle = _

  // TODO - useful ?
  @FXML
  protected var vbox: VBox = _

  @FXML
  protected var fundField: ComboBox[Savings.Fund] = _

  @FXML
  protected var historyPane: AnchorPane = _

  @FXML
  protected var progressIndicator: ProgressIndicator = _

  private lazy val stage = fundField.getScene.getWindow.asInstanceOf[Stage]

  private var chartPane: Option[AnchorPane] = None

  //def initialize(): Unit = { }

  def initialize(savings: Savings, fundIdOpt: Option[UUID]): Unit = {
    // Note: we need to tell the combobox how to display both the 'button' area
    // (what is shown as selected) and the content (list of choices).
    fundField.setButtonCell(new FundCell)
    fundField.setCellFactory(Callback { new FundCell })

    val funds = savings.schemes.flatMap { scheme =>
      scheme.funds.map(savings.getFund)
    }.distinct.sortBy(_.name)
    fundField.setItems(FXCollections.observableList(funds))

    fundField.getSelectionModel.selectedItemProperty.listen { fund =>
      if (Option(fund).isDefined) {
        import suiryc.scala.javafx.concurrent.JFXExecutor.executor

        // Remove previous chart if any
        chartPane.foreach { pane =>
          historyPane.getChildren.remove(pane)
        }
        // Prepare to display progress indicator (if loading takes too long)
        val showIndicator = epsa.Main.Akka.system.scheduler.scheduleOnce(500.milliseconds) {
          progressIndicator.setVisible(true)
        }

        // Load values (with timeout)
        DataStore.AssetHistory.readValues(fund.id).withTimeout(10.seconds).onComplete {
          case Success(values) =>
            // Hide indicator and display new chart
            showIndicator.cancel()
            progressIndicator.setVisible(false)
            val chartHandler = new ChartHandler(
              fundName = fund.name,
              fundValues = values,
              settings = ChartSettings.hidden.copy(xLabel = labelDate, yLabel = labelNAV)
            )
            val pane = chartHandler.chartPane
            chartPane = Some(pane)
            AnchorPane.setTopAnchor(pane, 0.0)
            AnchorPane.setRightAnchor(pane, 0.0)
            AnchorPane.setBottomAnchor(pane, 0.0)
            AnchorPane.setLeftAnchor(pane, 0.0)
            historyPane.getChildren.add(pane)

          case Failure(ex) =>
            showIndicator.cancel()
            progressIndicator.setVisible(false)
            Dialogs.error(Some(stage), None, Some(DataStore.readIssueMsg), ex)
        }
      }
    }

    fundIdOpt.flatMap { fundId =>
      funds.find(_.id == fundId)
    }.orElse(funds.headOption).foreach(fundField.getSelectionModel.select)
  }

  def onSrcFund(event: ActionEvent): Unit = {
    Option(fundField.getValue).map(_.id) match {
      case Some(fundId) =>
      case None =>
    }
  }

}

object NetAssetValueHistoryController {

  def labelDate = I18N.getResources.getString("Date")

  def labelNAV = I18N.getResources.getString("NAV")

  /** Builds a stage out of this controller. */
  def buildStage(savings: Savings, fundId: Option[UUID]): Stage = {
    val resources = I18N.getResources

    val loader = new FXMLLoader(getClass.getResource("/fxml/net-asset-value-history.fxml"), resources)
    val scene = new Scene(loader.load())
    val controller = loader.getController[NetAssetValueHistoryController]
    controller.initialize(savings, fundId)

    val stage = new Stage()
    val title = resources.getString("Net asset value history")
    stage.setTitle(title)
    stage.setScene(scene)

    Stages.trackMinimumDimensions(stage)

    stage
  }

}
