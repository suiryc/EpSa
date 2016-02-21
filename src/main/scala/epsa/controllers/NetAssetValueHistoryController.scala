package epsa.controllers

import epsa.I18N
import epsa.charts.{ChartHandler, ChartSettings}
import epsa.model.Savings
import epsa.storage.DataStore
import epsa.tools.EsaliaInvestmentFundProber
import java.nio.file.Path
import java.util.{ResourceBundle, UUID}
import javafx.collections.FXCollections
import javafx.event.ActionEvent
import javafx.fxml.{FXMLLoader, FXML}
import javafx.scene.Scene
import javafx.scene.control.{Button, ButtonType, ComboBox, ProgressIndicator}
import javafx.scene.layout.AnchorPane
import javafx.stage.{FileChooser, Stage}
import scala.collection.JavaConversions._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import suiryc.scala.concurrent.RichFuture._
import suiryc.scala.math.Ordering._
import suiryc.scala.settings.Preference
import suiryc.scala.javafx.scene.control.Dialogs
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.util.Callback

// TODO - be notified (by main view) if funds are added/removed ?
// TODO - show imported chart and ask confirmation to apply it (in-memory changes)
class NetAssetValueHistoryController {

  import NetAssetValueHistoryController._

  // Note: visible progress indicator seems to steal focus (or at least
  // prevent correct handling) from ComboBox if the latter is displayed
  // above the former. So have the ComboBox at the bottom.

  @FXML
  protected var resources: ResourceBundle = _

  @FXML
  protected var historyPane: AnchorPane = _

  @FXML
  protected var progressIndicator: ProgressIndicator = _

  @FXML
  protected var fundField: ComboBox[Savings.Fund] = _

  @FXML
  protected var importButton: Button = _

  @FXML
  protected var purgeButton: Button = _

  private var mainController: MainController = _

  private lazy val stage = fundField.getScene.getWindow.asInstanceOf[Stage]

  private var chartPane: Option[AnchorPane] = None

  //def initialize(): Unit = { }

  def initialize(mainController: MainController, savings: Savings, fundIdOpt: Option[UUID]): Unit = {
    this.mainController = mainController

    // Note: we need to tell the combobox how to display both the 'button' area
    // (what is shown as selected) and the content (list of choices).
    fundField.setButtonCell(new FundCell)
    fundField.setCellFactory(Callback { new FundCell })

    val funds = savings.schemes.flatMap { scheme =>
      scheme.funds.map(savings.getFund)
    }.distinct.sortBy(_.name)
    fundField.setItems(FXCollections.observableList(funds))

    importButton.setDisable(funds.isEmpty)
    purgeButton.setDisable(funds.isEmpty)

    fundIdOpt.flatMap { fundId =>
      funds.find(_.id == fundId)
    }.orElse(funds.headOption).foreach { fund =>
      fundField.getSelectionModel.select(fund)
      loadHistory(fund)
    }
  }

  def onFund(event: ActionEvent): Unit = {
    Option(fundField.getValue).foreach(loadHistory)
  }

  def onImport(event: ActionEvent): Unit = {
    Option(fundField.getValue).foreach { fund =>
      val fileChooser = new FileChooser()
      fileChooser.setTitle(resources.getString("Import net asset value history"))
      fileChooser.getExtensionFilters.addAll(
        new FileChooser.ExtensionFilter(resources.getString("Excel files"), "*.xls", "*.xlsx"),
        new FileChooser.ExtensionFilter(resources.getString("All files"), "*.*")
      )
      navHistoryImportPath.option.foreach { path =>
        fileChooser.setInitialDirectory(path.getParent.toFile)
        fileChooser.setInitialFileName(path.toFile.getName)
      }
      val selectedFile = fileChooser.showOpenDialog(stage)
      Option(selectedFile).foreach { file =>
        EsaliaInvestmentFundProber.probe(file.toPath) match {
          case Some(hist) =>
            // Save path in preferences
            navHistoryImportPath() = selectedFile.toPath

            // And import history
            importHistory(fund, hist.values.sortBy(_.date))

          case None =>
            Dialogs.warning(
              owner = Some(stage),
              title = None,
              headerText = Some(resources.getString("warning.unhandled-resource")),
              contentText = Some(resources.getString("Unknown format"))
            )
        }
      }
    }
  }

  def onPurge(event: ActionEvent): Unit = {
    Option(fundField.getValue).foreach { fund =>
      val resp = Dialogs.confirmation(
        owner = Some(stage),
        title = None,
        headerText = Some(resources.getString("confirmation.irreversible-action")),
        contentText = Some(resources.getString("Purge net asset value history"))
      )

      if (resp.contains(ButtonType.OK)) {
        purgeHistory(fund)
      }
    }
  }

  private def accessHistory[A](action: => Future[A], failureMsg: String, successAction: A => Unit): Unit = {
    import suiryc.scala.javafx.concurrent.JFXExecutor.executor

    // Remove previous chart if any
    chartPane.foreach { pane =>
      historyPane.getChildren.remove(pane)
    }
    chartPane = None

    // Prepare to display progress indicator (if action takes too long)
    val showIndicator = epsa.Main.Akka.system.scheduler.scheduleOnce(500.milliseconds) {
      progressIndicator.setVisible(true)
    }

    // Action on history (with timeout)
    action.withTimeout(10.seconds).onComplete {
      case Success(result) =>
        showIndicator.cancel()
        // Hide indicator and display new chart
        progressIndicator.setVisible(false)
        successAction(result)

      case Failure(ex) =>
        showIndicator.cancel()
        progressIndicator.setVisible(false)
        Dialogs.error(
          owner = Some(stage),
          title = None,
          headerText = Some(failureMsg),
          ex = Some(ex)
        )
    }
  }

  // TODO: display comparison result to use
  private def compareHistory(fund: Savings.Fund, values: Seq[Savings.AssetValue]): Unit = {
    import epsa.Main.Akka.dispatcher

    case class ComparedData(old: Long = 0, unchanged: Long = 0, changed: Long = 0, added: Long = 0)

    @scala.annotation.tailrec
    def compare(values1: Seq[Savings.AssetValue], values2: Seq[Savings.AssetValue], compared: ComparedData): ComparedData = {
      // We want to compare existing data to new ones.
      // Both are sorted by date. We need to match dates between both sets of
      // data. To do so, we simply check each head date, and iterate either the
      // set which has the oldest one (try to catch on the other set), or both
      // if the dates match.

      // If there is no more values in the old data, all remaining new ones are
      // really 'new'.
      // If there is no more values in the new data, all remaining old ones are
      // really 'old'.
      // Otherwise, we need to compare each head date.
      if (values1.isEmpty) compared.copy(added = compared.added + values2.length)
      else if (values2.isEmpty) compared.copy(old = compared.old + values1.length)
      else {
        // If the existing data head has the oldest date, it corresponds to a
        // value unmatched ('old') in the new data.
        // Conversely, if the new data has the oldest date, it corresponds to a
        // value unmatched ('new') in the old data.
        // Otherwise, either the old value is 'changed' or 'unchanged' in the
        // new data.
        val head1 = values1.head
        val head2 = values2.head
        val headCompared = head1.date.compareTo(head2.date)
        if (headCompared < 0) compare(values1.tail, values2, compared.copy(old = compared.old + 1))
        else if (headCompared > 0) compare(values1, values2.tail, compared.copy(added = compared.added + 1))
        else if (head1.value != head2.value) compare(values1.tail, values2.tail, compared.copy(changed = compared.changed + 1))
        else compare(values1.tail, values2.tail, compared.copy(unchanged = compared.unchanged + 1))
      }
    }

    DataStore.AssetHistory.readValues(fund.id).onComplete {
      case Success(result) =>
        println(compare(result, values, ComparedData()))

      case Failure(ex) =>
        //showIndicator.cancel()
        //progressIndicator.setVisible(false)
        Dialogs.error(
          owner = Some(stage),
          title = None,
          headerText = Some(DataStore.readIssueMsg),
          ex = Some(ex)
        )
    }
  }

  private def loadHistory(fund: Savings.Fund): Unit =
    accessHistory(
      action = DataStore.AssetHistory.readValues(fund.id),
      failureMsg = DataStore.readIssueMsg,
      successAction = displayChart(fund, _: Seq[Savings.AssetValue])
    )

  private def importHistory(fund: Savings.Fund, values: Seq[Savings.AssetValue]): Unit = {
    import epsa.Main.Akka.dispatcher

    compareHistory(fund, values)

    accessHistory(
      action = DataStore.AssetHistory.writeValues(fund.id, values: _*).map(_ => values)/*Future.successful(values)*/,
      failureMsg = DataStore.writeIssueMsg,
      successAction = (_: Seq[Savings.AssetValue]) => {
        mainController.refresh()
        loadHistory(fund)
      }
    )
  }

  private def purgeHistory(fund: Savings.Fund): Unit = {
    accessHistory(
      action = DataStore.AssetHistory.deleteValues(fund.id),
      failureMsg = DataStore.writeIssueMsg,
      successAction = (_: Int) => {
        mainController.refresh()
        loadHistory(fund)
      }
    )
  }

  private def displayChart(fund: Savings.Fund, values: Seq[Savings.AssetValue]): Unit = {
    purgeButton.setDisable(values.isEmpty)

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
  }

}

object NetAssetValueHistoryController {

  import epsa.Main.prefs
  import Preference._

  protected val navHistoryImportPath = Preference.from("nav.history.import.path", null:Path)

  def labelDate = I18N.getResources.getString("Date")

  def labelNAV = I18N.getResources.getString("NAV")

  /** Builds a stage out of this controller. */
  def buildStage(mainController: MainController, savings: Savings, fundId: Option[UUID]): Stage = {
    val resources = I18N.getResources

    val loader = new FXMLLoader(getClass.getResource("/fxml/net-asset-value-history.fxml"), resources)
    val scene = new Scene(loader.load())
    val controller = loader.getController[NetAssetValueHistoryController]
    controller.initialize(mainController, savings, fundId)

    val stage = new Stage()
    val title = resources.getString("Net asset value history")
    stage.setTitle(title)
    stage.setScene(scene)

    Stages.trackMinimumDimensions(stage)

    stage
  }

}
