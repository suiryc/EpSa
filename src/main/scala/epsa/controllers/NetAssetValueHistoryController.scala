package epsa.controllers

import epsa.I18N
import epsa.I18N.Strings
import epsa.charts.{ChartHandler, ChartSettings}
import epsa.model.Savings
import epsa.storage.DataStore
import epsa.tools.{BNPParibasInvestmentFundProber, EsaliaInvestmentFundProber, SpreadsheetInvestmentFundProber}
import epsa.util.Awaits
import java.nio.file.Path
import java.time.LocalDate
import java.util.UUID
import javafx.beans.property.SimpleObjectProperty
import javafx.collections.FXCollections
import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Node, Parent, Scene}
import javafx.scene.control._
import javafx.scene.layout.AnchorPane
import javafx.stage._
import scala.collection.JavaConversions._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import suiryc.scala.concurrent.RichFuture._
import suiryc.scala.math.Ordering._
import suiryc.scala.settings.Preference
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.event.EventHandler._
import suiryc.scala.javafx.scene.control.Dialogs
import suiryc.scala.javafx.stage.Stages.StageLocation
import suiryc.scala.javafx.stage.{FileChoosers, Stages}
import suiryc.scala.javafx.util.Callback

// TODO: be notified (by main view) if funds are added/removed ?
// TODO: possibility to manually edit the value for a given date ?
// TODO: button next to operation date to set it to latest asset action date ?
class NetAssetValueHistoryController {

  import NetAssetValueHistoryController._

  // Note: visible progress indicator seems to steal focus (or at least
  // prevent correct handling) from ComboBox if the latter is displayed
  // above the former. So have the ComboBox at the bottom.

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

  private lazy val stage = fundField.getScene.getWindow.asInstanceOf[Stage]

  private var changes = Map[Savings.Fund, Option[Seq[Savings.AssetValue]]]()

  private var chartPane: Option[Node] = None

  private val probers = List(
    EsaliaInvestmentFundProber,
    BNPParibasInvestmentFundProber,
    SpreadsheetInvestmentFundProber
  )

  def initialize(savings: Savings, fundIdOpt: Option[UUID]): Unit = {
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

  /** Restores (persisted) view. */
  private def restoreView(): Unit = {
    // Restore stage location
    Option(stageLocation()).foreach { loc =>
      Stages.setLocation(stage, loc, setSize = true)
    }
  }

  /** Persists view (stage location, ...). */
  private def persistView(): Unit = {
    // Persist stage location
    // Note: if iconified, resets it
    stageLocation() = Stages.getLocation(stage).orNull
  }

  def onCloseRequest(dialog: Dialog[_])(event: WindowEvent): Unit = {
    // Default dialog window closing request is handled to close the dialog
    // when applicable.
    // We do override this behaviour and need to close the dialog ourselves
    // if applicable.
    val canClose =
      if (changes.nonEmpty) Form.confirmDiscardPendingChanges(stage, event)
      else true
    if (canClose) {
      persistView()
      dialog.close()
    }
  }

  def onFund(event: ActionEvent): Unit = {
    Option(fundField.getValue).foreach(loadHistory)
  }

  def onImport(event: ActionEvent): Unit = {
    Option(fundField.getValue).foreach { fund =>
      val fileChooser = new FileChooser()
      fileChooser.setTitle(Strings.importNAVHistory)
      fileChooser.getExtensionFilters.addAll(
        new FileChooser.ExtensionFilter(Strings.spreadsheets, "*.ods", "*.xls", "*.xlsx")
      )
      navHistoryImportPath.option.foreach { path =>
        FileChoosers.setInitialPath(fileChooser, path.toFile)
      }
      val selectedFile = fileChooser.showOpenDialog(stage)
      Option(selectedFile).foreach { file =>
        val path = file.toPath
        probers.toStream.map { prober =>
          prober.probe(path)
        }.find(_.isDefined).map(_.get) match {
          case Some(hist) =>
            // Save path in preferences
            navHistoryImportPath() = selectedFile.toPath

            // And import history
            importHistory(fund, hist)

          case None =>
            Dialogs.warning(
              owner = Some(stage),
              title = None,
              headerText = Some(Strings.unhandledResource),
              contentText = Some(Strings.unknownFormat)
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
        headerText = Some(Strings.confirmAction),
        contentText = Some(Strings.purgeNAVHistory)
      )

      if (resp.contains(ButtonType.OK)) {
        purgeHistory(fund)
      }
    }
  }

  private def accessHistory[A](action: => Future[A], failureMsg: => String, successAction: A => Unit): Unit = {
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
    action.withTimeout(60.seconds).onComplete {
      case Success(result) =>
        showIndicator.cancel()
        // Hide indicator and perform success action (e.g. display new chart)
        progressIndicator.setVisible(false)
        successAction(result)

      case Failure(ex) =>
        showIndicator.cancel()
        // Hide indicator and display issue
        progressIndicator.setVisible(false)
        Dialogs.error(
          owner = Some(stage),
          title = None,
          headerText = Some(failureMsg),
          ex = Some(ex)
        )
    }
  }

  case class HistoryChanges(
    old: Long = 0,
    unchanged: Long = 0,
    changed: Seq[Savings.AssetValue] = Seq.empty,
    added: Seq[Savings.AssetValue] = Seq.empty
  ) {
    lazy val cleaned = (changed ++ added).sortBy(_.date)
  }

  private def cleanHistory(current: Seq[Savings.AssetValue], update: Seq[Savings.AssetValue]): HistoryChanges = {
    @scala.annotation.tailrec
    def loop(values1: Seq[Savings.AssetValue], values2: Seq[Savings.AssetValue], changes: HistoryChanges): HistoryChanges = {
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
      if (values1.isEmpty) changes.copy(added = changes.added ++ values2)
      else if (values2.isEmpty) changes.copy(old = changes.old + values1.length)
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
        if (headCompared < 0) loop(values1.tail, values2, changes.copy(old = changes.old + 1))
        else if (headCompared > 0) loop(values1, values2.tail, changes.copy(added = changes.added :+ values2.head))
        else if (head1.value != head2.value) loop(values1.tail, values2.tail, changes.copy(changed = changes.changed :+ values2.head))
        else loop(values1.tail, values2.tail, changes.copy(unchanged = changes.unchanged + 1))
      }
    }

    loop(current, update, HistoryChanges())
  }

  private def mergeHistory(current: Seq[Savings.AssetValue], update: Seq[Savings.AssetValue]): Seq[Savings.AssetValue] = {
    @scala.annotation.tailrec
    def loop(values1: Seq[Savings.AssetValue], values2: Seq[Savings.AssetValue], merged: Seq[Savings.AssetValue]): Seq[Savings.AssetValue] = {
      if (values1.isEmpty) merged ++ values2
      else if (values2.isEmpty) merged ++ values1
      else {
        val head1 = values1.head
        val head2 = values2.head
        val headCompared = head1.date.compareTo(head2.date)
        if (headCompared < 0) loop(values1.tail, values2, merged :+ values1.head)
        else if (headCompared > 0) loop(values1, values2.tail, merged :+ values2.head)
        else loop(values1.tail, values2.tail, merged :+ values2.head)
      }
    }

    loop(current, update, Seq.empty)
  }

  private def updatedHistory(fund: Savings.Fund, current: Seq[Savings.AssetValue]): Seq[Savings.AssetValue] = {
    changes.getOrElse(fund, Some(Seq.empty)) match {
      case Some(update) => mergeHistory(current, update)
      case None         => Seq.empty
    }
  }

  private def loadHistory(fund: Savings.Fund): Unit =
    accessHistory(
      action = DataStore.AssetHistory.readValues(fund.id),
      failureMsg = DataStore.readIssueMsg(),
      successAction = displayChart(fund, _: Seq[Savings.AssetValue])
    )

  case class ImportResult(name: Option[String], current: Seq[Savings.AssetValue], fundChanges: HistoryChanges)

  private def importHistory(fund: Savings.Fund, history: Savings.AssetValueHistory): Unit = {
    import epsa.Main.Akka.dispatcher

    val values = history.values.sortBy(_.date)
    val action = DataStore.AssetHistory.readValues(fund.id).map { current =>
      // Compute import changes (imported values relatively to updated history)
      val updated = updatedHistory(fund, current)
      val importChanges = cleanHistory(updated, values)
      // Compute fund changes (all changes relatively to initial history)
      val fundChanges = mergeHistory(updatedHistory(fund, Seq.empty), values)
      changes += fund -> Some(fundChanges)
      ImportResult(history.name, updated, importChanges)
    }

    def showResult(result: ImportResult): Unit = {
      displayImportResult(fund, result)
      displayChart(fund, result.current)
    }

    accessHistory(
      action = action,
      failureMsg = DataStore.readIssueMsg(),
      successAction = showResult
    )
  }

  private def purgeHistory(fund: Savings.Fund): Unit = {
    accessHistory(
      action = Future.successful(()),
      failureMsg = "",
      successAction = (_: Unit) => {
        changes += fund -> None
        loadHistory(fund)
      }
    )
  }

  private def displayChart(fund: Savings.Fund, values: Seq[Savings.AssetValue]): Unit = {
    val actualValues = updatedHistory(fund, values)
    purgeButton.setDisable(actualValues.isEmpty)

    val chartHandler = new ChartHandler(
      seriesName = fund.name,
      seriesValues = actualValues,
      settings = ChartSettings.hidden.copy(
        xLabel = Strings.date,
        yLabel = Strings.nav,
        ySuffix = epsa.Settings.defaultCurrency
      )
    )
    val pane = chartHandler.chartPane
    chartPane = Some(pane)
    AnchorPane.setTopAnchor(pane, 0.0)
    AnchorPane.setRightAnchor(pane, 0.0)
    AnchorPane.setBottomAnchor(pane, 0.0)
    AnchorPane.setLeftAnchor(pane, 0.0)
    historyPane.getChildren.add(pane)
  }

  /** Displays history import result in dedicated window. */
  private def displayImportResult(fund: Savings.Fund, result: ImportResult): Unit = {
    type AssetEntries = Seq[AssetEntry]
    case class AssetEntry(date: LocalDate, values: Seq[BigDecimal])

    // Note: it appears hard, if not impossible, to declare the table columns
    // in fxml while expecting to its widths to adjust to its content upon
    // populating it.
    // If columns are created and set here in the code, the width adjust as
    // wanted.

    def setupTable(table: TableView[AssetEntry], headers: Seq[String], entries: AssetEntries): Unit = {
      val column1 = new TableColumn[AssetEntry, LocalDate](headers.head)
      column1.setCellValueFactory(Callback { data =>
        new SimpleObjectProperty(data.getValue.date)
      })
      val columns = column1 +: headers.tail.zipWithIndex.map {
        case (header, idx) =>
          val column2 = new TableColumn[AssetEntry, BigDecimal](header)
          column2.setCellValueFactory(Callback { data =>
            new SimpleObjectProperty(data.getValue.values(idx))
          })
          column2
      }
      table.getColumns.setAll(columns)
      table.setItems(FXCollections.observableList(entries))
    }

    val current = result.current.groupBy(_.date).mapValues(_.head)
    val updatedEntries = result.fundChanges.changed.map { changed =>
      AssetEntry(changed.date, Seq(current(changed.date).value, changed.value))
    }
    val addedEntries = result.fundChanges.added.map { added =>
      AssetEntry(added.date, Seq(added.value))
    }

    val loader = new FXMLLoader(getClass.getResource("/fxml/net-asset-value-history-changes.fxml"), I18N.getResources)
    val root = loader.load[Parent]()
    val tabPane = root.lookup("#tabPane").asInstanceOf[TabPane]
    val fundLabel = root.lookup("#fundLabel").asInstanceOf[Label]
    val updatedLabel = root.lookup("#updatedLabel").asInstanceOf[Label]
    // Note: 'TabPane' does not lookup its own 'Tab's but only their content ...
    val updatedTab = tabPane.getTabs.find(_.getId == "updatedTab").get
    val updatedTable = root.lookup("#updatedTable").asInstanceOf[TableView[AssetEntry]]
    val addedLabel = root.lookup("#addedLabel").asInstanceOf[Label]
    val addedTab = tabPane.getTabs.find(_.getId == "addedTab").get
    val addedTable = root.lookup("#addedTable").asInstanceOf[TableView[AssetEntry]]
    val unchangedLabel = root.lookup("#unchangedLabel").asInstanceOf[Label]

    fundLabel.setText(fund.name + result.name.map(name => s"\n($name)").getOrElse(""))
    updatedLabel.setText(updatedEntries.size.toString)
    addedLabel.setText(addedEntries.size.toString)
    unchangedLabel.setText(result.fundChanges.unchanged.toString)
    if (updatedEntries.nonEmpty) {
      val headers = Seq(
        Strings.date,
        Strings.oldValue,
        Strings.newValue
      )
      setupTable(updatedTable, headers, updatedEntries)
    } else {
      tabPane.getTabs.remove(updatedTab)
    }
    if (addedEntries.nonEmpty) {
      val headers = Seq(
        Strings.date,
        Strings.nav
      )
      setupTable(addedTable, headers, addedEntries)
    } else {
      tabPane.getTabs.remove(addedTab)
    }

    // Notes:
    // Make a dedicated window. Don't bother close any previous one.
    // Use current window as owner, and share its title.
    // Use 'utility' style (we don't need minimize/maximize).
    val resultStage = new Stage()
    resultStage.initStyle(StageStyle.UTILITY)
    resultStage.initOwner(stage)
    resultStage.setTitle(stage.getTitle)
    val scene = new Scene(root)
    resultStage.setScene(scene)
    Stages.trackMinimumDimensions(resultStage)
    resultStage.show()
  }

}

object NetAssetValueHistoryController {

  import epsa.Settings.prefs
  import Preference._

  private val stageLocation = Preference.from("stage.nav.history.location", null:StageLocation)

  private val navHistoryImportPath = Preference.from("nav.history.import.path", null:Path)

  def title = Strings.navHistory

  /** Builds a stage out of this controller. */
  def buildStage(mainController: MainController, savings: Savings, fundId: Option[UUID], window: Window): Dialog[Boolean] = {
    val dialog = new Dialog[Boolean]()
    Stages.getStage(dialog).getIcons.setAll(Images.iconChartUp)
    dialog.setTitle(title)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val loader = new FXMLLoader(getClass.getResource("/fxml/net-asset-value-history.fxml"), I18N.getResources)
    dialog.getDialogPane.setContent(loader.load())
    val controller = loader.getController[NetAssetValueHistoryController]
    controller.initialize(savings, fundId)

    // Delegate closing request to controller
    Stages.getStage(dialog).setOnCloseRequest(controller.onCloseRequest(dialog) _)

    // Wait for dialog to be shown before restoring the view
    dialog.showingProperty().listen2 { cancellable =>
      cancellable.cancel()
      controller.restoreView()
    }

    dialog.setResultConverter(Callback { resultConverter(mainController, window, controller) _ })
    Stages.trackMinimumDimensions(Stages.getStage(dialog))

    dialog
  }

  private def resultConverter(mainController: MainController, windows: Window, controller: NetAssetValueHistoryController)(buttonType: ButtonType): Boolean = {
    import epsa.Main.Akka.dispatcher

    // Apply changes upon validation
    if (buttonType == ButtonType.OK) {
      val actions = controller.changes.map {
        case (fund, change) =>
          change match {
            case Some(values) => Action(DataStore.AssetHistory.writeValues(fund.id, values))
            case None         => Action(DataStore.AssetHistory.deleteValues(fund.id))
          }
      }.toSeq
      // Apply as many changes as possible
      val future = executeSequentially(stopOnError = false, actions: _*)
      // Wait for result and display issue if any
      Awaits.orError(future, Some(windows), DataStore.writeIssueMsg())
      // Request main view to refresh (i.e. check for pending changes)
      mainController.refresh()
      true
    } else {
      false
    }
  }

}
