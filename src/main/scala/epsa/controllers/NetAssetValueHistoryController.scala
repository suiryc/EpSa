package epsa.controllers

import epsa.{I18N, Main, Settings}
import epsa.I18N.Strings
import epsa.Settings.{formatCompactNumber, parseNumber}
import epsa.charts._
import epsa.model.Savings
import epsa.storage.DataStore
import epsa.tools.{AmundiInvestmentFundDownloader, BNPParibasInvestmentFundProber, EsaliaInvestmentFundProber, SpreadsheetInvestmentFundProber}
import epsa.util.{Awaits, JFXStyles}
import java.nio.file.Path
import java.time.LocalDate
import java.util.UUID
import javafx.beans.property.SimpleObjectProperty
import javafx.collections.FXCollections
import javafx.event.{ActionEvent, Event}
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene}
import javafx.scene.control._
import javafx.scene.input.MouseEvent
import javafx.scene.layout.AnchorPane
import javafx.stage._
import scala.collection.JavaConverters._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import suiryc.scala.concurrent.RichFuture._
import suiryc.scala.math.Ordered._
import suiryc.scala.math.Ordering._
import suiryc.scala.settings.ConfigEntry
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.scene.control.{Dialogs, TextFieldWithButton}
import suiryc.scala.javafx.stage.Stages.StageLocation
import suiryc.scala.javafx.stage.{PathChoosers, StagePersistentView, Stages}

class NetAssetValueHistoryController extends StagePersistentView {

  import NetAssetValueHistoryController._

  // Note: visible progress indicator seems to steal focus (or at least
  // prevent correct handling) from ComboBox if the latter is displayed
  // above the former. So have the ComboBox at the bottom.

  @FXML
  protected var historyPane: AnchorPane = _

  @FXML
  protected var progressIndicator: ProgressIndicator = _

  @FXML
  protected var fundField: ComboBox[Option[Savings.Fund]] = _

  @FXML
  protected var downloadButton: Button = _

  @FXML
  protected var importButton: Button = _

  @FXML
  protected var purgeButton: Button = _

  private val historyChartContextMenu = new ContextMenu()

  private lazy val stage = fundField.getScene.getWindow.asInstanceOf[Stage]

  private var changes = Map[Savings.Fund, Option[Seq[Savings.AssetValue]]]()

  private var chartHandler: ChartHandler[ChartMark] = _

  private val probers = List(
    EsaliaInvestmentFundProber,
    BNPParibasInvestmentFundProber,
    SpreadsheetInvestmentFundProber
  )

  private var working = false

  def initialize(savings: Savings, dialog: Dialog[_], fundIdOpt: Option[UUID]): Unit = {
    // JavaFx applies padding on dialog pane content.
    // We can either add another CSS class to disable it, or simply remove the
    // "content" class as it is only defined to apply padding.
    dialog.getDialogPane.getContent.getStyleClass.remove("content")

    // Note: we need to tell the combobox how to display both the 'button' area
    // (what is shown as selected) and the content (list of choices).
    fundField.setButtonCell(new FundCell)
    fundField.setCellFactory(_ => new FundCell)

    val funds = savings.getFunds(associated = true)
    val entries = Form.buildOptions(funds.filter(!_.disabled), funds.filter(_.disabled))
    fundField.setItems(FXCollections.observableList(entries.asJava))

    downloadButton.setDisable(funds.isEmpty)
    importButton.setDisable(funds.isEmpty)
    purgeButton.setDisable(funds.isEmpty)

    fundIdOpt.flatMap { fundId =>
      funds.find(_.id == fundId)
    }.orElse(entries.flatten.headOption).foreach { fund =>
      fundField.getSelectionModel.select(Some(fund))
      loadHistory(fund)
    }

    // Lookup dialog buttons
    val buttonOk = dialog.getDialogPane.lookupButton(ButtonType.OK)
    val buttonCancel = dialog.getDialogPane.lookupButton(ButtonType.CANCEL)

    // Ask confirmation before leaving if we are working
    def confirmationFilter[A <: Event](event: A): Unit = {
      if (working) Form.confirmDiscardPendingAction(Stages.getStage(dialog), event)
      ()
    }
    for (button <- List(buttonOk, buttonCancel)) {
      button.addEventFilter(ActionEvent.ACTION, confirmationFilter[ActionEvent] _)
    }
  }

  /** Restores (persisted) view. */
  override protected def restoreView(): Unit = {
    Stages.onStageReady(stage, first = false) {
      // Restore stage location
      Stages.setMinimumDimensions(stage)
      stageLocation.opt.foreach { loc =>
        Stages.setLocation(stage, loc, setSize = true)
      }
    }(JFXSystem.dispatcher)
  }

  /** Persists view (stage location, ...). */
  override protected def persistView(): Unit = {
    // Persist stage location
    // Note: if iconified, resets it
    stageLocation.set(Stages.getLocation(stage).orNull)
  }

  def onCloseRequest(dialog: Dialog[_])(event: WindowEvent): Unit = {
    // Default dialog window closing request is handled to close the dialog
    // when applicable.
    // We do override this behaviour and need to close the dialog ourselves
    // if applicable.

    def checkWorking = if (working) Form.confirmDiscardPendingAction(stage, event) else true
    def checkPendingChanges = if (changes.nonEmpty) Form.confirmDiscardPendingChanges(stage, event) else true

    val canClose = checkWorking && checkPendingChanges
    if (canClose) dialog.close()
  }

  def onDownloadNAVHistories(@deprecated("unused","") event: ActionEvent): Unit = {
    // Remember currently selected fund
    val currentFund = getFund
    // Hide chart and selection
    fundField.setValue(null)
    Option(chartHandler).foreach(_.chartPane.setVisible(false))

    // Process all concerned funds
    val actions = fundField.getItems.iterator().asScala.toList.flatten.filter { fund =>
      // We need an enabled fund with AMF id
      fund.amfId.isDefined && !fund.disabled
    }.map { fund =>
      Action(downloadHistory(fund, chart = false))
    }

    executeSequentially(actions)(epsa.Main.Akka.dispatcher).onComplete { _ =>
      // Time to get back to initially selected fund
      fundField.getSelectionModel.select(currentFund)
    }(JFXSystem.dispatcher)
  }

  def onFund(@deprecated("unused","") event: ActionEvent): Unit = {
    getFund.foreach { fund =>
      downloadButton.setDisable(fund.amfId.isEmpty)
      loadHistory(fund)
    }
  }

  def onDownload(@deprecated("unused","") event: ActionEvent): Unit = {
    getFund.filter(_.amfId.isDefined).foreach(downloadHistory(_))
  }

  def onImport(@deprecated("unused","") event: ActionEvent): Unit = {
    getFund.foreach { fund =>
      val fileChooser = new FileChooser()
      fileChooser.setTitle(Strings.importNAVHistory)
      fileChooser.getExtensionFilters.addAll(
        new FileChooser.ExtensionFilter(Strings.spreadsheets, "*.ods", "*.xls", "*.xlsx")
      )
      navHistoryImportPath.opt.foreach { path =>
        PathChoosers.setInitialPath(fileChooser, path.toFile)
      }
      val selectedFile = fileChooser.showOpenDialog(stage)
      Option(selectedFile).foreach { file =>
        val path = file.toPath
        probers.toStream.map { prober =>
          prober.probe(path)
        }.find(_.isDefined).flatten match {
          case Some(hist) =>
            // Save path in preferences
            navHistoryImportPath.set(selectedFile.toPath)

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

  def onPurge(@deprecated("unused","") event: ActionEvent): Unit = {
    getFund.foreach { fund =>
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

  private def accessHistory[A](action: => Future[A], failureMsg: => String, successAction: A => Unit): Future[A] = {
    working = true

    // Prepare to display progress indicator (if action takes too long)
    val showIndicator = JFXSystem.scheduler.scheduleOnce(500.milliseconds) {
      progressIndicator.toFront()
      progressIndicator.setVisible(true)
    }

    // Action on history (with timeout)
    val f = action.withTimeout(60.seconds)
    f.onComplete {
      case Success(result) =>
        showIndicator.cancel()
        // Hide indicator and perform success action (e.g. display new chart)
        progressIndicator.toBack()
        progressIndicator.setVisible(false)
        successAction(result)
        working = false

      case Failure(ex) =>
        showIndicator.cancel()
        // Hide indicator and display issue
        progressIndicator.toBack()
        progressIndicator.setVisible(false)
        Dialogs.error(
          owner = Some(stage),
          title = None,
          contentText = Some(failureMsg),
          ex = Some(ex)
        )
        working = false
    }(JFXSystem.dispatcher)

    f
  }

  case class HistoryChanges(
    old: Long = 0,
    unchanged: Long = 0,
    changed: Seq[Savings.AssetValue] = Seq.empty,
    added: Seq[Savings.AssetValue] = Seq.empty
  ) {
    lazy val cleaned: Seq[Savings.AssetValue] = (changed ++ added).sortBy(_.date)
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

  private def loadHistory(fund: Savings.Fund): Future[Seq[Savings.AssetValue]] =
    accessHistory(
      action = DataStore.AssetHistory.readValues(fund.id),
      failureMsg = DataStore.readIssueMsg(),
      successAction = displayChart(fund, _: Seq[Savings.AssetValue])
    )

  case class ImportResult(name: Option[String], current: Seq[Savings.AssetValue], fundChanges: HistoryChanges)

  private def showImportResult(fund: Savings.Fund, chart: Boolean)(result: ImportResult): Unit = {
    displayImportResult(fund, result)
    if (chart) displayChart(fund, result.current)
  }

  private def updateHistory(fund: Savings.Fund, current: Seq[Savings.AssetValue], history: Savings.AssetValueHistory): ImportResult = {
    val values = history.values.sortBy(_.date)
    // Compute import changes (imported values relatively to updated history)
    val updated = updatedHistory(fund, current)
    val importChanges = cleanHistory(updated, values)
    if (importChanges.added.nonEmpty || importChanges.changed.nonEmpty) {
      // Compute new fund history
      val fundChanges = mergeHistory(updatedHistory(fund, Seq.empty), values)
      changes += fund -> Some(fundChanges)
    }
    ImportResult(history.name, updated, importChanges)
  }

  private def downloadHistory(fund: Savings.Fund, chart: Boolean = true): Future[ImportResult] = {
    import epsa.Main.Akka.dispatcher

    val action = DataStore.AssetHistory.readValues(fund.id).flatMap { current =>
      // We want to start from last known date
      val dateStart = current.lastOption.map(_.date)
      if (dateStart.exists(_ < LocalDate.now.minusDays(1))) {
        AmundiInvestmentFundDownloader.download(fund.amfId.get, dateStart = dateStart).map { history =>
          updateHistory(fund, current, history)
        }
      } else {
        // We actually are up to date
        val history = Savings.AssetValueHistory(name = None)
        Future.successful(updateHistory(fund, current, history))
      }
    }

    accessHistory(
      action = action,
      // $1=fund
      failureMsg = Strings.downloadNavHistoryError.format(fund.name),
      successAction = showImportResult(fund, chart)
    )
  }

  private def importHistory(fund: Savings.Fund, history: Savings.AssetValueHistory): Future[ImportResult] = {
    import epsa.Main.Akka.dispatcher

    val action = DataStore.AssetHistory.readValues(fund.id).map { current =>
      updateHistory(fund, current, history)
    }

    accessHistory(
      action = action,
      failureMsg = DataStore.readIssueMsg(),
      successAction = showImportResult(fund, chart = true)
    )
  }

  private def purgeHistory(fund: Savings.Fund): Future[Unit] = {
    accessHistory(
      action = Future.successful(()),
      failureMsg = "",
      successAction = (_: Unit) => {
        changes += fund -> None
        loadHistory(fund)
        ()
      }
    )
  }

  private def onMouseEvent(event: ChartEvent.Value, mouseEvent: MouseEvent, data: ChartSeriesData): Unit = {
    event match {
      case ChartEvent.RightClicked =>
        // Refresh context menu with new item (for currently selected date)
        val header = new CustomMenuItem(new Label(data.date.toString), false)
        header.getStyleClass.addAll(JFXStyles.CLASS_HEADER, JFXStyles.CLASS_NO_SELECT)

        val text = formatCompactNumber(data.value)
        val menuTextField = new TextFieldWithButton("text-field-with-refresh-button")
        val editNAV = new CustomMenuItem(menuTextField, false)
        // Set text, and reset it when requested
        menuTextField.setText(text)
        menuTextField.setOnButtonAction { _ =>
          menuTextField.setText(text)
        }
        // Bind so that changing value allows to reset it
        menuTextField.buttonDisableProperty.bind(menuTextField.textField.textProperty.isEqualTo(text))

        // Check edited value is OK
        def getUserNAV = parseNumber(menuTextField.getText)
        menuTextField.textField.textProperty.listen { _ =>
          val navOk = getUserNAV > 0
          JFXStyles.toggleError(menuTextField, !navOk, Strings.positiveValue)
        }

        // Save edited value if applicable when requested
        menuTextField.textField.setOnAction { _: ActionEvent =>
          val nav = getUserNAV
          if ((menuTextField.getText != text) && (nav > 0) && (nav != data.value)) {
            getFund.foreach { fund =>
              val assetValues = Seq(Savings.AssetValue(data.date, nav))
              // Compute fund changes (all changes relatively to initial history)
              val fundChanges = mergeHistory(updatedHistory(fund, Seq.empty), assetValues)
              changes += fund -> Some(fundChanges)
              // And refresh chart
              chartHandler.updateSeries(assetValues)
            }
          }
          // Finally hide the context menu
          historyChartContextMenu.hide()
        }

        historyChartContextMenu.getItems.setAll(header, new SeparatorMenuItem, editNAV)
        // Display context menu at current mouse position. Enable auto-hide and
        // call the 'show(...)' variant without anchor so that clicking anywhere
        // other than the popup node will make it disappear.
        historyChartContextMenu.setAutoHide(true)
        historyChartContextMenu.show(stage, mouseEvent.getScreenX, mouseEvent.getScreenY)

      case _ =>
        // We don't care
    }
  }

  private def displayChart(fund: Savings.Fund, values: Seq[Savings.AssetValue]): Unit = {
    val actualValues = updatedHistory(fund, values)
    purgeButton.setDisable(actualValues.isEmpty)

    if (actualValues.isEmpty) {
      historyPane.setVisible(false)
    } else {
      // Note: showing the dialog while creating the chart without values and
      // then updating series data (initial fund selection) right away tends to
      // lock up JavaFX (requiring to kill the application), especially when
      // changing the series name (which triggers chart legend updating).
      // So wait for the first series data to show (initial func selection) to
      // create the chart, which prevents the lock up (or at least significantly
      // lowers the chances for it to happen).
      if (chartHandler == null) {
        val meta = ChartMeta[ChartMark](mouseHandler = onMouseEvent)
        chartHandler = new ChartHandler(
          seriesName = fund.name,
          seriesValues = actualValues,
          meta = meta,
          settings = ChartSettings.hidden.copy(
            xLabel = Strings.date,
            yLabel = Strings.nav,
            ySuffix = Main.settings.currency.get
          )
        )
        val chartPane = chartHandler.chartPane
        historyPane.getChildren.add(chartPane)
        AnchorPane.setTopAnchor(chartPane, 0.0)
        AnchorPane.setRightAnchor(chartPane, 0.0)
        AnchorPane.setBottomAnchor(chartPane, 0.0)
        AnchorPane.setLeftAnchor(chartPane, 0.0)
        chartHandler.centerOnDate(actualValues.last.date, track = true)
      } else {
        chartHandler.setSeriesName(fund.name)
        chartHandler.updateSeries(actualValues, replace = true)
        chartHandler.chartPane.setVisible(true)
      }
      historyPane.setVisible(true)
    }
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
      column1.setCellValueFactory(data => {
        new SimpleObjectProperty(data.getValue.date)
      })
      val columns = column1 +: headers.tail.zipWithIndex.map {
        case (header, idx) =>
          val column2 = new TableColumn[AssetEntry, BigDecimal](header)
          column2.setCellValueFactory(data => {
            new SimpleObjectProperty(data.getValue.values(idx))
          })
          column2
      }
      table.getColumns.setAll(columns.asJava)
      table.setItems(FXCollections.observableList(entries.asJava))
    }

    val current: Map[LocalDate, Savings.AssetValue] = result.current.groupBy(_.date).mapValues(_.head).view.force
    val updatedEntries: Seq[AssetEntry] = result.fundChanges.changed.map { changed =>
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
    val updatedTab = tabPane.getTabs.asScala.find(_.getId == "updatedTab").get
    val updatedTable = root.lookup("#updatedTable").asInstanceOf[TableView[AssetEntry]]
    val addedLabel = root.lookup("#addedLabel").asInstanceOf[Label]
    val addedTab = tabPane.getTabs.asScala.find(_.getId == "addedTab").get
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
    Stages.initOwner(resultStage, stage)
    resultStage.setTitle(stage.getTitle)
    val scene = new Scene(root)
    resultStage.setScene(scene)

    Stages.onStageReady(resultStage, first = false) {
      Stages.setMinimumDimensions(stage)
    }(JFXSystem.dispatcher)

    resultStage.show()
  }

  private def getFund: Option[Savings.Fund] =
    Option(fundField.getValue).flatten

}

object NetAssetValueHistoryController {

  private val settingsKeyPrefix = "nav-history"

  private val stageLocation = ConfigEntry.from[StageLocation](Main.settings.settings,
    Settings.KEY_SUIRYC, Settings.KEY_EPSA, Settings.KEY_STAGE, settingsKeyPrefix, Settings.KEY_LOCATION)

  private val navHistoryImportPath = ConfigEntry.from[Path](Main.settings.settings,
    Settings.KEY_SUIRYC, Settings.KEY_EPSA, settingsKeyPrefix, "import", "path")

  def title: String = Strings.navHistory

  /** Builds a dialog out of this controller. */
  def buildDialog(mainController: MainController, savings: Savings, fundId: Option[UUID], window: Window): Dialog[Boolean] = {
    val dialog = new Dialog[Boolean]()
    val stage = Stages.getStage(dialog)
    stage.getIcons.setAll(Images.iconChartUp)
    dialog.setTitle(title)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val loader = new FXMLLoader(getClass.getResource("/fxml/net-asset-value-history.fxml"), I18N.getResources)
    dialog.getDialogPane.setContent(loader.load())
    stage.getScene.getStylesheets.add(getClass.getResource("/css/main.css").toExternalForm)
    JFXStyles.addStylesheet(stage.getScene)
    val controller = loader.getController[NetAssetValueHistoryController]
    controller.initialize(savings, dialog, fundId)

    // Delegate closing request to controller
    Stages.getStage(dialog).setOnCloseRequest(controller.onCloseRequest(dialog) _)

    Dialogs.addPersistence(dialog, controller)

    dialog.setResultConverter(resultConverter(mainController, window, controller) _)

    dialog
  }

  private def resultConverter(mainController: MainController, window: Window, controller: NetAssetValueHistoryController)(buttonType: ButtonType): Boolean = {
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
      val future = executeAllSequentially(stopOnError = false, actions: _*)
      // Wait for result and display issue if any
      Awaits.orError(future, Some(window), DataStore.writeIssueMsg())
      // Request main view to refresh (i.e. check for pending changes)
      mainController.refresh()
      true
    } else {
      false
    }
  }

}
