package epsa.controllers

import epsa.I18N.Strings
import epsa.controllers.MainController._
import epsa.model.{StandardAssetDetails, _}
import epsa.util.{Awaits, JFXStyles}
import java.time.LocalDate
import javafx.collections.FXCollections
import javafx.collections.transformation.SortedList
import javafx.event.ActionEvent
import javafx.scene.control.{SeparatorMenuItem, _}
import javafx.scene.image.ImageView
import javafx.scene.input._
import javafx.scene.layout.AnchorPane
import scala.collection.JavaConversions._
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.event.EventHandler._
import suiryc.scala.javafx.scene.control.TableViews
import suiryc.scala.javafx.util.Callback
import suiryc.scala.math.Ordered._
import suiryc.scala.math.Ordering.localDateOrdering

class SavingsView(tab: SavingsViewTab) {

  private val clipboard = Clipboard.getSystemClipboard

  private val CTRL_C = new KeyCodeCombination(KeyCode.C, KeyCombination.CONTROL_DOWN)

  val assetsTable = new TableView[AssetDetails]()

  val assetFields = AssetField.fields()

  val assetsColumns = assetFields.mapValues(_.column).view.force.toList

  private val columnAmount = new TableColumn[AssetDetails, Nothing](Strings.amount)

  columnAmount.getColumns.addAll(
    assetFields(AssetField.KEY_INVESTED_AMOUNT).column,
    assetFields(AssetField.KEY_GROSS_AMOUNT).column,
    assetFields(AssetField.KEY_LEVIES_AMOUNT).column,
    assetFields(AssetField.KEY_NET_AMOUNT).column
  )

  private val columnGain = new TableColumn[AssetDetails, Nothing](Strings.gain)

  columnGain.getColumns.addAll(
    assetFields(AssetField.KEY_GROSS_GAIN).column,
    assetFields(AssetField.KEY_GROSS_GAIN_PCT).column,
    assetFields(AssetField.KEY_NET_GAIN).column,
    assetFields(AssetField.KEY_NET_GAIN_PCT).column
  )

  // Allow user to show/hide columns
  assetsTable.setTableMenuButtonVisible(true)

  // Note: if using a SortedList as table items, column sorting works out
  // of the box. But wrapping the SortedList does not because the default
  // sort policy does explicitly check for a SortedList which comparator is
  // bound to the table one.
  // Since we will ensure it, override the sort policy.
  assetsTable.setSortPolicy(Callback { true })

  // Note: Asset gives scheme/fund UUID. Since State is immutable (and is
  // changed when applying events in controller) we must delegate scheme/fund
  // lookup to the controller.
  assetsTable.setRowFactory(Callback { newAssetRow() })

  assetsTable.getSelectionModel.selectedItemProperty.listen(updateDetailsValue())

  // Handle 'Ctrl-c' to copy asset information.
  assetsTable.addEventHandler(KeyEvent.KEY_PRESSED, { (event: KeyEvent) =>
    if (CTRL_C.`match`(event)) Option(assetsTable.getSelectionModel.getSelectedItem).foreach(copyAssetDetailsToClipboard)
  })

  def updateDetailsValue(assetDetailsOpt: Option[AssetDetails]): Unit = {
    assetFields.values.foreach(_.updateDetailsValue(assetDetailsOpt))
    val fundOpt = assetDetailsOpt.filter { assetDetails =>
      (assetDetails.kind == AssetDetailsKind.Standard) || (assetDetails.kind == AssetDetailsKind.TotalPerFund)
    }.map(_.fund)
    tab.mainController.showNAVHistory(fundOpt)
  }

  def updateDetailsValue(): Unit = {
    val assetDetailsOpt = Option(assetsTable.getSelectionModel.getSelectedItem)
    updateDetailsValue(assetDetailsOpt)
  }

  /**
   * Creates a new Asset table view row.
   *
   * Binds context menu when applicable.
   */
  private def newAssetRow(): TableRow[AssetDetails] = {
    val row = new TableRow[AssetDetails]()

    // See: https://www.marshall.edu/genomicjava/2013/12/30/javafx-tableviews-with-contextmenus/
    // Note: only have context menu on 'today' savings tab.
    val contextMenu = if (tab.dateOpt.nonEmpty) null
    else {
      val menu = new ContextMenu()
      val editScheme = new MenuItem(Strings.editScheme,
        new ImageView(Images.iconTables))
      editScheme.setOnAction { (event: ActionEvent) =>
        Option(row.getItem).foreach { details =>
          tab.mainController.actor ! OnEditSchemes(Some(details.asset.schemeId))
        }
      }
      val editFund = new MenuItem(Strings.editFund,
        new ImageView(Images.iconTable))
      editFund.setOnAction { (event: ActionEvent) =>
        Option(row.getItem).foreach { details =>
          tab.mainController.actor ! OnEditFunds(Some(details.asset.fundId))
        }
      }

      val newPayment = new MenuItem(Strings.newPayment,
        new ImageView(Images.iconTableImport))
      newPayment.setOnAction { (event: ActionEvent) =>
        Option(row.getItem).foreach { details =>
          tab.mainController.actor ! OnNewAssetAction(AssetActionKind.Payment, Some(details.asset))
        }
      }
      val newArbitrage = new MenuItem(Strings.newTransfer,
        new ImageView(Images.iconTablesRelation))
      newArbitrage.setOnAction { (event: ActionEvent) =>
        Option(row.getItem).foreach { details =>
          tab.mainController.actor ! OnNewAssetAction(AssetActionKind.Transfer, Some(details.asset))
        }
      }
      val newRefund = new MenuItem(Strings.newRefund,
        new ImageView(Images.iconTableExport))
      newRefund.setOnAction { (event: ActionEvent) =>
        Option(row.getItem).foreach { details =>
          tab.mainController.actor ! OnNewAssetAction(AssetActionKind.Refund, Some(details.asset))
        }
      }

      val navHistory = new MenuItem(NetAssetValueHistoryController.title,
        new ImageView(Images.iconChartUp))
      navHistory.setOnAction { (event: ActionEvent) =>
        Option(row.getItem).foreach { details =>
          tab.mainController.actor ! OnNetAssetValueHistory(Some(details.asset.fundId))
        }
      }

      menu.getItems.addAll(editScheme, editFund, new SeparatorMenuItem(),
        newPayment, newArbitrage, newRefund, new SeparatorMenuItem(),
        navHistory)
      menu
    }

    // Apply appropriate context menu (and style) according to actual row item
    row.itemProperty.listen { v =>
      Option(v) match {
        case Some(item) =>
          val (total, partialTotal, first) = item.kind match {
            case AssetDetailsKind.Standard             => (false, false, item.first)
            case AssetDetailsKind.TotalPartial         => (false, true, item.first)
            case AssetDetailsKind.TotalPerFund         => (false, true, item.first)
            case AssetDetailsKind.TotalPerAvailability => (false, true, item.first)
            case AssetDetailsKind.Total                => (true, false, item.first)
          }
          JFXStyles.togglePseudoClass(row, "row-total", set = total)
          JFXStyles.togglePseudoClass(row, "row-total-partial", set = partialTotal)
          JFXStyles.togglePseudoClass(row, "first", set = first)
          if (item.kind != AssetDetailsKind.Standard) row.setContextMenu(null)
          else row.setContextMenu(contextMenu)

        case None =>
          JFXStyles.togglePseudoClass(row, "row-total", set = false)
          JFXStyles.togglePseudoClass(row, "row-total-partial", set = false)
          JFXStyles.togglePseudoClass(row, "first", set = false)
          row.setContextMenu(null)
      }
    }

    row
  }

  /** Gets currently selected asset if any. */
  def getSelectedAsset: Option[Savings.Asset] =
    Option(assetsTable.getSelectionModel.getSelectedItem).map(_.asset)

  /** Gets (computes) given asset details. */
  private def getAssetDetails(asset: Savings.Asset, data: RefreshData): AssetDetails = {
    val state = data.state
    val savings = state.savingsUpd

    // Note: it is expected that we have an asset because there is an invested
    // amount. So there is no need to try to prevent division by 0.
    val actualVWAP =
      if (data.vwapPerAsset) None
      else savings.assets.vwaps.get(asset.id)
    StandardAssetDetails(
      savings = savings,
      asset = asset,
      scheme = savings.getScheme(asset.schemeId),
      fund = savings.getFund(asset.fundId),
      date = state.assetsValue.get(asset.fundId).map(_.date),
      nav = state.assetsValue.get(asset.fundId).map(_.value),
      actualVWAP,
      availabilityBase = tab.getDateOpt(data)
    )
  }

  /** Copy asset details to clipboard. */
  private def copyAssetDetailsToClipboard(details: AssetDetails): Unit = {
    val text = assetFields.values.flatMap { field =>
      Option(field.format(details)) match {
        case Some(value) => Some(s"${field.detailsLabel} $value")
        case None        => None
      }
    }.mkString("", "\n", "\n")

    val content = new ClipboardContent()
    content.putString(text)
    clipboard.setContent(content)
  }

  def displaySavings(data: RefreshData): Unit = {
    val savings = data.state.savingsUpd

    // Then update table content: takes care of added/removed entries
    val dateOpt = tab.getDateOpt(data)
    val assets = savings.computeAssets(dateOpt.getOrElse(LocalDate.now)).assets.list
    // Get details and sort by scheme, fund then availability by default
    // See: http://stackoverflow.com/a/10027682
    val assetsDetails = assets.map(getAssetDetails(_, data)).sortBy { details =>
      (details.scheme.name, details.fund.name, details.asset.availability)
    }
    val sortedAssetsDetails = new SortedList(FXCollections.observableList(assetsDetails))
    sortedAssetsDetails.comparatorProperty.bind(assetsTable.comparatorProperty)
    val sortedAssetsWithTotal = new AssetDetailsWithTotal(
      savings,
      sortedAssetsDetails,
      data.vwapPerAsset,
      data.showTotalsPerScheme,
      data.showTotalsPerFund,
      data.showTotalsPerAvailability,
      availabilityBase = dateOpt
    )
    // Bind (and first set) our total comparator to the table comparator
    sortedAssetsWithTotal.comparatorProperty.setValue(assetsTable.getComparator)
    sortedAssetsWithTotal.comparatorProperty.bind(assetsTable.comparatorProperty)
    // It is better (up to JavaFX 8) to unbind the previous SortedList
    // comparator if any. The previous list will eventually get GCed, but not
    // the binding itself.
    // See: http://bugs.java.com/bugdatabase/view_bug.do?bug_id=8089305
    Option(assetsTable.getItems).foreach {
      case items: AssetDetailsWithTotal =>
        items.getSource.asInstanceOf[SortedList[AssetDetails]].comparatorProperty.unbind()
      case _ =>
    }
    // Note: TableView.setItems clears the sort order if items are not in a
    // SortedList.
    TableViews.setItems(assetsTable, sortedAssetsWithTotal)
  }

}

class SavingsViewTab(val mainController: MainController, val dateOpt: Option[LocalDate]) extends TabWithState {

  val view = new SavingsView(this)

  private val anchorPane = new AnchorPane()
  anchorPane.getChildren.add(view.assetsTable)
  AnchorPane.setTopAnchor(view.assetsTable, 10.0)
  AnchorPane.setRightAnchor(view.assetsTable, 10.0)
  AnchorPane.setBottomAnchor(view.assetsTable, 10.0)
  AnchorPane.setLeftAnchor(view.assetsTable, 10.0)
  val tab = new Tab()
  tab.setClosable(dateOpt.isDefined)
  tab.setContent(anchorPane)
  tab.setUserData(this)

  def getDateOpt(savings: Savings, upToDateAssets: Boolean): Option[LocalDate] =
    if (dateOpt.isDefined) dateOpt
    else if (upToDateAssets) None
    else savings.latestAssetAction

  private[epsa] def getDateOpt(data: RefreshData): Option[LocalDate] =
    getDateOpt(data.state.savingsUpd, data.upToDateAssets)

  override def refresh(data: RefreshData): Unit = {
    val state = data.state
    getDateOpt(data) match {
      case Some(date) => tab.setText(Strings.savingsOnDateTab.format(date))
      case None       => tab.setText(Strings.savings)
    }
    val actualData = dateOpt match {
      case Some(date) =>
        // Compute actual state from account history up to requested date
        val owner = Some(state.stage)
        val history = Awaits.getEventsHistory(owner, upTo = Some(date))
        val savings = Savings(levies = state.savingsUpd.levies).processEvents(history)
        val eventsNAVs = Savings.getEventsNAVs(history)
        val navs = savings.getNAVs(owner, date, eventsNAVs)
        val actualState = state.copy(
          savingsInit = savings,
          savingsUpd = savings,
          assetsValue = navs
        )
        data.copy(state = actualState)

      case None =>
        data
    }
    view.displaySavings(actualData)
  }

}
