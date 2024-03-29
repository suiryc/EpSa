package epsa.controllers

import com.typesafe.scalalogging.StrictLogging
import epsa.{I18N, Main, Settings}
import epsa.I18N.Strings
import epsa.Settings._
import epsa.model.Savings
import epsa.util.{Awaits, JFXStyles}
import javafx.event.ActionEvent
import javafx.collections.FXCollections
import javafx.fxml.{FXML, FXMLLoader}
import javafx.geometry.Side
import javafx.scene.Node
import javafx.scene.control._
import javafx.scene.image.ImageView
import javafx.scene.layout.HBox
import javafx.stage.{Modality, Stage, Window}
import javafx.util.converter.LocalDateStringConverter
import suiryc.scala.javafx.stage.Stages.StageLocation
import suiryc.scala.math.Ordered._
import suiryc.scala.settings.ConfigEntry
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.scene.control.{DatePickers, Dialogs, TextFieldWithButton}
import suiryc.scala.javafx.stage.{StageLocationPersistentView, Stages}

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.annotation.{nowarn, unused}
import scala.jdk.CollectionConverters._

class NewAssetActionController extends StageLocationPersistentView(NewAssetActionController.stageLocation) with StrictLogging {

  import Main.settings.{scaleAmount, scaleUnits, scalePercents}
  import NewAssetActionController._

  @FXML
  protected var actionKindGroup: ToggleGroup = _

  @FXML
  protected var paymentButton: RadioButton = _

  @FXML
  protected var transferButton: RadioButton = _

  @FXML
  protected var refundButton: RadioButton = _

  @FXML
  protected var amountBox: HBox = _

  @FXML
  protected var operationDateField: DatePicker = _

  @FXML
  protected var dstOperationDateField: DatePicker = _

  @FXML
  protected var latestDateButton: Button = _

  @FXML
  protected var srcFundField: ComboBox[Option[SchemeAndFund]] = _

  @FXML
  protected var srcAvailabilityBox: HBox = _

  @FXML
  protected var srcAvailabilityField: DatePicker = _

  @FXML
  protected var srcUnavailabilityPeriodButton: Button = _

  @FXML
  protected var srcAvailabilityField2: ComboBox[Option[LocalDate]] = _

  @FXML
  protected var srcNAVField: TextFieldWithButton = _

  @FXML
  protected var srcNAVButton: Button = _

  @FXML
  protected var srcAmountField: TextField = _

  @FXML
  protected var srcEmptyButton: Button = _

  @FXML
  protected var srcUnitsField: TextField = _

  @FXML
  protected var dstFundField: ComboBox[Option[SchemeAndFund]] = _

  @FXML
  protected var dstAvailabilityField: DatePicker = _

  @FXML
  protected var dstUnavailabilityPeriodButton: Button = _

  @FXML
  protected var dstNAVField: TextFieldWithButton = _

  @FXML
  protected var dstNAVButton: Button = _

  @FXML
  protected var dstAmountField: TextField = _

  @FXML
  protected var dstUnitsField: TextField = _

  @FXML
  protected var dstUnitsAutoButton: ToggleButton = _

  @FXML
  protected var commentField: TextArea = _

  protected var buttonOk: Node = _

  private var mainController: MainController = _

  lazy protected val stage: Stage = paymentButton.getScene.getWindow.asInstanceOf[Stage]

  lazy private val toggleButtons = List(paymentButton, transferButton, refundButton)

  private var savings: Savings = _

  private var actualSavings: Savings = _

  private var actualSavingsDate: LocalDate = _

  private var actionKind: AssetActionKind.Value = _

  private var recursionLevel = 0

  private var dstAvailabilityChosen = false

  def initialize(mainController0: MainController, savings0: Savings, dialog: Dialog[_], actionKind0: AssetActionKind.Value, asset: Option[Savings.Asset]): Unit = {
    // Save initial state
    mainController = mainController0
    savings = savings0
    actionKind = actionKind0

    val unavailabilityPeriods = Awaits.readDataStoreUnavailabilityPeriods(stage).getOrElse(Seq.empty).sortBy(_.id)

    // Load css
    JFXStyles.addStylesheet(stage.getScene)

    // Lookup OK dialog button
    buttonOk = dialog.getDialogPane.lookupButton(ButtonType.OK)
    // Disable OK button unless there are events to take into account
    buttonOk.setDisable(true)

    // Associate action kind to related toggle button
    paymentButton.setUserData(AssetActionKind.Payment)
    transferButton.setUserData(AssetActionKind.Transfer)
    refundButton.setUserData(AssetActionKind.Refund)

    // Setup funds buttons.
    // Some things must be set before first selecting (toggle) operation kind
    // as we will then disable destination fields when applicable.
    for (field <- List(srcNAVButton, srcEmptyButton, dstNAVButton, dstUnitsAutoButton)) {
      // Disable by default; will be enabled when a fund is selected
      field.setDisable(true)
    }
    latestDateButton.setDisable(savings.latestAssetAction.isEmpty)
    for ((button, field) <- List(
      (srcUnavailabilityPeriodButton, srcAvailabilityField),
      (dstUnavailabilityPeriodButton, dstAvailabilityField)
    )) {
      button.setDisable(unavailabilityPeriods.isEmpty)
      // Apply selected unavailability period when requested
      if (unavailabilityPeriods.nonEmpty) {
        val contextMenu = new ContextMenu()
        unavailabilityPeriods.foreach { period =>
          val menuItem = new MenuItem(period.id)
          menuItem.setOnAction { _: ActionEvent =>
            getOperationDate.foreach { date =>
              val (month, dayOfMonth) = period.month match {
                case Some(v) => (v, 1)
                case None    => (date.getMonth, date.getDayOfMonth)
              }
              val availability = date.plusYears(period.years.toLong).withMonth(month.getValue).withDayOfMonth(dayOfMonth)
              field.setValue(availability)
            }
          }
          contextMenu.getItems.add(menuItem)
        }
        button.setOnAction { _ =>
          contextMenu.show(button, Side.RIGHT, 0, 0)
        }
      }
    }

    // Listen to action kind change
    actionKindGroup.selectedToggleProperty.listen(onToggleKind())
    // Select initial toggle button
    toggleButtons.find(getToggleKind(_) == actionKind).foreach(actionKindGroup.selectToggle)

    // Listen to operation date changes
    List(operationDateField, dstOperationDateField).foreach { field =>
      field.valueProperty.listen(onOperationDate())
    }

    // Listen to fund changes
    srcFundField.valueProperty.listen(onSrcFund())
    dstFundField.valueProperty.listen(onDstFund())

    // Note: we need to tell the combobox how to display both the 'button' area
    // (what is shown as selected) and the content (list of choices).
    for (field <- List(srcFundField, dstFundField)) {
      field.setButtonCell(new SchemeAndFundCell)
      field.setCellFactory(_ => new SchemeAndFundCell)
    }

    // Listen to availability date changes
    srcAvailabilityField.valueProperty.listen(onSrcAvailability())
    srcAvailabilityField2.getSelectionModel.selectedItemProperty.listen(onSrcAvailability())
    dstAvailabilityField.valueProperty.listen(onDstAvailability())

    // Note: we set the availability combobox format now and change it later
    // if operation date if changed.
    srcAvailabilityField2.setButtonCell(new AvailabilityListCell(None))
    srcAvailabilityField2.setCellFactory(_ => new AvailabilityListCell(None))

    // Force date format (to match the one of LocalDate in other views) in date picker fields
    val dateFormat = "yyyy-MM-dd"
    val dateConverter = new LocalDateStringConverter(DateTimeFormatter.ofPattern(dateFormat), null)
    for (field <- List(operationDateField, dstOperationDateField, srcAvailabilityField, dstAvailabilityField)) {
      field.setPromptText(dateFormat)
      field.setConverter(dateConverter)
      // Track field editor to apply value when possible
      DatePickers.trackEdition(field)
    }
    // Override availability date pickers to disable dates anterior to operation date.
    for (field <- List(srcAvailabilityField, dstAvailabilityField)) {
      field.setDayCellFactory(_ => {
        new DateCell {
          override def updateItem(item: LocalDate, empty: Boolean): Unit = {
            super.updateItem(item, empty)
            getOperationDate.foreach { operationDate =>
              val unavailable = !empty && (item < operationDate)
              setDisable(unavailable)
              // Note: ideally we would like to set a tooltip explaining why a
              // date is not available; but this is not possible on a disabled
              // node (see: https://bugs.openjdk.java.net/browse/JDK-8090379).
              if (!unavailable) setStyle("")
              else setStyle("-fx-background-color: #ffc0cb;")
            }
          }
        }:DateCell
      })
    }

    // Setup NAV history buttons
    for (field <- List(srcNAVButton, dstNAVButton)) {
      field.setTooltip(new Tooltip(NetAssetValueHistoryController.title))
      field.setOnAction { _ =>
        val opt =
          if (field == dstNAVButton) getDstFund
          else getSrcFund
        opt.foreach { schemeAndFund =>
          onNAVHistory(schemeAndFund.fund)
        }
      }
    }

    // Setup src/dst NAV/amount/units field listeners
    for ((field, cb) <- List(
      (srcNAVField.textField, () => onSrcNAV()),
      (srcAmountField, () => onSrcAmount()),
      (srcUnitsField, () => onSrcUnits()),
      (dstNAVField.textField, () => onDstNAV()),
      (dstUnitsField, () => onDstUnits())
    )) {
      field.textProperty.listen(cb())
    }

    // Setup dst units auto button: select if necessary, and compute once
    // selected.
    // Note: behave as if counterpart value was modified ('initRecursion'), so
    // that recursion can be broken as expected.
    dstUnitsAutoButton.setSelected(dstUnitsAuto.get)
    dstUnitsAutoButton.selectedProperty.listen { selected =>
      if (selected) onSrcAmount()
    }

    // Really make sure we don't leave if something is not OK
    buttonOk.addEventFilter(ActionEvent.ACTION, (event: ActionEvent) => {
      if (checkForm().isEmpty) event.consume()
    })

    // Set now as operation date default
    operationDateField.setValue(LocalDate.now)
    dstOperationDateField.setValue(null)

    // Select initial scheme and fund (as source)
    for {
      a <- asset
      scheme <- savings.findScheme(a.schemeId)
      fund <- savings.findFund(a.fundId)
    } {
      srcFundField.getSelectionModel.select(Some(SchemeAndFund(scheme, fund)))
      // There is no meaning to pre-select source availability in case of
      // payment. In other cases, use the source asset one.
      if (actionKind != AssetActionKind.Payment) {
        srcAvailabilityField2.getSelectionModel.select(a.availability)
      }
    }

    checkForm()
    ()
  }

  /** Persists view (stage location, ...). */
  override protected def persistView(): Unit = {
    super.persistView()
    dstUnitsAuto.set(dstUnitsAutoButton.isSelected)
  }

  private def onToggleKind(): Unit = {
    actionKind = getToggleKind(actionKindGroup.getSelectedToggle)

    // @nowarn workarounds scala 2.13.x false-positive
    val (icon, title) = (actionKind: @nowarn) match {
      case AssetActionKind.Payment  => (Images.iconTableImport, Strings.payment)
      case AssetActionKind.Transfer => (Images.iconTablesRelation, Strings.transfer)
      case AssetActionKind.Refund   => (Images.iconTableExport, Strings.refund)
    }
    stage.getIcons.setAll(icon)
    stage.setTitle(title)

    if (actionKind == AssetActionKind.Refund) {
      computeLevies(getSrcAmount)
    } else {
      amountBox.getChildren.clear()
    }

    val disableDst = !isDstEnabled
    dstOperationDateField.setDisable(disableDst)
    dstFundField.setDisable(disableDst)
    dstAvailabilityField.setDisable(disableDst)
    if (Option(dstUnavailabilityPeriodButton.getOnAction).isDefined) dstUnavailabilityPeriodButton.setDisable(disableDst)
    dstNAVField.setDisable(disableDst)
    dstAmountField.setDisable(disableDst)
    dstUnitsField.setDisable(disableDst)
    if (isDstEnabled) dstAmountField.setText(srcAmountField.getText)
    // Reset user choice of destination availability when changing to action
    // other than transfer.
    if (disableDst) dstAvailabilityChosen = false

    val srcAvailabilityExact = actionKind != AssetActionKind.Payment
    srcAvailabilityBox.setVisible(!srcAvailabilityExact)
    srcAvailabilityField2.setVisible(srcAvailabilityExact)

    // Updating comboboxes usually cleans and re-sets selected value. We want to
    // disabling handlers while we do that, then call them manually.
    breakRecursion {
      updateSchemeAndFund()
    }
    onSrcFund()
    if (!disableDst) {
      onSrcAvailability()
      onDstFund()
    }
    checkForm()
    ()
  }

  private def onOperationDate(): Unit = {
    updateSrcAvailability()
    updateNAV()
    checkForm()
    ()
  }

  def onLatestDate(@unused event: ActionEvent): Unit = {
    savings.latestAssetAction.foreach { date =>
      operationDateField.setValue(date)
    }
  }

  // Breaks recursion triggered from changing action kind.
  private def onSrcFund(): Unit = breakRecursion {
    val srcAvailabilityExact = actionKind != AssetActionKind.Payment

    if (srcAvailabilityExact) updateSrcAvailability()
    updateDstSchemeAndFund()
    updateNAV()
    checkForm()
  }

  // Note: use 'breakRecursion' together with 'onDstAvailability' so that we
  // can differentiate whether value was auto-set from source or whether user
  // did select a value (in which case we don't auto-set it anymore).
  // Also breaks recursion triggered from changing action kind.
  private def onSrcAvailability(): Unit = breakRecursion {
    updateDstAvailability()
    checkForm()
  }

  private def onSrcNAV(): Unit = {
    computeSrcAmount()
    checkForm()
    ()
  }

  private def onSrcAmount(): Unit = {
    val value = getSrcAmount
    if ((value > 0) && isDstEnabled && dstUnitsAutoButton.isSelected) {
      val dstNAV = getDstNAV
      if (dstNAV > 0) {
        val dstUnits = scaleUnits(value / dstNAV)
        dstUnitsField.setText(formatCompactNumber(dstUnits))
      }
    }

    computeLevies(value)
  }

  private def onSrcUnits(): Unit = {
    computeSrcAmount()
    checkForm()
    ()
  }

  def onSrcEmpty(@unused event: ActionEvent): Unit = {
    for {
      operationDate <- getOperationDate
      schemeAndFund <- getSrcFund
    } {
      val srcAvailability = getSrcAvailability
      val searchAsset = Savings.Asset(schemeAndFund.scheme.id, schemeAndFund.fund.id, srcAvailability, 0, 0)
      getSavings(operationDate).findAsset(operationDate, searchAsset).foreach { asset =>
        // Note: setting the units will trigger amount computing and update the field
        srcUnitsField.setText(formatCompactNumber(asset.units))
      }
    }
  }

  private def onDstFund(): Unit = {
    updateNAV(updateSrc = false)
    // Trigger units/amount computation from source amount
    onSrcAmount()
    checkForm()
    ()
  }

  private def onDstAvailability(): Unit = breakRecursion {
    dstAvailabilityChosen = Option(dstAvailabilityField.getValue).isDefined
    checkForm()
  }

  private def onDstNAV(): Unit = {
    computeDstAmount()
    checkForm()
    ()
  }

  private def onDstUnits(): Unit = {
    computeDstAmount()
    checkForm()
    ()
  }

  private def onNAVHistory(fund: Savings.Fund): Unit = {
    val dialog = NetAssetValueHistoryController.buildDialog(mainController, savings, Some(fund.id), stage)
    dialog.initModality(Modality.WINDOW_MODAL)
    Stages.initOwner(dialog, stage)
    dialog.setResizable(true)
    if (dialog.showAndWait().orElse(false)) updateNAV()
  }

  private def computeAmount(units: BigDecimal, nav: BigDecimal, amountField: TextField): Unit = {
    if ((units > 0) && (nav > 0)) {
      val amount = scaleAmount(units * nav)
      amountField.setText(formatCompactNumber(amount))
    }
  }

  private def computeSrcAmount(): Unit = {
    computeAmount(getSrcUnits, getSrcNAV, srcAmountField)
  }

  private def computeDstAmount(): Unit = {
    computeAmount(getDstUnits, getDstNAV, dstAmountField)
  }

  private def updateSchemeAndFund(): Unit = {
    // Note: previous selected value is kept if still present in new items

    // Scheme&fund with asset
    val fundsWithAsset = savings.assets.list.map { asset =>
      val scheme = savings.getScheme(asset.schemeId)
      val fund = savings.getFund(asset.fundId)
      SchemeAndFund(scheme, fund)
    }.distinct.sorted

    // Note:
    // If grid columns have 'computed' preferred width, the ones which items
    // are changed usually gets resized accordingly, which may not look nice.
    // Changing both source/destination items often workaround this, but not
    // always. The best solution is to have an explicit preferred width so
    // that it does not get recomputed according to content.
    if (actionKind == AssetActionKind.Payment) {
      // Other scheme&fund (filter disabled ones)
      val fundsOther = savings.schemes.flatMap { scheme =>
        scheme.funds.map { fundId =>
          val fund = savings.getFund(fundId)
          SchemeAndFund(scheme, fund)
        }
      }.filterNot { schemeAndFund =>
        schemeAndFund.scheme.disabled ||
          schemeAndFund.fund.disabled ||
          fundsWithAsset.contains(schemeAndFund)
      }.sorted

      // Choices are listed by order of 'preference' (chances to be chosen),
      // with a separator between categories:
      //   - funds that already have assets
      //   - funds without assets
      srcFundField.setItems(FXCollections.observableList(Form.buildOptions(fundsWithAsset, fundsOther).asJava))
    } else {
      srcFundField.setItems(FXCollections.observableList(Form.buildOptions(fundsWithAsset).asJava))
    }
    updateDstSchemeAndFund()
  }

  private def updateDstSchemeAndFund(): Unit = {
    // Notes: don't empty destination list to keep selected value if needed.
    // Fields are disabled and the selected item will remain if order is
    // changed next time the fields are enabled.
    //
    // If grid columns have 'computed' preferred width, the ones which items
    // are changed usually gets resized accordingly, which may not look nice.
    // Changing both source/destination items often workaround this, but not
    // always. The best solution is to have an explicit preferred width so
    // that it does not get recomputed according to content.
    if (isDstEnabled) {
      // Scheme&fund with asset
      val fundsWithAsset = savings.assets.list.map { asset =>
        val scheme = savings.getScheme(asset.schemeId)
        val fund = savings.getFund(asset.fundId)
        SchemeAndFund(scheme, fund)
      }.distinct.sorted
      // Other scheme&fund (filter disabled ones)
      val fundsOther = savings.schemes.flatMap { scheme =>
        scheme.funds.map { fundId =>
          val fund = savings.getFund(fundId)
          SchemeAndFund(scheme, fund)
        }
      }.filterNot { schemeAndFund =>
        schemeAndFund.scheme.disabled ||
          schemeAndFund.fund.disabled ||
          fundsWithAsset.contains(schemeAndFund)
      }.sorted
      // Other scheme&fund for destination, with first same scheme as source if
      // selected
      val fundsDst = getSrcFund.map { schemeAndFund =>
        val (fundsSameScheme1, fundsOtherScheme1) = fundsWithAsset.filterNot(_ == schemeAndFund).partition(_.scheme == schemeAndFund.scheme)
        val (fundsSameScheme2, fundsOtherScheme2) = fundsOther.partition(_.scheme == schemeAndFund.scheme)
        Form.buildOptions(fundsSameScheme1, fundsSameScheme2, fundsOtherScheme1, fundsOtherScheme2)
      }.getOrElse(Form.buildOptions(fundsWithAsset, fundsOther))

      // Choices are listed by order of 'preference' (chances to be chosen),
      // with a separator between categories:
      //   - funds in the source scheme that already have assets
      //   - funds in the source scheme without assets
      //   - funds in another scheme that already have assets
      //   - funds in another scheme without assets
      dstFundField.setItems(FXCollections.observableList(fundsDst.asJava))
    }
  }

  private def updateSrcAvailability(): Unit = {
    getOperationDate match {
      case Some(date) =>
        // Note: changing the combobox format appears to be taken into account
        // right away (unlike TableView). Probably because the concerned content
        // is drawn when necessary while the table has some already shown.
        srcAvailabilityField2.setButtonCell(new AvailabilityListCell(Some(date)))
        srcAvailabilityField2.setCellFactory(_ => new AvailabilityListCell(Some(date)))

        srcAvailabilityField2.setDisable(false)
        // Note: get availabilities for selected scheme&fund, sorted by date (with
        // immediate availability first).
        val availabilities = getSrcFund.map { schemeAndFund =>
          getSavings(date).assets.byId.getOrElse(schemeAndFund.id, Nil).map(_.availability).distinct.sortBy { opt =>
            opt.getOrElse(LocalDate.ofEpochDay(0))
          }
        }.getOrElse(Nil)
        srcAvailabilityField2.setItems(FXCollections.observableList(availabilities.asJava))
        if (availabilities.lengthCompare(1) == 0) {
          srcAvailabilityField2.getSelectionModel.select(availabilities.head)
          // Manually update dst availability (overrides recursion prevention)
          updateDstAvailability()
        }

      case None =>
        srcAvailabilityField2.setDisable(true)
    }
  }

  private def updateDstAvailability(): Unit = {
    if (!dstAvailabilityChosen && isDstEnabled) {
      // Note: don't forget to use actual availability based on operation date
      val operationDate = getOperationDate
      val availability = Savings.resolveAvailability(getSrcAvailability, operationDate)
      dstAvailabilityField.setValue(availability.orElse(operationDate).orNull)
    }
  }

  private def updateNAV(updateSrc: Boolean = true): Unit = {
    def updateField(field: TextFieldWithButton, fund: Savings.Fund, operationDate: LocalDate, extraDate: LocalDate*): Unit = {
      // There may be two dates (for destination fund, source and destination date).
      // We want to remember any NAV for the wanted date(s).
      val dates = Set(operationDate) ++ extraDate
      val navs = dates.toList.flatMap { date =>
        Awaits.readDataStoreNAV(Some(stage), fund.id, date).getOrElse(None).map(date -> _)
      }.toMap
      val userData = if (navs.nonEmpty) navs else null
      // The operationDate dictates what is displayed in the field.
      // We still remember retrieved NAVs if any.
      navs.get(operationDate) match {
        case Some(nav) =>
          val text = formatCompactNumber(nav.value)
          field.setUserData(userData)
          field.setText(text)
          field.textField.setTooltip(new Tooltip(s"${Strings.date}: ${nav.date}\n${Strings.nav}: ${formatNumber(nav.value, Main.settings.currency.get)}"))
          field.setOnButtonAction { _ =>
            field.setText(text)
          }
          // Bind so that changing value allows to reset it
          field.buttonDisableProperty.bind(field.textField.textProperty.isEqualTo(text))
          // Forbid changing NAV value if we found one for this exact date: we
          // only allow updating NAV history for missing dates. This also
          // prevents accidentally changing the field value.
          field.textField.setEditable(nav.date != operationDate)

        case None =>
          field.setUserData(userData)
          field.setText(null)
          field.textField.setTooltip(null)
          field.buttonDisableProperty.unbind()
          field.setOnButtonAction(null)
          field.setButtonDisable(true)
          field.textField.setEditable(true)
      }
    }

    getOperationDate.foreach { operationDate =>
      if (updateSrc) getSrcFund.foreach { schemeAndFund =>
        updateField(srcNAVField, schemeAndFund.fund, operationDate)
      }
      getDstFund.foreach { schemeAndFund =>
        updateField(dstNAVField, schemeAndFund.fund, getDstOperationDate.getOrElse(operationDate), operationDate)
        // Manually trigger src amount callback in order to recompute dst
        // units if necessary.
        if (updateSrc) onSrcAmount()
      }
    }
  }

  private def computeLevies(grossAmount: BigDecimal): Unit = {
    if (actionKind == AssetActionKind.Refund) {
      getSrcFund.foreach { schemeAndFund =>
        val amount = getSrcAmount
        val units = getSrcUnits
        if ((amount > 0) && (units > 0)) {
          for {
            operationDate <- getOperationDate
            srcAvailability = getSrcAvailability
            searchAsset = Savings.Asset(schemeAndFund.scheme.id, schemeAndFund.fund.id, srcAvailability, 0, 0)
            savings = getSavings(operationDate)
            asset <- savings.findAsset(operationDate, searchAsset)
          } {
            val totalUnits = savings.assets.units(schemeAndFund.id)
            val leviesPeriodsData = savings.computeLevies(schemeAndFund.id, operationDate, getSrcNAV)
            val (refundLevies, _) = leviesPeriodsData.proportioned(units / totalUnits)
            val currency = Main.settings.currency.get
            val investedAmount = scaleAmount((asset.amount(savings.assets.vwaps(asset.id)) * units) / asset.units)
            val grossGain = grossAmount - investedAmount
            val leviesAmount = refundLevies.amount
            val leviesPct = scalePercents(leviesAmount * 100 / grossGain)
            if (savings.hasLevies) {
              lazy val msg = s"action=<refund> date=<$operationDate> id=<${schemeAndFund.id.toString(savings)}> nav=<$getSrcNAV> totalUnits=<$totalUnits> units=<$units> investedAmount=<$investedAmount> grossAmount=<$grossAmount> grossGain=<$grossGain> refundLevies=<$refundLevies> leviesAmount=<${refundLevies.amount}> leviesPct=<$leviesPct>"
              if (Settings.debug(Debug.LeviesHistory))
                logger.info(s"$msg debugInfo=<\n${refundLevies.debugInfo}\n>")
              else if (Settings.debug(Debug.LeviesComputation))
                logger.info(msg)
            }
            val warnings =
              if (refundLevies.warnings.isEmpty) ""
              else refundLevies.warnings.mkString("\n\n", "\n", "")
            // $1 = gross gain $2 = levies amount $3 = levies global rate
            val msg =
              Strings.leviesEstimation.format(
                formatNumber(grossGain, currency),
                formatNumber(leviesAmount, currency),
                formatNumber(leviesPct, "%")
              ) + warnings
            val node = new ImageView(Images.iconMoneyCoin)
            Tooltip.install(node, new Tooltip(msg))
            amountBox.getChildren.setAll(node)
            amountBox.setVisible(true)
          }
        }
      }
    }
  }

  private def checkForm(): Option[Savings.AssetEvent] = {
    val operationDate = operationDateField.getValue
    val dstOperationDate = dstOperationDateField.getValue
    val opDateSelected = Option(operationDate).isDefined
    val opDstDateSelected = Option(dstOperationDate).isDefined
    val opDateAnterior = opDateSelected && savings.latestAssetAction.exists(operationDate < _)
    val opDstDateAnterior = opDateSelected && opDstDateSelected && (dstOperationDate < operationDate)
    // Selecting a date of operation anterior to the latest asset action date is allowed even if discouraged
    val opDateOk = opDateSelected && !opDstDateAnterior
    val warningMsgOpt = savings.latestAssetAction.map(Strings.anteriorOpDate.format(_))
    JFXStyles.toggleStyles(operationDateField, None,
      JFXStyles.ErrorStyle(!opDateSelected, Strings.mandatoryField),
      JFXStyles.WarningStyle(opDateAnterior, warningMsgOpt.getOrElse(""))
    )
    JFXStyles.toggleError(dstOperationDateField, opDstDateAnterior, Strings.anteriorDstOpDate)

    val isPayment = actionKind == AssetActionKind.Payment
    val srcFund = getSrcFund.orNull
    val srcSelected = Option(srcFund).isDefined
    val srcAvailability = getSrcAvailability
    val srcAvailabilitySelected =
      (isPayment && Option(srcAvailabilityField.getValue).isDefined) ||
      (!isPayment && Option(srcAvailabilityField2.getValue).isDefined)
    val srcAvailabilityAnterior = opDateSelected && srcAvailability.exists(_ < operationDate)
    val srcNAV = getSrcNAV
    val srcNAVValued = srcNAV > 0
    val srcUnits = getSrcUnits
    val srcUnitsValued = srcUnits > 0
    lazy val srcAsset = Savings.AssetPart(srcFund.scheme.id, srcFund.fund.id, srcAvailability, srcUnits, getSrcNAV)
    val srcAvailableAsset =
      if (isPayment || !opDateSelected || !srcSelected || !srcAvailabilitySelected) None
      else getSavings(operationDate).findAsset(operationDate, srcAsset)
    val srcUnitsPrompt = srcAvailableAsset match {
      case Some(asset) => Some(s"≤${asset.units}")
      case None        => None
    }
    val srcUnitsIssue = {
      val srcUnitsValueIssue =
        if (srcUnitsValued) None
        else Some(Strings.positiveValue)
      if (!isPayment && srcSelected && srcAvailabilitySelected) {
        srcAvailableAsset match {
          case Some(asset) =>
            if (srcUnitsValueIssue.nonEmpty) srcUnitsValueIssue
            else if (srcAsset.units > asset.units) Some(Strings.valueLimit.format(asset.units))
            else None

          case None =>
            srcUnitsValueIssue
        }
      } else srcUnitsValueIssue
    }
    val srcOk = srcSelected && srcAvailabilitySelected && !srcAvailabilityAnterior && srcNAVValued && srcUnitsIssue.isEmpty
    JFXStyles.toggleError(srcFundField, !srcSelected, Strings.mandatoryField)
    JFXStyles.toggleStyles(srcAvailabilityField, None,
      JFXStyles.ErrorStyle(!srcAvailabilitySelected, Strings.mandatoryField),
      JFXStyles.ErrorStyle(srcAvailabilityAnterior, Strings.anteriorAvailDate)
    )
    JFXStyles.toggleError(srcAvailabilityField2, !srcAvailabilitySelected, Strings.mandatoryField)
    JFXStyles.toggleError(srcNAVField, !srcNAVValued, Strings.positiveValue)
    srcUnitsField.setPromptText(srcUnitsPrompt.orNull)
    JFXStyles.toggleError(srcUnitsField, srcUnitsIssue.nonEmpty, srcUnitsIssue.orElse(srcUnitsPrompt))

    val dstNeeded = isDstEnabled
    lazy val dstFund = getDstFund.orNull
    val dstSelected = !dstNeeded || Option(dstFund).isDefined
    lazy val dstAvailability = Option(dstAvailabilityField.getValue)
    val dstAvailabilitySelected = !dstNeeded || {
      // We require to select a date if source has one
      getSrcAvailability.isEmpty || dstAvailability.isDefined
    }
    val dstAvailabilityAnterior = dstNeeded && {
      opDateSelected && dstAvailability.exists(_ < operationDate)
    }
    lazy val dsnNAV = getDstNAV
    val dstNAVValued = !dstNeeded || (dsnNAV > 0)
    lazy val dstUnits = getDstUnits
    val dstUnitsValued = !dstNeeded || (dstUnits > 0)
    lazy val dstAsset = Savings.AssetPart(dstFund.scheme.id, dstFund.fund.id, dstAvailability, dstUnits, getDstNAV)
    val dstOk = dstSelected && dstAvailabilitySelected && !dstAvailabilityAnterior && dstNAVValued && dstUnitsValued
    JFXStyles.toggleError(dstFundField, !dstSelected, Strings.mandatoryField)
    JFXStyles.toggleStyles(dstAvailabilityField, None,
      JFXStyles.ErrorStyle(!dstAvailabilitySelected, Strings.mandatoryField),
      JFXStyles.ErrorStyle(dstAvailabilityAnterior, Strings.anteriorAvailDate)
    )
    JFXStyles.toggleError(dstNAVField, !dstNAVValued, Strings.positiveValue)
    JFXStyles.toggleError(dstUnitsField, !dstUnitsValued, Strings.positiveValue)

    lazy val srcAmount = getSrcAmount
    lazy val dstAmount = getDstAmount
    if (dstNeeded && dstNAVValued && (srcAmount > 0) && (dstAmount > 0)) {
      val amountDelta = (dstAmount - srcAmount).abs
      // Amount value step (due to units scale) is 'NAV * 10^-unitsScale'.
      // The max delta (absolute) to reach any amount is then 'amountStep / 2'.
      val allowedDelta = scaleAmount((dsnNAV / BigDecimal(s"1${"0" * Main.settings.unitsScale.get}")) / 2)
      val exceedsDelta = amountDelta > allowedDelta
      JFXStyles.toggleWarning(dstAmountField, exceedsDelta,
        Strings.dstAmountDelta.format(amountDelta, allowedDelta))
    }

    val event = if (opDateOk && srcOk && dstOk) Some {
      val comment = Option(commentField.getText).map(_.trim).find(_.nonEmpty)
      // @nowarn workarounds scala 2.13.x false-positive
      (actionKind: @nowarn) match {
        case AssetActionKind.Payment  => Savings.MakePayment(operationDate, srcAsset, comment)
        case AssetActionKind.Transfer => Savings.MakeTransfer(operationDate, srcAsset, Option(dstOperationDate), dstAsset, comment)
        case AssetActionKind.Refund   => Savings.MakeRefund(operationDate, srcAsset, comment)
      }
    } else None

    srcNAVButton.setDisable(!srcSelected)
    if (isPayment) {
      srcEmptyButton.setVisible(false)
    } else {
      srcEmptyButton.setVisible(true)
      srcEmptyButton.setDisable(!opDateSelected || !srcSelected || !srcAvailabilitySelected)
    }
    dstNAVButton.setDisable(!dstNeeded || !dstSelected)
    dstUnitsAutoButton.setDisable(!dstNeeded || !dstSelected)
    buttonOk.setDisable(event.isEmpty)

    event
  }

  private def getToggleKind(toggle: Toggle): AssetActionKind.Value =
    toggle.getUserData.asInstanceOf[AssetActionKind.Value]

  private def isDstEnabled: Boolean =
    actionKind == AssetActionKind.Transfer

  private def getOperationDate: Option[LocalDate] =
    Option(operationDateField.getValue)

  private def getDstOperationDate: Option[LocalDate] =
    Option(dstOperationDateField.getValue)

  private def getSavings(operationDate: LocalDate): Savings = {
    // If operation date predates latest asset action, we need to get
    // actual savings on the operation date (as availabilities may have
    // been resolved by the following operations).
    // Cache the computed assets for better efficiency.
    if (actualSavingsDate == operationDate) actualSavings
    else {
      val savings0 =
        if (savings.latestAssetAction.exists(operationDate < _)) {
          val history = Awaits.getEventsHistory(stage, upTo = Some(operationDate))
          Savings(levies = savings.levies).processEvents(history)
        } else savings
      actualSavings = savings0.computeAssets(operationDate)
      actualSavingsDate = operationDate
      actualSavings
    }
  }

  private def getSrcFund: Option[SchemeAndFund] =
    Option(srcFundField.getValue).flatten

  private def getSrcAvailability: Option[LocalDate] = {
    val srcAvailabilityExact = actionKind != AssetActionKind.Payment

    if (!srcAvailabilityExact) Option(srcAvailabilityField.getValue)
    else Option(srcAvailabilityField2.getValue).flatten
  }

  private def getSrcNAV: BigDecimal =
    parseNumber(srcNAVField.getText)

  private def getSrcAmount: BigDecimal =
    parseNumber(srcAmountField.getText)

  private def getSrcUnits: BigDecimal =
    parseNumber(srcUnitsField.getText)

  private def getDstFund: Option[SchemeAndFund] =
    Option(dstFundField.getValue).flatten

  private def getDstNAV: BigDecimal =
    parseNumber(dstNAVField.getText)

  private def getDstAmount: BigDecimal =
    parseNumber(dstAmountField.getText)

  private def getDstUnits: BigDecimal =
    parseNumber(dstUnitsField.getText)

  private def breakRecursion[A](f: => A): Unit =
    if (recursionLevel == 0) {
      recursionLevel += 1
      try {
        f
      }
      finally {
        recursionLevel -= 1
      }
      ()
    }

}

object NewAssetActionController {

  private val settingsKeyPrefix = "new-asset-action"

  private val stageLocation = ConfigEntry.from[StageLocation](Main.settings.settings,
    Settings.prefix ++ Seq(Settings.KEY_STAGE, settingsKeyPrefix, Settings.KEY_LOCATION))

  private val dstUnitsAuto = ConfigEntry.from[Boolean](Main.settings.settings,
    Settings.prefix ++ Seq(settingsKeyPrefix, "dst-units-auto"))

  /** Builds a dialog out of this controller. */
  def buildDialog(owner: Window, mainController: MainController, savings: Savings,
    kind: AssetActionKind.Value, asset: Option[Savings.Asset]): Dialog[Option[Savings.AssetEvent]] =
  {
    val dialog = new Dialog[Option[Savings.AssetEvent]]()
    // Note: initializing owner resets dialog icon, so set the icon afterwards
    Stages.initOwner(dialog, owner)
    // Icon and title are changed according to chosen asset action
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val loader = new FXMLLoader(getClass.getResource("/fxml/new-asset-action.fxml"), I18N.getResources)
    dialog.getDialogPane.setContent(loader.load[Node]())
    val controller = loader.getController[NewAssetActionController]
    controller.initialize(mainController, savings, dialog, kind, asset)

    Dialogs.addPersistence(dialog, controller)

    dialog.setResultConverter(resultConverter(owner, controller) _)

    dialog
  }

  private def resultConverter(owner: Window, controller: NewAssetActionController)(buttonType: ButtonType): Option[Savings.AssetEvent] = {
    if (buttonType != ButtonType.OK) None
    else {
      val eventOpt = controller.checkForm()
      eventOpt.foreach { event =>
        // For the validated asset operation, we wish to update fund(s) NAV(s)
        // when applicable. This is the case if either:
        //  - there was no known NAV for a given date, either because there is
        //    no NAV history yet (newly created fund), or operation date
        //    predates current history
        //  - a NAV was found but not for the exact operation date: we only
        //    found a NAV for a date preceding the operation
        // For non-transfer operations, there is only one fund concerned, and
        // one operation date.
        // For transfer operations, there usually is only one date.
        // But sometimes the operation is actually effective (truly visible) on
        // a different date.
        // For the later case, what we expect is that the effective second date
        // is later compared to the event date.
        // What may also happen is that the destination fund was newly created
        // and has no NAV history yet, or at least that this operation predates
        // (for event and effective date) any known NAV history.
        // For this particular case, we insert as usual the NAV for the event
        // date, and for visual reasons (fund NAV graph) we wish to insert
        // the same value for the effective date too: the only condition is
        // thus that for this date there was no known NAV yet (even for a
        // preceding date).
        val dstDate = event match {
          case Savings.MakeTransfer(_, _, dstDate, _, _) => dstDate.getOrElse(event.date)
          case _ => event.date
        }
        for {
          (schemeAndFundOpt, field, date, value, insertOnlyMissing) <- List(
            (controller.getSrcFund, controller.srcNAVField, event.date, controller.getSrcNAV, false),
            (controller.getDstFund, controller.dstNAVField, event.date, controller.getDstNAV, false)
          ) ++ {
            // If asset operation is effective on destination fund on a different
            // date, take it into account.
            if (dstDate != event.date) List((controller.getDstFund, controller.dstNAVField, dstDate, controller.getDstNAV, true))
            else Nil
          }
          schemeAndFund <- schemeAndFundOpt
          // Only consider NAVs when operation concerns this field.
          navs <-
            if (field.isDisabled) None
            else Option(field.getUserData.asInstanceOf[Map[LocalDate, Savings.AssetValue]]).orElse(Some(Map.empty[LocalDate, Savings.AssetValue]))
          // See comments above: in nominal case we update/insert if filled form
          // differs from history NAV; in a specific case we only insert if
          // there is no NAV history at the wanted date.
          navOpt = navs.get(date)
          mayInsert = !insertOnlyMissing || navOpt.isEmpty
          nav = navOpt.getOrElse(Savings.AssetValue(LocalDate.ofEpochDay(0), 0))
        } {
          // Belt and suspenders: only insert/update if NAV date is not the
          // operation date.
          if (mayInsert && (date != nav.date) && (value != nav.value)) {
            Awaits.saveDataStoreNAV(owner, schemeAndFund.fund.id, Savings.AssetValue(date, value))
          }
        }
      }
      eventOpt
    }
  }

}
