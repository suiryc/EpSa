package epsa.controllers

import epsa.I18N
import epsa.I18N.Strings
import epsa.model.Savings
import epsa.util.{Awaits, JFXStyles}
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import javafx.event.ActionEvent
import javafx.collections.FXCollections
import javafx.fxml.{FXML, FXMLLoader}
import javafx.geometry.Insets
import javafx.scene.Node
import javafx.scene.control._
import javafx.scene.layout.{GridPane, Region}
import javafx.stage.{Modality, Stage}
import javafx.util.converter.LocalDateStringConverter
import scala.collection.JavaConversions._
import suiryc.scala.javafx.stage.Stages.StageLocation
import suiryc.scala.math.Ordering._
import suiryc.scala.settings.Preference
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.event.EventHandler._
import suiryc.scala.javafx.scene.control.{DatePickers, TextFieldWithButton}
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.util.Callback

// TODO: option of default day/month and number of years for frozen assets ?
class NewAssetActionController {

  import epsa.Settings.{scaleAmount, scaleUnits}
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
  protected var operationDateField: DatePicker = _

  @FXML
  protected var srcFundField: ComboBox[Option[SchemeAndFund]] = _

  @FXML
  protected var srcAvailabilityField: DatePicker = _

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

  private lazy val stage = paymentButton.getScene.getWindow.asInstanceOf[Stage]

  private lazy val srcFundGridPane = srcEmptyButton.getParent.asInstanceOf[GridPane]

  private lazy val columnConstraints = srcFundGridPane.getColumnConstraints.get(GridPane.getColumnIndex(srcEmptyButton))

  private lazy val toggleButtons = List(paymentButton, transferButton, refundButton)

  private var savings: Savings = _

  private var actionKind: AssetActionKind.Value = _

  private var recursionLevel = 0

  private var dstAvailabilityChosen = false

  def initialize(mainController0: MainController, savings0: Savings, dialog: Dialog[_], actionKind0: AssetActionKind.Value, asset: Option[Savings.Asset]): Unit = {
    // Save initial state
    mainController = mainController0
    savings = savings0
    actionKind = actionKind0

    // Load css
    dialog.getDialogPane.getStylesheets.add(getClass.getResource("/css/form.css").toExternalForm)

    // Lookup OK dialog button
    buttonOk = dialog.getDialogPane.lookupButton(ButtonType.OK)
    // Disable OK button unless there are events to take into account
    buttonOk.setDisable(true)

    // Associate action kind to related toggle button
    paymentButton.setUserData(AssetActionKind.Payment)
    transferButton.setUserData(AssetActionKind.Transfer)
    refundButton.setUserData(AssetActionKind.Refund)

    // Listen to action kind change
    actionKindGroup.selectedToggleProperty.listen(onToggleKind())
    // Select initial toggle button
    toggleButtons.find(getToggleKind(_) == actionKind).foreach(actionKindGroup.selectToggle)

    // Listen to operation date changes
    operationDateField.valueProperty.listen(onOperationDate())

    // Listen to fund changes
    srcFundField.valueProperty.listen(onSrcFund())
    dstFundField.valueProperty.listen(onDstFund())

    // Note: we need to tell the combobox how to display both the 'button' area
    // (what is shown as selected) and the content (list of choices).
    for (field <- List(srcFundField, dstFundField)) {
      field.setButtonCell(new SchemeAndFundCell)
      field.setCellFactory(Callback { new SchemeAndFundCell })
    }

    // Listen to availability date changes
    srcAvailabilityField.valueProperty.listen(onSrcAvailability())
    srcAvailabilityField2.getSelectionModel.selectedItemProperty.listen(onSrcAvailability())
    dstAvailabilityField.valueProperty.listen(onDstAvailability())

    // Note: we set the availability combobox format now and change it later
    // if operation date if changed.
    srcAvailabilityField2.setButtonCell(new AvailabilityListCell(None))
    srcAvailabilityField2.setCellFactory(Callback { new AvailabilityListCell(None) })

    // Force date format (to match the one of LocalDate in other views) in date picker fields
    val dateFormat = "yyyy-MM-dd"
    val dateConverter = new LocalDateStringConverter(DateTimeFormatter.ofPattern(dateFormat), null)
    for (field <- List(operationDateField, srcAvailabilityField, dstAvailabilityField)) {
      field.setPromptText(dateFormat)
      field.setConverter(dateConverter)
      // Track field editor to apply value when possible
      DatePickers.trackEdition(field)
    }
    // Override availability date pickers to disable dates anterior to operation date.
    for (field <- List(srcAvailabilityField, dstAvailabilityField)) {
      field.setDayCellFactory(Callback { _ =>
        new DateCell {
          override def updateItem(item: LocalDate, empty: Boolean): Unit = {
            super.updateItem(item, empty)
            Option(operationDateField.getValue).foreach { operationDate =>
              val unavailable = !empty && item.isBefore(operationDate)
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

    // Setup funds buttons
    for (field <- List(srcNAVButton, srcEmptyButton, dstNAVButton, dstUnitsAutoButton)) {
      // Disable by default; will be enabled when a fund is selected
      field.setDisable(true)
      // Reset padding of button; by default uses 8 on each horizontal side
      // and 4 on each vertical side, which gives a rectangle. We will use
      // 4 on each side to get a square to display our square icon inside.
      field.setPadding(new Insets(4))
    }

    // Setup NAV history buttons
    for (field <- List(srcNAVButton, dstNAVButton)) {
      field.setTooltip(new Tooltip(NetAssetValueHistoryController.title))
      field.setOnAction { (event: ActionEvent) =>
        val opt =
          if (field == dstNAVButton) getDstFund
          else getSrcFund
        opt.foreach { schemeAndFund =>
          onNAVHistory(schemeAndFund.fund)
        }
      }
    }

    // Setup source fund emptying button
    srcEmptyButton.setTooltip(new Tooltip(Strings.empty))
    srcEmptyButton.setOnAction { (event: ActionEvent) =>
      onSrcEmpty()
    }

    // Setup src/dst NAV/amount/units field listeners
    for ((field, cb) <- List(
      (srcNAVField.textField, onSrcNAV _),
      (srcAmountField, onSrcAmount _),
      (srcUnitsField, onSrcUnits _),
      (dstNAVField.textField, onDstNAV _),
      (dstUnitsField, onDstUnits _)
    )) {
      field.textProperty.listen(cb())
    }

    // Setup dst units auto button: select if necessary, and compute once
    // selected.
    // Note: behave as if counterpart value was modified ('initRecursion'), so
    // that recursion can be broken as expected.
    dstUnitsAutoButton.setSelected(dstUnitsAuto())
    dstUnitsAutoButton.selectedProperty.listen { selected =>
      if (selected) onSrcAmount()
    }

    // Really make sure we don't leave if something is not OK
    buttonOk.addEventFilter(ActionEvent.ACTION, { (event: ActionEvent) =>
      if (checkForm().isEmpty) event.consume()
    })

    // Set now as operation date default
    operationDateField.setValue(LocalDate.now)

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

  def onCloseRequest(event: DialogEvent): Unit = {
    persistView()
  }

  private def onToggleKind(): Unit = {
    actionKind = getToggleKind(actionKindGroup.getSelectedToggle)

    val disableDst = !isDstEnabled
    dstFundField.setDisable(disableDst)
    dstAvailabilityField.setDisable(disableDst)
    dstNAVField.setDisable(disableDst)
    dstAmountField.setDisable(disableDst)
    dstUnitsField.setDisable(disableDst)
    if (isDstEnabled) dstAmountField.setText(srcAmountField.getText)
    // Reset user choice of destination availability when changing to action
    // other than transfer.
    if (disableDst) dstAvailabilityChosen = false

    val srcAvailabilityExact = actionKind != AssetActionKind.Payment
    srcAvailabilityField.setVisible(!srcAvailabilityExact)
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
  }

  private def onOperationDate(): Unit = {
    updateSrcAvailability()
    updateNAV()
    checkForm()
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
    if (!dstAvailabilityChosen && isDstEnabled) {
      // Note: don't forget to use actual availability based on operation date
      val availability = Savings.resolveAvailability(getSrcAvailability, Option(operationDateField.getValue))
      dstAvailabilityField.setValue(availability.orNull)
    }
    checkForm()
  }

  private def onSrcNAV(): Unit = {
    computeSrcAmount()
    checkForm()
  }

  def onSrcAmount(): Unit = {
    val value = getSrcAmount
    if ((value > 0) && isDstEnabled && dstUnitsAutoButton.isSelected) {
      val dstNAV = getDstNAV
      if (dstNAV > 0) {
        val dstUnits = scaleUnits(value / dstNAV)
        dstUnitsField.setText(dstUnits.toString)
      }
    }
  }

  private def onSrcUnits(): Unit = {
    computeSrcAmount()
    checkForm()
  }

  private def onSrcEmpty(): Unit = {
    for {
      operationDate <- Option(operationDateField.getValue)
      schemeAndFund <- getSrcFund
    } {
      val srcAvailability = getSrcAvailability
      val searchAsset = Savings.Asset(schemeAndFund.scheme.id, schemeAndFund.fund.id, srcAvailability, 0, 0)
      savings.computeAssets(operationDate).findAsset(operationDate, searchAsset).foreach { asset =>
        srcAmountField.setText(asset.amount(getSrcNAV).toString)
        srcUnitsField.setText(asset.units.toString)
      }
    }
  }

  private def onDstFund(): Unit = {
    updateNAV(updateSrc = false)
    // Trigger units/amount computation from source amount
    onSrcAmount()
    checkForm()
  }

  private def onDstAvailability(): Unit = breakRecursion {
    dstAvailabilityChosen = Option(dstAvailabilityField.getValue).isDefined
    checkForm()
  }

  private def onDstNAV(): Unit = {
    computeDstAmount()
    checkForm()
  }

  private def onDstUnits(): Unit = {
    computeDstAmount()
    checkForm()
  }

  private def onNAVHistory(fund: Savings.Fund): Unit = {
    val dialog = NetAssetValueHistoryController.buildStage(mainController, savings, Some(fund.id), stage)
    dialog.initModality(Modality.WINDOW_MODAL)
    dialog.initOwner(stage)
    dialog.setResizable(true)
    if (dialog.showAndWait().orElse(false)) updateNAV()
  }

  private def computeAmount(units: BigDecimal, nav: BigDecimal, amountField: TextField): Unit = {
    if ((units > 0) && (nav > 0)) {
      val amount = scaleAmount(units * nav)
      amountField.setText(amount.toString)
    }
  }

  private def computeSrcAmount(): Unit = {
    computeAmount(getSrcUnits, getSrcNAV, srcAmountField)
  }

  private def computeDstAmount(): Unit = {
    computeAmount(getDstUnits, getDstNAV, dstAmountField)
  }

  private def buildSchemeAndFunds(entries: List[SchemeAndFund]*): List[Option[SchemeAndFund]] =
    entries.foldLeft(List[Option[SchemeAndFund]]()) { (acc, schemeAndFunds) =>
      val rest = schemeAndFunds.map(Some(_))
      if (acc.isEmpty || schemeAndFunds.isEmpty) acc ::: rest
      else acc ::: None :: rest
    }

  private def updateSchemeAndFund(): Unit = {
    // Note: previous selected value is kept if still present in new items

    // Scheme&fund with asset
    val fundsWithAsset = savings.assets.map { asset =>
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
      // Other scheme&fund
      val fundsOther = savings.schemes.flatMap { scheme =>
        scheme.funds.map { fundId =>
          val fund = savings.getFund(fundId)
          SchemeAndFund(scheme, fund)
        }
      }.filterNot(fundsWithAsset.contains).sorted

      // Choices are listed by order of 'preference' (chances to be chosen),
      // with a separator between categories:
      //   - funds that already have assets
      //   - funds without assets
      srcFundField.setItems(FXCollections.observableList(buildSchemeAndFunds(fundsWithAsset, fundsOther)))
    } else {
      srcFundField.setItems(FXCollections.observableList(buildSchemeAndFunds(fundsWithAsset)))
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
      val fundsWithAsset = savings.assets.map { asset =>
        val scheme = savings.getScheme(asset.schemeId)
        val fund = savings.getFund(asset.fundId)
        SchemeAndFund(scheme, fund)
      }.distinct.sorted
      // Other scheme&fund
      val fundsOther = savings.schemes.flatMap { scheme =>
        scheme.funds.map { fundId =>
          val fund = savings.getFund(fundId)
          SchemeAndFund(scheme, fund)
        }
      }.filterNot(fundsWithAsset.contains).sorted
      // Other scheme&fund for destination, with first same scheme as source if
      // selected
      val fundsDst = getSrcFund.map { schemeAndFund =>
        val (fundsSameScheme1, fundsOtherScheme1) = fundsWithAsset.filterNot(_ == schemeAndFund).partition(_.scheme == schemeAndFund.scheme)
        val (fundsSameScheme2, fundsOtherScheme2) = fundsOther.partition(_.scheme == schemeAndFund.scheme)
        buildSchemeAndFunds(fundsSameScheme1, fundsSameScheme2, fundsOtherScheme1, fundsOtherScheme2)
      }.getOrElse(buildSchemeAndFunds(fundsWithAsset, fundsOther))

      // Choices are listed by order of 'preference' (chances to be chosen),
      // with a separator between categories:
      //   - funds in the source scheme that already have assets
      //   - funds in the source scheme without assets
      //   - funds in another scheme that already have assets
      //   - funds in another scheme without assets
      dstFundField.setItems(FXCollections.observableList(fundsDst))
    }
  }

  private def updateSrcAvailability(): Unit = {
    Option(operationDateField.getValue) match {
      case Some(date) =>
        // Note: changing the combobox format appears to be taken into account
        // right away (unlike TableView). Probably because the concerned content
        // is drawn when necessary while the table has some already shown.
        srcAvailabilityField2.setButtonCell(new AvailabilityListCell(Some(date)))
        srcAvailabilityField2.setCellFactory(Callback { new AvailabilityListCell(Some(date)) })

        srcAvailabilityField2.setDisable(false)
        // Note: get availabilities for selected scheme&fund, sorted by date (with
        // immediate availability first).
        val availabilities = getSrcFund.map { schemeAndFund =>
          filterAssets(savings.computeAssets(date).assets, schemeAndFund).map(_.availability).distinct.sortBy { opt =>
            opt.getOrElse(LocalDate.ofEpochDay(0))
          }
        }.getOrElse(Nil)
        srcAvailabilityField2.setItems(FXCollections.observableList(availabilities))

      case None =>
        srcAvailabilityField2.setDisable(true)
    }
  }

  private def updateNAV(updateSrc: Boolean = true): Unit = {
    Option(operationDateField.getValue).foreach { operationDate =>
      def updateField(field: TextFieldWithButton, fund: Savings.Fund): Unit = {
        val navOpt = Awaits.readDataStoreNAV(Some(stage), fund.id, operationDate).getOrElse(None)
        navOpt match {
          case Some(nav) =>
            val text = nav.value.toString
            field.setText(text)
            field.setTooltip(new Tooltip(s"${Strings.date}: ${nav.date}"))
            field.setOnButtonAction { (event: ActionEvent) =>
              field.setText(text)
            }
            // Bind so that changing value allows to reset it
            field.buttonDisableProperty.bind(field.textField.textProperty.isEqualTo(text))

          case None =>
            field.setText(null)
            field.setTooltip(null)
            field.buttonDisableProperty.unbind()
            field.setOnButtonAction(null)
            field.setButtonDisable(true)
        }
      }

      if (updateSrc) getSrcFund.foreach { schemeAndFund =>
        updateField(srcNAVField, schemeAndFund.fund)
      }
      getDstFund.foreach { schemeAndFund =>
        updateField(dstNAVField, schemeAndFund.fund)
      }
    }
  }

  private def checkForm(): Option[Savings.Event] = {
    val operationDate = operationDateField.getValue
    val opDateSelected = Option(operationDate).isDefined
    val opDateAnterior = opDateSelected && savings.latestAssetAction.exists(_.isAfter(operationDate))
    // Selecting a date of operation anterior to the latest asset action date is allowed even if discouraged
    val opDateOk = opDateSelected
    val warningMsgOpt = savings.latestAssetAction.map(Strings.anteriorOpDate.format(_))
    JFXStyles.toggleStyles(operationDateField, None,
      JFXStyles.ErrorStyle(!opDateSelected, Strings.mandatoryField),
      JFXStyles.WarningStyle(opDateAnterior, warningMsgOpt.getOrElse(""))
    )

    val isPayment = actionKind == AssetActionKind.Payment
    val srcFund = getSrcFund.orNull
    val srcSelected = Option(srcFund).isDefined
    val srcAvailability = getSrcAvailability
    val srcAvailabilitySelected =
      (isPayment && Option(srcAvailabilityField.getValue).isDefined) ||
      (!isPayment && Option(srcAvailabilityField2.getValue).isDefined)
    val srcAvailabilityAnterior = opDateSelected && srcAvailability.exists(_.isBefore(operationDate))
    val srcNAV = getSrcNAV
    val srcNAVValued = srcNAV > 0
    val srcUnits = getSrcUnits
    val srcUnitsValued = srcUnits > 0
    lazy val srcAsset = Savings.AssetPart(srcFund.scheme.id, srcFund.fund.id, srcAvailability, srcUnits, getSrcNAV)
    val srcAvailableAsset =
      if (isPayment || !opDateSelected || !srcSelected || !srcAvailabilitySelected) None
      else savings.computeAssets(operationDate).findAsset(operationDate, srcAsset)
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
    JFXStyles.toggleError(srcFundField, !srcSelected,
      if (srcSelected) None
      else Some(Strings.mandatoryField)
    )
    JFXStyles.toggleStyles(srcAvailabilityField, None,
      JFXStyles.ErrorStyle(!srcAvailabilitySelected, Strings.mandatoryField),
      JFXStyles.ErrorStyle(srcAvailabilityAnterior, Strings.anteriorAvailDate)
    )
    JFXStyles.toggleError(srcAvailabilityField2, !srcAvailabilitySelected,
      if (srcAvailabilitySelected) None
      else Some(Strings.mandatoryField)
    )
    JFXStyles.toggleError(srcNAVField, !srcNAVValued,
      if (srcNAVValued) None
      else Some(Strings.positiveValue)
    )
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
      opDateSelected && dstAvailability.exists(_.isBefore(operationDate))
    }
    lazy val dsnNAV = getDstNAV
    val dstNAVValued = !dstNeeded || (dsnNAV > 0)
    lazy val dstUnits = getDstUnits
    val dstUnitsValued = !dstNeeded || (dstUnits > 0)
    lazy val dstAsset = Savings.AssetPart(dstFund.scheme.id, dstFund.fund.id, dstAvailability, dstUnits, getDstNAV)
    val dstOk = dstSelected && dstAvailabilitySelected && !dstAvailabilityAnterior && dstNAVValued && dstUnitsValued
    JFXStyles.toggleError(dstFundField, !dstSelected,
      if (dstSelected) None
      else Some(Strings.mandatoryField)
    )
    JFXStyles.toggleStyles(dstAvailabilityField, None,
      JFXStyles.ErrorStyle(!dstAvailabilitySelected, Strings.mandatoryField),
      JFXStyles.ErrorStyle(dstAvailabilityAnterior, Strings.anteriorAvailDate)
    )
    JFXStyles.toggleError(dstNAVField, !dstNAVValued,
      if (dstNAVValued) None
      else Some(Strings.positiveValue)
    )
    JFXStyles.toggleError(dstUnitsField, !dstUnitsValued,
      if (dstUnitsValued) None
      else Some(Strings.positiveValue)
    )

    lazy val srcAmount = getSrcAmount
    lazy val dstAmount = getDstAmount
    if (dstNeeded && dstNAVValued && (srcAmount > 0) && (dstAmount > 0)) {
      val amountDelta = (dstAmount - srcAmount).abs
      // Amount value step (due to units scale) is 'NAV * 10^-unitsScale'.
      // The max delta (absolute) to reach any amount is then 'amountStep / 2'.
      val allowedDelta = scaleAmount((dsnNAV / BigDecimal(s"1${"0" * epsa.Settings.unitsScale()}")) / 2)
      val exceedsDelta = amountDelta > allowedDelta
      JFXStyles.toggleWarning(dstAmountField, exceedsDelta,
        if (!exceedsDelta) None
        else Some(Strings.dstAmountDelta.format(amountDelta, allowedDelta))
      )
    }

    val event = if (opDateOk && srcOk && dstOk) Some {
      val comment = Option(commentField.getText).map(_.trim).find(_.nonEmpty)
      actionKind match {
        case AssetActionKind.Payment  => Savings.MakePayment(operationDate, srcAsset, comment)
        case AssetActionKind.Transfer => Savings.MakeTransfer(operationDate, srcAsset, dstAsset, comment)
        case AssetActionKind.Refund   => Savings.MakeRefund(operationDate, srcAsset, comment)
      }
    } else None

    srcNAVButton.setDisable(!srcSelected)
    if (isPayment) {
      srcEmptyButton.setVisible(false)
      // Set column preferred width to zero, which visually renders as if there
      // was not such column.
      columnConstraints.setPrefWidth(0.0)
    } else {
      // Reset column preferred width before displaying its content.
      columnConstraints.setPrefWidth(Region.USE_COMPUTED_SIZE)
      srcEmptyButton.setVisible(true)
      srcEmptyButton.setDisable(!opDateSelected || !srcSelected || !srcAvailabilitySelected)
    }
    dstNAVButton.setDisable(!dstNeeded || !dstSelected)
    dstUnitsAutoButton.setDisable(!dstNeeded || !dstSelected)
    buttonOk.setDisable(event.isEmpty)

    event
  }

  private def filterAssets(assets: List[Savings.Asset], schemeAndFund: SchemeAndFund): List[Savings.Asset] =
    assets.filter { asset =>
      (asset.schemeId == schemeAndFund.scheme.id) &&
        (asset.fundId == schemeAndFund.fund.id)
    }

  private def getToggleKind(toggle: Toggle): AssetActionKind.Value =
    toggle.getUserData.asInstanceOf[AssetActionKind.Value]

  private def isDstEnabled: Boolean =
    actionKind == AssetActionKind.Transfer

  private def getSrcFund: Option[SchemeAndFund] =
    Option(srcFundField.getValue).flatten

  private def getSrcAvailability: Option[LocalDate] = {
    val srcAvailabilityExact = actionKind != AssetActionKind.Payment

    if (!srcAvailabilityExact) Option(srcAvailabilityField.getValue)
    else Option(srcAvailabilityField2.getValue).getOrElse(None)
  }

  private def getBigDecimal(str: String): BigDecimal = try {
    Option(str).map(BigDecimal(_)).getOrElse(BigDecimal(0))
  } catch {
    case ex: Throwable => BigDecimal(0)
  }

  private def getSrcNAV: BigDecimal =
    getBigDecimal(srcNAVField.getText)

  private def getSrcAmount: BigDecimal =
    getBigDecimal(srcAmountField.getText)

  private def getSrcUnits: BigDecimal =
    getBigDecimal(srcUnitsField.getText)

  private def getDstFund: Option[SchemeAndFund] =
    Option(dstFundField.getValue).flatten

  private def getDstNAV: BigDecimal =
    getBigDecimal(dstNAVField.getText)

  private def getDstAmount: BigDecimal =
    getBigDecimal(dstAmountField.getText)

  private def getDstUnits: BigDecimal =
    getBigDecimal(dstUnitsField.getText)

  private def breakRecursion[A](f: => A): Unit =
    if (recursionLevel == 0) {
      recursionLevel += 1
      try {
        f
      }
      finally {
        recursionLevel -= 1
      }
    }

}

object NewAssetActionController {

  import epsa.Settings.prefs
  import Preference._

  private val prefsKeyPrefix = "stage.new-asset-action"

  private val stageLocation = Preference.from(s"$prefsKeyPrefix.location", null:StageLocation)

  private val dstUnitsAuto = Preference.from(s"$prefsKeyPrefix.dst-units-auto", true)

  /** Builds a dialog out of this controller. */
  def buildDialog(mainController: MainController, savings: Savings, kind: AssetActionKind.Value, asset: Option[Savings.Asset]): Dialog[Option[Savings.Event]] = {
    val dialog = new Dialog[Option[Savings.Event]]()
    val title = s"${Strings.payment} / ${Strings.transfer} / ${Strings.refund}"
    dialog.setTitle(title)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val loader = new FXMLLoader(getClass.getResource("/fxml/new-asset-action.fxml"), I18N.getResources)
    dialog.getDialogPane.setContent(loader.load())
    val controller = loader.getController[NewAssetActionController]
    controller.initialize(mainController, savings, dialog, kind, asset)

    // Delegate closing request to controller
    dialog.setOnCloseRequest(controller.onCloseRequest _)

    // Wait for dialog to be shown before restoring the view
    dialog.showingProperty().listen2 { cancellable =>
      cancellable.cancel()
      controller.restoreView()
    }

    dialog.setResultConverter(Callback { resultConverter(controller) _ })
    Stages.trackMinimumDimensions(Stages.getStage(dialog))

    dialog
  }

  private def resultConverter(controller: NewAssetActionController)(buttonType: ButtonType): Option[Savings.Event] = {
    if (buttonType != ButtonType.OK) None
    else {
      dstUnitsAuto() = controller.dstUnitsAutoButton.isSelected
      controller.checkForm()
    }
  }

}
