package epsa.controllers

import epsa.I18N
import epsa.model.Savings
import epsa.util.Awaits
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.ResourceBundle
import javafx.event.ActionEvent
import javafx.collections.FXCollections
import javafx.fxml.{FXML, FXMLLoader}
import javafx.geometry.Insets
import javafx.scene.Node
import javafx.scene.control._
import javafx.stage.{Modality, Stage}
import javafx.util.converter.LocalDateStringConverter
import scala.collection.JavaConversions._
import suiryc.scala.javafx.stage.Stages.StageLocation
import suiryc.scala.math.Ordering._
import suiryc.scala.settings.Preference
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.event.EventHandler._
import suiryc.scala.javafx.scene.control.DatePickers
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.util.Callback

// TODO - handle some event (action ?) on amount/units to update counterpart (and also destination data if any)
// TODO - option of default day/month and number of years for frozen assets ?
class NewAssetActionController {

  import epsa.Settings.prefs

  private val stageLocation = Preference.from("stage.new-asset-action.location", null:StageLocation)

  @FXML
  protected var resources: ResourceBundle = _

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
  protected var srcNAVField: TextField = _

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
  protected var dstNAVField: TextField = _

  @FXML
  protected var dstNAVButton: Button = _

  @FXML
  protected var dstAmountField: TextField = _

  @FXML
  protected var dstUnitsField: TextField = _

  protected var buttonOk: Node = _

  private var mainController: MainController = _

  private lazy val stage = paymentButton.getScene.getWindow.asInstanceOf[Stage]

  private lazy val toggleButtons = List(paymentButton, transferButton, refundButton)

  private lazy val mandatoryMsg = resources.getString("Mandatory field")

  private lazy val positiveValueMsg = resources.getString("Positive value expected")

  private lazy val valueLimitMsg = resources.getString("Value exceeds available quantity")

  private lazy val emptyOneMsg = resources.getString("warning.empty-amount-and-units")

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
          override def updateItem(item: LocalDate, empty: Boolean) {
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
        }
      })
    }

    // Setup funds buttons
    for (field <- List(srcNAVButton, srcEmptyButton, dstNAVButton)) {
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
    srcEmptyButton.setTooltip(new Tooltip(resources.getString("Empty")))
    srcEmptyButton.setOnAction { (event: ActionEvent) =>
      onSrcEmpty()
    }

    // Re-check form when source/destination amount/units is changed
    for (field <- List(srcAmountField, srcUnitsField, dstAmountField, dstUnitsField)) {
      field.textProperty.listen {
        checkForm()
        ()
      }
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

  def onToggleKind(): Unit = breakRecursion {
    actionKind = getToggleKind(actionKindGroup.getSelectedToggle)

    val disableDst = !isDstEnabled
    dstFundField.setDisable(disableDst)
    dstAvailabilityField.setDisable(disableDst)
    dstNAVField.setDisable(disableDst)
    dstNAVButton.setDisable(disableDst)
    dstAmountField.setDisable(disableDst)
    dstUnitsField.setDisable(disableDst)

    val srcAvailabilityExact = actionKind != AssetActionKind.Payment
    srcAvailabilityField.setVisible(!srcAvailabilityExact)
    srcAvailabilityField2.setVisible(srcAvailabilityExact)

    updateSchemeAndFund()
    if (srcAvailabilityExact) updateSrcAvailability()
    // Simulate changing source availability (to possibly change destination one)
    if (!disableDst) onSrcAvailability()
    checkForm()
  }

  def onOperationDate(): Unit = breakRecursion {
    updateSrcAvailability()
    updateNAV()
    checkForm()
  }

  def onSrcFund(): Unit = breakRecursion {
    val srcAvailabilityExact = actionKind != AssetActionKind.Payment

    if (srcAvailabilityExact) updateSrcAvailability()
    updateDstSchemeAndFund()
    updateNAV()
    checkForm()
  }

  def onSrcAvailability(): Unit = breakRecursion {
    if (!dstAvailabilityChosen && isDstEnabled) {
      // Note: don't forget to use actual availability based on operation date
      val availability = Savings.resolveAvailablity(getSrcAvailability, Option(operationDateField.getValue))
      dstAvailabilityField.setValue(availability.orNull)
    }
    checkForm()
  }

  def onSrcEmpty(): Unit = {
    for {
      operationDate <- Option(operationDateField.getValue)
      schemeAndFund <- getSrcFund
    } {
      val srcAvailability = getSrcAvailability
      val searchAsset = Savings.Asset(schemeAndFund.scheme.id, schemeAndFund.fund.id, srcAvailability, 0, 0)
      savings.computeAssets(operationDate).findAsset(operationDate, searchAsset).foreach { asset =>
        srcAmountField.setText(asset.amount.toString)
        srcUnitsField.setText(asset.units.toString)
      }
    }
  }

  def onDstFund(): Unit = breakRecursion {
    updateNAV()
    checkForm()
  }

  def onDstAvailability(): Unit = breakRecursion {
    dstAvailabilityChosen = Option(dstAvailabilityField.getValue).isDefined
    checkForm()
  }

  def onNAVHistory(fund: Savings.Fund): Unit = {
    val dialog = NetAssetValueHistoryController.buildStage(mainController, savings, Some(fund.id), stage)
    dialog.initModality(Modality.WINDOW_MODAL)
    dialog.initOwner(stage)
    dialog.setResizable(true)
    if (dialog.showAndWait().orElse(false)) updateNAV()
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
    if (actionKind == AssetActionKind.Transfer) {
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

  private def updateNAV(): Unit = {
    // TODO - recompute amount based on units and operation date ?
    Option(operationDateField.getValue).foreach { operationDate =>
      val labelDate = resources.getString("Date")

      def updateField(field: TextField, fund: Savings.Fund): Unit = {
        val navOpt = Awaits.readDataStoreNAV(Some(stage), fund.id, operationDate).getOrElse(None)
        navOpt match {
          case Some(nav) =>
            field.setText(nav.value.toString)
            field.setTooltip(new Tooltip(s"$labelDate: ${nav.date}"))

          case None =>
            field.setText(null)
            field.setTooltip(null)
        }
      }

      getSrcFund.foreach { schemeAndFund =>
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
    val warningMsgOpt = savings.latestAssetAction.map(resources.getString("warning.anterior-operation-date").format(_))
    Form.toggleStyles(operationDateField, None,
      Form.ErrorStyle(!opDateSelected, mandatoryMsg),
      Form.WarningStyle(opDateAnterior, warningMsgOpt.getOrElse(""))
    )

    val isPayment = actionKind == AssetActionKind.Payment
    val srcFund = getSrcFund.orNull
    val srcSelected = Option(srcFund).isDefined
    val srcAvailability = getSrcAvailability
    val srcAvailabilitySelected =
      (isPayment && Option(srcAvailabilityField.getValue).isDefined) ||
      (!isPayment && Option(srcAvailabilityField2.getValue).isDefined)
    val srcAvailabilityAnterior = opDateSelected && srcAvailability.exists(_.isBefore(operationDate))
    val srcAmount = getSrcAmount
    val srcAmountValued = srcAmount > 0
    val srcUnits = getSrcUnits
    val srcUnitsValued = srcUnits > 0
    lazy val srcAsset = Savings.Asset(srcFund.scheme.id, srcFund.fund.id, srcAvailability, srcAmount, srcUnits)
    val srcAvailableAsset =
      if (isPayment || !opDateSelected || !srcSelected || !srcAvailabilitySelected) None
      else savings.computeAssets(operationDate).findAsset(operationDate, srcAsset)
    val (srcAmountPrompt, srcUnitsPrompt) = srcAvailableAsset match {
      case Some(asset) => (Some(s"≤${asset.amount}"), Some(s"≤${asset.units}"))
      case None        => (None, None)
    }
    val (srcAmountIssue, srcUnitsIssue) = {
      val srcAmountValueIssue =
        if (srcAmountValued) None
        else Some(positiveValueMsg)
      val srcUnitsValueIssue =
        if (srcUnitsValued) None
        else Some(positiveValueMsg)
      if (!isPayment && srcSelected && srcAvailabilitySelected) {
        srcAvailableAsset match {
          case Some(asset) =>
            val emptyOne = ((srcAsset.amount == asset.amount) && (srcAsset.units != asset.units)) ||
              ((srcAsset.amount != asset.amount) && (srcAsset.units == asset.units))
            val amountIssue =
              if (srcAmountValueIssue.nonEmpty) srcAmountValueIssue
              else if (srcAsset.amount > asset.amount) Some(valueLimitMsg.format(asset.amount))
              else if (emptyOne) Some(emptyOneMsg)
              else None
            val unitsIssue =
              if (srcUnitsValueIssue.nonEmpty) srcUnitsValueIssue
              else if (srcAsset.units > asset.units) Some(valueLimitMsg.format(asset.units))
              else if (emptyOne) Some(emptyOneMsg)
              else None
            (amountIssue, unitsIssue)

          case None =>
            (srcAmountValueIssue, srcUnitsValueIssue)
        }
      } else (srcAmountValueIssue, srcUnitsValueIssue)
    }
    val srcValuedOk = srcAmountIssue.isEmpty && srcUnitsIssue.isEmpty
    val srcOk = srcSelected && srcAvailabilitySelected && !srcAvailabilityAnterior && srcValuedOk
    Form.toggleError(srcFundField, !srcSelected,
      if (srcSelected) None
      else Some(mandatoryMsg)
    )
    Form.toggleStyles(srcAvailabilityField, None,
      Form.ErrorStyle(!srcAvailabilitySelected, mandatoryMsg),
      Form.ErrorStyle(srcAvailabilityAnterior, resources.getString("error.anterior-availability-date"))
    )
    Form.toggleError(srcAvailabilityField2, !srcAvailabilitySelected,
      if (srcAvailabilitySelected) None
      else Some(mandatoryMsg)
    )
    srcAmountField.setPromptText(srcAmountPrompt.orNull)
    Form.toggleError(srcAmountField, srcAmountIssue.nonEmpty, srcAmountIssue.orElse(srcAmountPrompt))
    srcUnitsField.setPromptText(srcUnitsPrompt.orNull)
    Form.toggleError(srcUnitsField, srcUnitsIssue.nonEmpty, srcUnitsIssue.orElse(srcUnitsPrompt))

    val dstNeeded = actionKind == AssetActionKind.Transfer
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
    lazy val dstAmount = getDstAmount
    val dstAmountValued = !dstNeeded || (dstAmount > 0)
    lazy val dstUnits = getDstUnits
    val dstUnitsValued = !dstNeeded || (dstUnits > 0)
    val dstValued = dstAmountValued && dstUnitsValued
    lazy val dstAsset = Savings.Asset(dstFund.scheme.id, dstFund.fund.id, dstAvailability, dstAmount, dstUnits)
    val dstOk = dstSelected && dstAvailabilitySelected && !dstAvailabilityAnterior && dstValued
    Form.toggleError(dstFundField, !dstSelected,
      if (dstSelected) None
      else Some(mandatoryMsg)
    )
    Form.toggleStyles(dstAvailabilityField, None,
      Form.ErrorStyle(!dstAvailabilitySelected, mandatoryMsg),
      Form.ErrorStyle(dstAvailabilityAnterior, resources.getString("error.anterior-availability-date"))
    )
    Form.toggleError(dstAmountField, !dstAmountValued,
      if (dstAmountValued) None
      else Some(positiveValueMsg)
    )
    Form.toggleError(dstUnitsField, !dstUnitsValued,
      if (dstUnitsValued) None
      else Some(positiveValueMsg)
    )

    val event = if (opDateOk && srcOk && dstOk) Some {
      actionKind match {
        case AssetActionKind.Payment  => Savings.MakePayment(operationDate, srcAsset)
        case AssetActionKind.Transfer => Savings.MakeTransfer(operationDate, srcAsset, dstAsset)
        case AssetActionKind.Refund   => Savings.MakeRefund(operationDate, srcAsset)
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

  private def getSrcAmount: BigDecimal =
    getBigDecimal(srcAmountField.getText)

  private def getSrcUnits: BigDecimal =
    getBigDecimal(srcUnitsField.getText)

  private def getDstFund: Option[SchemeAndFund] =
    Option(dstFundField.getValue).flatten

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

  /** Builds a dialog out of this controller. */
  def buildDialog(mainController: MainController, savings: Savings, kind: AssetActionKind.Value, asset: Option[Savings.Asset]): Dialog[Option[Savings.Event]] = {
    val resources = I18N.getResources

    val dialog = new Dialog[Option[Savings.Event]]()
    val title = s"${resources.getString("Payment")} / ${resources.getString("Transfer")} / ${resources.getString("Refund")}"
    dialog.setTitle(title)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val loader = new FXMLLoader(getClass.getResource("/fxml/new-asset-action.fxml"), resources)
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
    else controller.checkForm()
  }

}
