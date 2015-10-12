package epsa.controllers

import epsa.I18N
import epsa.model.Savings
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.ResourceBundle
import javafx.event.ActionEvent
import javafx.collections.FXCollections
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.Node
import javafx.scene.control._
import javafx.stage.Stage
import javafx.util.converter.LocalDateStringConverter
import scala.collection.JavaConversions._
import suiryc.scala.javafx.stage.Stages.StageLocation
import suiryc.scala.settings.Preference
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.event.EventHandler._
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.util.Callback

// TODO - button to select (existing) scheme&fund+availability
// TODO - handle some event (action ?) on amount/units to update counterpart (and also destination data if any)
// TODO - use value date to handle amount/units conversion on destination
// TODO - give info on limits for selected asset (and button to use all)
// TODO - option of default day/month and number of years for frozen assets ?
// TODO - hint (tooltips/combobox) existing availability dates when applicable ?
// TODO - select first (if any) value of ComboBoxes by default ?
class NewAssetActionController {

  import epsa.Main.prefs

  private val stageLocation = Preference.from("stage.new-asset-action.location", null:StageLocation)

  //@FXML
  //protected var location: URL = _

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
  protected var srcFundField: ComboBox[SchemeAndFund] = _

  @FXML
  protected var srcAvailabilityField: DatePicker = _

  @FXML
  protected var srcAvailabilityField2: ComboBox[Option[LocalDate]] = _

  @FXML
  protected var srcAmountField: TextField = _

  @FXML
  protected var srcUnitsField: TextField = _

  @FXML
  protected var dstFundField: ComboBox[SchemeAndFund] = _

  @FXML
  protected var dstAvailabilityField: DatePicker = _

  @FXML
  protected var dstAmountField: TextField = _

  @FXML
  protected var dstUnitsField: TextField = _

  protected var buttonOk: Node = _

  private lazy val stage = paymentButton.getScene.getWindow.asInstanceOf[Stage]

  private lazy val toggleButtons = List(paymentButton, transferButton, refundButton)

  private lazy val mandatoryMsg = resources.getString("Mandatory field")

  private lazy val valueLimitMsg = resources.getString("Value exceeds available quantity")

  private var savings: Savings = _

  private var actionKind: AssetActionKind.Value = _

  private var recursionLevel = 0

  private var dstAvailabilityChosen = false

  //def initialize(): Unit = { }

  def initialize(savings0: Savings, dialog: Dialog[_], actionKind0: AssetActionKind.Value, asset: Option[Savings.Asset]): Unit = {
    // Save initial state
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

    // Note: we need to tell the combobox how to display both the 'button' area
    // (what is shown as selected) and the content (list of choices).
    for (field <- List(srcFundField, dstFundField)) {
      field.setButtonCell(new SchemeAndFundCell)
      field.setCellFactory(Callback { new SchemeAndFundCell })
    }

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
    dstAmountField.setDisable(disableDst)
    dstUnitsField.setDisable(disableDst)

    val srcAvailabilityExact = actionKind != AssetActionKind.Payment
    srcAvailabilityField.setVisible(!srcAvailabilityExact)
    srcAvailabilityField2.setVisible(srcAvailabilityExact)

    updateSchemeAndFund()
    if (srcAvailabilityExact) updateSrcAvailability()
    // Simulate changing source availability (to possibly change destination one)
    if (!disableDst) onSrcAvailability(new ActionEvent())
    checkForm()
  }

  def onOperationDate(event: ActionEvent): Unit = breakRecursion {
    updateSrcAvailability()
    checkForm()
  }

  def onSrcFund(event: ActionEvent): Unit = breakRecursion {
    // TODO - recompute amount based on units and operation date ?
    val srcAvailabilityExact = actionKind != AssetActionKind.Payment

    if (srcAvailabilityExact) updateSrcAvailability()
    updateDstSchemeAndFund()
    checkForm()
  }

  def onSrcAvailability(event: ActionEvent): Unit = breakRecursion {
    if (!dstAvailabilityChosen && isDstEnabled) {
      // Note: don't forget to use actual availability based o operation date
      val availability = Savings.resolveAvailablity(getSrcAvailability, Option(operationDateField.getValue))
      dstAvailabilityField.setValue(availability.orNull)
    }
    checkForm()
  }

  def onDstFund(event: ActionEvent): Unit = breakRecursion {
    // TODO - recompute amount based on units and operation date ?
    checkForm()
  }

  def onDstAvailability(event: ActionEvent): Unit = breakRecursion {
    dstAvailabilityChosen = Option(dstAvailabilityField.getValue).isDefined
    checkForm()
  }

  private def updateSchemeAndFund(): Unit = {
    // TODO - can have separation between list of proposed scheme&fund ?
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

      srcFundField.setItems(FXCollections.observableList(fundsWithAsset ::: fundsOther))
    } else {
      srcFundField.setItems(FXCollections.observableList(fundsWithAsset))
    }
    updateDstSchemeAndFund()
  }

  private def updateDstSchemeAndFund(): Unit = {
    // TODO - can have separation between list of proposed scheme&fund ?

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
      val fundsDst = Option(srcFundField.getValue).map { schemeAndFund =>
        val (fundsSameScheme1, fundsOtherScheme1) = fundsWithAsset.filterNot(_ == schemeAndFund).partition(_.scheme == schemeAndFund.scheme)
        val (fundsSameScheme2, fundsOtherScheme2) = fundsOther.partition(_.scheme == schemeAndFund.scheme)
        fundsSameScheme1 ::: fundsSameScheme2 ::: fundsOtherScheme1 ::: fundsOtherScheme2
      }.getOrElse(fundsWithAsset ::: fundsOther)

      dstFundField.setItems(FXCollections.observableList(fundsDst))
    }
  }

  private def updateSrcAvailability(): Unit = {
    // TODO - share ? (here, or in suiryc-scala)
    implicit val ordering: Ordering[LocalDate] = new Ordering[LocalDate]() {
      override def compare(x: LocalDate, y: LocalDate) = x.compareTo(y)
    }

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
        val availabilities = Option(srcFundField.getValue).map { schemeAndFund =>
          filterAssets(savings.computeAssets(date).assets, schemeAndFund).map(_.availability).distinct.sortBy { opt =>
            opt.getOrElse(LocalDate.ofEpochDay(0))
          }
        }.getOrElse(Nil)
        srcAvailabilityField2.setItems(FXCollections.observableList(availabilities))

      case None =>
        srcAvailabilityField2.setDisable(true)
    }
  }

  private def checkForm(): Option[Savings.Event] = {
    val operationDate = operationDateField.getValue
    val opDateOk = Option(operationDate).isDefined
    Form.toggleError(operationDateField, set = !opDateOk,
      if (opDateOk) None
      else Some(mandatoryMsg)
    )

    val isPayment = actionKind == AssetActionKind.Payment
    val srcFund = srcFundField.getValue
    val srcSelected = Option(srcFund).isDefined
    val srcAvailability = getSrcAvailability
    val srcAvailabilitySelected =
      (isPayment && Option(srcAvailabilityField.getValue).isDefined) ||
      (!isPayment && Option(srcAvailabilityField2.getValue).isDefined)
    val srcAmount = getSrcAmount
    val srcAmountValued = srcAmount > 0
    val srcUnits = getSrcUnits
    val srcUnitsValued = srcUnits > 0
    lazy val srcAsset = Savings.Asset(srcFund.scheme.id, srcFund.fund.id, srcAvailability, srcAmount, srcUnits)
    val (srcAmountOk, srcUnitsOk) =
      if (!isPayment && srcSelected && srcAvailabilitySelected) {
        savings.computeAssets(operationDate).findAsset(operationDate, srcAsset) match {
          case Some(asset) =>
            (srcAmountValued && (asset.amount >= srcAsset.amount),
              srcUnitsValued && (asset.units >= srcAsset.units))

          case None =>
            (false, false)
        }
      } else (srcAmountValued, srcUnitsValued)
    val srcValuedOk = srcAmountOk && srcUnitsOk
    val srcOk = srcSelected && srcAvailabilitySelected && srcValuedOk
    Form.toggleError(srcFundField, set = !srcSelected,
      if (srcSelected) None
      else Some(mandatoryMsg)
    )
    Form.toggleError(srcAvailabilityField, set = !srcAvailabilitySelected,
      if (srcAvailabilitySelected) None
      else Some(mandatoryMsg)
    )
    Form.toggleError(srcAvailabilityField2, set = !srcAvailabilitySelected,
      if (srcAvailabilitySelected) None
      else Some(mandatoryMsg)
    )
    Form.toggleError(srcAmountField, set = !srcAmountValued || !srcAmountOk,
      if (!srcAmountValued) Some(mandatoryMsg)
      else if (!srcAmountOk) Some(valueLimitMsg)
      else None
    )
    Form.toggleError(srcUnitsField, set = !srcUnitsValued || !srcUnitsOk,
      if (!srcUnitsValued) Some(mandatoryMsg)
      else if (!srcUnitsOk) Some(valueLimitMsg)
      else None
    )

    val dstNeeded = actionKind == AssetActionKind.Transfer
    lazy val dstFund = dstFundField.getValue
    val dstSelected = !dstNeeded || Option(dstFund).isDefined
    lazy val dstAvailability = Option(dstAvailabilityField.getValue)
    val dstAvailabilitySelected = !dstNeeded || {
      // We require to select a date if source has one
      getSrcAvailability.isEmpty || dstAvailability.isDefined
    }
    lazy val dstAmount = getDstAmount
    val dstAmountValued = !dstNeeded || (dstAmount > 0)
    lazy val dstUnits = getDstUnits
    val dstUnitsValued = !dstNeeded || (dstUnits > 0)
    val dstValued = dstAmountValued && dstUnitsValued
    lazy val dstAsset = Savings.Asset(dstFund.scheme.id, dstFund.fund.id, dstAvailability, dstAmount, dstUnits)
    val dstOk = dstSelected && dstAvailabilitySelected && dstValued
    Form.toggleError(dstFundField, set = !dstSelected,
      if (dstSelected) None
      else Some(mandatoryMsg)
    )
    Form.toggleError(dstAvailabilityField, set = !dstAvailabilitySelected,
      if (dstAvailabilitySelected) None
      else Some(mandatoryMsg)
    )
    Form.toggleError(dstAmountField, set = !dstAmountValued,
      if (dstAmountValued) None
      else Some(mandatoryMsg)
    )
    Form.toggleError(dstUnitsField, set = !dstUnitsValued,
      if (dstUnitsValued) None
      else Some(mandatoryMsg)
    )

    val event = if (opDateOk && srcOk && dstOk) Some {
      actionKind match {
        case AssetActionKind.Payment  => Savings.MakePayment(operationDate, srcAsset)
        case AssetActionKind.Transfer => Savings.MakeTransfer(operationDate, srcAsset, dstAsset)
        case AssetActionKind.Refund   => Savings.MakeRefund(operationDate, srcAsset)
      }
    } else None
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
  def buildDialog(savings: Savings, kind: AssetActionKind.Value, asset: Option[Savings.Asset]): Dialog[Option[Savings.Event]] = {
    val resources = I18N.getResources

    val dialog = new Dialog[Option[Savings.Event]]()
    val title = s"${resources.getString("Payment")} / ${resources.getString("Transfer")} / ${resources.getString("Refund")}"
    dialog.setTitle(title)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val loader = new FXMLLoader(getClass.getResource("/fxml/new-asset-action.fxml"), resources)
    dialog.getDialogPane.setContent(loader.load())
    val controller = loader.getController[NewAssetActionController]
    controller.initialize(savings, dialog, kind, asset)

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
