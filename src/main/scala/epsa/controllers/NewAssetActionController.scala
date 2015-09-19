package epsa.controllers

import epsa.I18N
import epsa.model.Savings
import java.util.ResourceBundle
import javafx.fxml.{FXMLLoader, FXML}
import javafx.scene.control.{RadioButton, ToggleGroup, ButtonType, Dialog}
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.util.Callback

// TODO - fields for date (of action and value), scheme&fund, availability, amount and units
// TODO - in case of transfer, enable fields to select destination
// TODO - button to select (existing) scheme&fund+availability
// TODO - handle some event (action ?) on amount/units to update counterpart (and also destination data if any)
// TODO - changing availability updates destination' one
// TODO - use value date to handle amount/units conversion on destination
// TODO - hint (form) error if amount/units exceeds asset for transfer/fund
// TODO - give info on limits for selected asset (and button to use all)
class NewAssetActionController {

  //@FXML
  //protected var location: URL = _

  @FXML
  protected var resources: ResourceBundle = _

  @FXML
  protected var actionKind: ToggleGroup = _

  @FXML
  protected var paymentButton: RadioButton = _

  @FXML
  protected var transferButton: RadioButton = _

  @FXML
  protected var refundButton: RadioButton = _

  //def initialize(): Unit = { }

  def initialize(savings0: Savings, dialog: Dialog[_], kind: AssetActionKind.Value, asset: Option[Savings.Asset]): Unit = {
    val toggle = kind match {
      case AssetActionKind.Payment  => paymentButton
      case AssetActionKind.Transfer => transferButton
      case AssetActionKind.Refund   => refundButton
    }
    actionKind.selectToggle(toggle)
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

    dialog.setResultConverter(Callback { resultConverter(controller) _ })
    Stages.trackMinimumDimensions(Stages.getStage(dialog))

    dialog
  }

  private def resultConverter(controller: NewAssetActionController)(buttonType: ButtonType): Option[Savings.Event] = {
    // TODO - build Event from of form fields
    if (buttonType != ButtonType.OK) None
    else None
  }

}
