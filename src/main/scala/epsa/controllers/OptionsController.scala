package epsa.controllers

import epsa.I18N
import epsa.I18N.Strings
import epsa.util.JFXStyles
import javafx.collections.FXCollections
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.Node
import javafx.scene.control._
import javafx.stage.Window
import scala.concurrent.duration.{Duration, FiniteDuration}
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.settings.{SettingSnapshot, SettingsSnapshot}
import suiryc.scala.util.I18NLocale

class OptionsController {

  import OptionsController._

  @FXML
  protected var languageChoice: ComboBox[I18NLocale] = _

  @FXML
  protected var currencyChoice: ComboBox[String] = _

  @FXML
  protected var amountScale: Slider = _

  @FXML
  protected var amountRounding: ComboBox[BigDecimal.RoundingMode.Value] = _

  @FXML
  protected var unitsScale: Slider = _

  @FXML
  protected var unitsRounding: ComboBox[BigDecimal.RoundingMode.Value] = _

  @FXML
  protected var vwapScale: Slider = _

  @FXML
  protected var vwapRounding: ComboBox[BigDecimal.RoundingMode.Value] = _

  @FXML
  protected var httpClientTimeout: TextField = _

  protected var buttonOk: Node = _

  def initialize(dialog: Dialog[_], snapshot: SettingsSnapshot): Unit = {
    // Load css
    dialog.getDialogPane.getStylesheets.add(getClass.getResource("/css/form.css").toExternalForm)

    // Lookup OK dialog button
    buttonOk = dialog.getDialogPane.lookupButton(ButtonType.OK)

    snapshot.add(
      SettingSnapshot(I18N.pref),
      SettingSnapshot(settings.currency),
      SettingSnapshot(settings.amountScale),
      SettingSnapshot(settings.amountRounding),
      SettingSnapshot(settings.unitsScale),
      SettingSnapshot(settings.unitsRounding),
      SettingSnapshot(settings.vwapScale),
      SettingSnapshot(settings.vwapRounding)
    )

    // Note: we need to tell the combobox how to display both the 'button' area
    // (what is shown as selected) and the content (list of choices).
    languageChoice.setButtonCell(new I18NLocaleCell)
    languageChoice.setCellFactory(_ => new I18NLocaleCell)

    import scala.collection.JavaConverters._
    val locales = I18N.locales.sortBy(_.displayName)
    languageChoice.setItems(FXCollections.observableList(locales.asJava))
    locales.find(_.code == I18N.pref()).foreach { locale =>
      languageChoice.getSelectionModel.select(locale)
    }

    val currency = settings.currency()
    val currencies = if (settings.preferredCurrencies.contains(currency)) {
      settings.preferredCurrencies
    } else {
      currency :: settings.preferredCurrencies
    }
    currencyChoice.setItems(FXCollections.observableList(currencies.asJava))
    currencyChoice.getSelectionModel.select(currency)

    amountScale.setValue(settings.amountScale().toDouble)
    amountRounding.setItems(FXCollections.observableList(BigDecimal.RoundingMode.values.toList.asJava))
    amountRounding.getSelectionModel.select(settings.amountRounding())

    unitsScale.setValue(settings.unitsScale().toDouble)
    unitsRounding.setItems(FXCollections.observableList(BigDecimal.RoundingMode.values.toList.asJava))
    unitsRounding.getSelectionModel.select(settings.unitsRounding())

    vwapScale.setValue(settings.vwapScale().toDouble)
    vwapRounding.setItems(FXCollections.observableList(BigDecimal.RoundingMode.values.toList.asJava))
    vwapRounding.getSelectionModel.select(settings.vwapRounding())

    httpClientTimeout.textProperty.listen(checkForm())
    httpClientTimeout.setText(settings.httpClientTimeout().toString)
  }

  private def checkForm(): Unit = {
    val ok = try {
      Duration(httpClientTimeout.getText).isFinite()
      true
    } catch {
      case _: Throwable => false
    }
    JFXStyles.toggleError(httpClientTimeout, !ok,
      if (ok) None else Some(I18N.Strings.durationError))
    buttonOk.setDisable(!ok)
  }

  protected def applyChanges(snapshot: SettingsSnapshot): (Boolean, Boolean) = {
    Option(currencyChoice.getEditor.getText).filterNot(_.isEmpty).foreach { currency =>
      settings.currency() = currency
    }
    val restart = Option(languageChoice.getValue) match {
      case Some(locale) if locale.code != I18N.pref() =>
        I18N.setLocale(locale.code)
        true

      case _ =>
        false
    }

    settings.amountScale() = amountScale.getValue.round.toInt
    settings.amountRounding() = amountRounding.getValue

    settings.unitsScale() = unitsScale.getValue.round.toInt
    settings.unitsRounding() = unitsRounding.getValue

    settings.vwapScale() = vwapScale.getValue.round.toInt
    settings.vwapRounding() = vwapRounding.getValue

    settings.httpClientTimeout() = Duration(httpClientTimeout.getText).asInstanceOf[FiniteDuration]

    // Caller needs to reload (view) if something changed
    val reload = snapshot.changed()

    (reload || restart, restart)
  }

}

object OptionsController {

  // Note: the result of this dialog is whether the owner window needs to be
  // reloaded (language change)

  private val settings = epsa.Settings

  def buildDialog(owner: Option[Window]): Dialog[(Boolean, Boolean)] = {
    val dialog = new Dialog[(Boolean, Boolean)]()
    // Note: initializing owner resets dialog icon, so set the icon afterwards
    owner.foreach(dialog.initOwner)
    Stages.getStage(dialog).getIcons.setAll(Images.iconGear)
    dialog.setTitle(Strings.options)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    // Note: snapshot is used to check whether something changed.
    // The controller does not actually apply changes until asked to.
    val snapshot = new SettingsSnapshot()

    val loader = new FXMLLoader(getClass.getResource("/fxml/options.fxml"), I18N.getResources)
    dialog.getDialogPane.setContent(loader.load())
    val controller = loader.getController[OptionsController]
    controller.initialize(dialog, snapshot)

    dialog.setResultConverter(resultConverter(snapshot, controller) _)
    Stages.trackMinimumDimensions(Stages.getStage(dialog))

    dialog
  }

  def resultConverter(snapshot: SettingsSnapshot, controller: OptionsController)(buttonType: ButtonType): (Boolean, Boolean) =
    if (buttonType != ButtonType.OK) (false, false)
    else controller.applyChanges(snapshot)

}
