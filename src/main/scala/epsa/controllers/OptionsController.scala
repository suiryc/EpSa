package epsa.controllers

import epsa.I18N
import epsa.I18N.Strings
import javafx.collections.FXCollections
import javafx.fxml.{FXMLLoader, FXML}
import javafx.scene.control.{Slider, ButtonType, ComboBox, Dialog}
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.util.Callback
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

  def initialize(snapshot: SettingsSnapshot): Unit = {
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
    languageChoice.setCellFactory(Callback { new I18NLocaleCell })

    import scala.collection.JavaConversions._
    val locales = I18N.locales.sortBy(_.displayName)
    languageChoice.setItems(FXCollections.observableList(locales))
    locales.find(_.code == I18N.pref()).foreach { locale =>
      languageChoice.getSelectionModel.select(locale)
    }

    val currency = settings.currency()
    val currencies = if (settings.preferredCurrencies.contains(currency)) {
      settings.preferredCurrencies
    } else {
      currency :: settings.preferredCurrencies
    }
    currencyChoice.setItems(FXCollections.observableList(currencies))
    currencyChoice.getSelectionModel.select(currency)

    amountScale.setValue(settings.amountScale())
    amountRounding.setItems(FXCollections.observableList(BigDecimal.RoundingMode.values.toList))
    amountRounding.getSelectionModel.select(settings.amountRounding())

    unitsScale.setValue(settings.unitsScale())
    unitsRounding.setItems(FXCollections.observableList(BigDecimal.RoundingMode.values.toList))
    unitsRounding.getSelectionModel.select(settings.unitsRounding())

    vwapScale.setValue(settings.vwapScale())
    vwapRounding.setItems(FXCollections.observableList(BigDecimal.RoundingMode.values.toList))
    vwapRounding.getSelectionModel.select(settings.vwapRounding())
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

    // Caller needs to reload (view) if something changed
    val reload = snapshot.changed()

    (reload || restart, restart)
  }

}

object OptionsController {

  // Note: the result of this dialog is whether the owner window needs to be
  // reloaded (language change)

  private val settings = epsa.Settings

  def buildDialog(): Dialog[(Boolean, Boolean)] = {
    val dialog = new Dialog[(Boolean, Boolean)]()
    dialog.setTitle(Strings.options)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    // Note: snapshot is used to check whether something changed.
    // The controller does not actually apply changes until asked to.
    val snapshot = new SettingsSnapshot()

    val loader = new FXMLLoader(getClass.getResource("/fxml/options.fxml"), I18N.getResources)
    dialog.getDialogPane.setContent(loader.load())
    val controller = loader.getController[OptionsController]
    controller.initialize(snapshot)

    dialog.setResultConverter(Callback { resultConverter(snapshot, controller) _ })
    Stages.trackMinimumDimensions(Stages.getStage(dialog))

    dialog
  }

  def resultConverter(snapshot: SettingsSnapshot, controller: OptionsController)(buttonType: ButtonType): (Boolean, Boolean) =
    if (buttonType != ButtonType.OK) (false, false)
    else controller.applyChanges(snapshot)

}
