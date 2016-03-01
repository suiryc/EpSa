package epsa.controllers

import epsa.I18N
import java.util.ResourceBundle
import javafx.collections.FXCollections
import javafx.fxml.{FXMLLoader, FXML}
import javafx.scene.control.{ButtonType, ComboBox, Dialog}
import suiryc.scala.javafx.stage.Stages
import suiryc.scala.javafx.util.Callback
import suiryc.scala.settings.{SettingSnapshot, SettingsSnapshot}
import suiryc.scala.util.I18NLocale

class OptionsController {

  @FXML
  protected var resources: ResourceBundle = _

  @FXML
  protected var languageChoice: ComboBox[I18NLocale] = _

  @FXML
  protected var currencyChoice: ComboBox[String] = _

  def initialize(snapshot: SettingsSnapshot): Unit = {
    snapshot.add(SettingSnapshot(I18N.pref))
    snapshot.add(SettingSnapshot(epsa.Settings.currency))

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

    val currency = epsa.Settings.currency()
    val currencies = if (epsa.Settings.preferredCurrencies.contains(currency)) {
      epsa.Settings.preferredCurrencies
    } else {
      currency :: epsa.Settings.preferredCurrencies
    }
    currencyChoice.setItems(FXCollections.observableList(currencies))
    currencyChoice.getSelectionModel.select(currency)
  }

}

object OptionsController {

  // Note: the result of this dialog is whether the owner window needs to be
  // reloaded (language change)

  def buildDialog(): Dialog[(Boolean, Boolean)] = {
    val resources = I18N.getResources

    val dialog = new Dialog[(Boolean, Boolean)]()
    dialog.setTitle(resources.getString("Options"))
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val snapshot = new SettingsSnapshot()

    val loader = new FXMLLoader(getClass.getResource("/fxml/options.fxml"), resources)
    dialog.getDialogPane.setContent(loader.load())
    val controller = loader.getController[OptionsController]
    controller.initialize(snapshot)

    dialog.setResultConverter(Callback { resultConverter(snapshot, controller) _ })
    Stages.trackMinimumDimensions(Stages.getStage(dialog))

    dialog
  }

  def resultConverter(snapshot: SettingsSnapshot, controller: OptionsController)(buttonType: ButtonType): (Boolean, Boolean) = {
    if (buttonType != ButtonType.OK) {
      snapshot.reset()
      (false, false)
    }
    else {
      val reload = Option(controller.currencyChoice.getEditor.getText).filterNot(_.isEmpty) match {
        case Some(currency) =>
          epsa.Settings.currency() = currency
          true

        case None =>
          false
      }
      val restart = Option(controller.languageChoice.getValue) match {
        case Some(locale) if locale.code != I18N.pref() =>
          I18N.setLocale(locale.code)
          true

        case _ =>
          false
      }

      (reload || restart, restart)
    }
  }

}
