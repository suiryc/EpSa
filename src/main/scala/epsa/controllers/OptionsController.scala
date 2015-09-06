package epsa.controllers

import epsa.I18N
import java.util.ResourceBundle
import javafx.collections.FXCollections
import javafx.fxml.{FXMLLoader, FXML}
import javafx.scene.control.{ButtonType, ComboBox, Dialog, ListView}
import suiryc.scala.javafx.util.Callback._
import suiryc.scala.settings.{SettingSnapshot, SettingsSnapshot}

class OptionsController {

  //@FXML
  //protected var location: URL = _

  @FXML
  protected var resources: ResourceBundle = _

  @FXML
  protected var languageChoice: ComboBox[I18N.I18NLocale] = _

  //def initialize(): Unit = { }

  def initialize(snapshot: SettingsSnapshot): Unit = {
    snapshot.add(SettingSnapshot(I18N.localeCodePref))

    // Note: we need to tell the combobox how to display both the 'button' area
    // (what is shown as selected) and the content (list of choices).
    languageChoice.setButtonCell(new I18NLocaleCell)
    languageChoice.setCellFactory { (lv: ListView[I18N.I18NLocale]) =>
      new I18NLocaleCell
    }

    import scala.collection.JavaConversions._
    val locales = I18N.locales.sortBy(_.displayName)
    languageChoice.setItems(FXCollections.observableList(locales))
    locales.find(_.code == I18N.localeCodePref()).foreach { locale =>
      languageChoice.getSelectionModel.select(locale)
    }
  }

}

object OptionsController {

  // Note: the result of this dialog is whether the owner window needs to be
  // reloaded (language change)

  def buildDialog(): Dialog[Boolean] = {
    val resources = I18N.getResources

    val dialog = new Dialog[Boolean]()
    dialog.setTitle(resources.getString("Options"))
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val snapshot = new SettingsSnapshot()

    val loader = new FXMLLoader(getClass.getResource("/fxml/options.fxml"), resources)
    dialog.getDialogPane.setContent(loader.load())
    val controller = loader.getController[OptionsController]
    controller.initialize(snapshot)

    dialog.setResultConverter(resultConverter(snapshot, controller) _)

    dialog
  }

  def resultConverter(snapshot: SettingsSnapshot, controller: OptionsController)(buttonType: ButtonType): Boolean = {
    if (buttonType != ButtonType.OK) {
      snapshot.reset()
      false
    }
    else {
      Option(controller.languageChoice.getValue) match {
        case Some(locale) if locale.code != I18N.localeCodePref() =>
          I18N.setLocale(locale.code)
          true

        case _ =>
          false
      }
    }
  }

}
