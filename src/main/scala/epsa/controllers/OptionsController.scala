package epsa.controllers

import epsa.{I18N, Main, Settings}
import epsa.I18N.Strings
import epsa.util.JFXStyles
import javafx.collections.FXCollections
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.Node
import javafx.scene.control._
import javafx.stage.{Stage, Window}
import scala.concurrent.duration.FiniteDuration
import suiryc.scala.concurrent.duration.Durations
import suiryc.scala.javafx.stage.{StageLocationPersistentView, Stages}
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.scene.control.{Dialogs, FiniteDurationSpinnerValueFactory, I18NLocaleCell, Spinners}
import suiryc.scala.javafx.stage.Stages.StageLocation
import suiryc.scala.settings.{ConfigEntry, SettingSnapshot, SettingsSnapshot}
import suiryc.scala.util.I18NLocale

class OptionsController extends StageLocationPersistentView(OptionsController.stageLocation) {

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
  protected var httpClientTimeout: Spinner[Option[FiniteDuration]] = _

  protected var buttonOk: Node = _

  lazy protected val stage: Stage = httpClientTimeout.getScene.getWindow.asInstanceOf[Stage]

  def initialize(dialog: Dialog[_], snapshot: SettingsSnapshot): Unit = {
    // Load css
    JFXStyles.addStylesheet(stage.getScene)

    // Lookup OK dialog button
    buttonOk = dialog.getDialogPane.lookupButton(ButtonType.OK)

    // Note: the snapshot is used to check whether we will need to refresh
    // the main view; hence it does not contain the HTTP timeout.
    snapshot.add(
      SettingSnapshot(I18N.setting),
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
    locales.find(_.code == I18N.setting.get).foreach { locale =>
      languageChoice.getSelectionModel.select(locale)
    }

    val currency = settings.currency.get
    val currencies = if (Settings.preferredCurrencies.contains(currency)) {
      Settings.preferredCurrencies
    } else {
      currency :: Settings.preferredCurrencies
    }
    currencyChoice.setItems(FXCollections.observableList(currencies.asJava))
    currencyChoice.getSelectionModel.select(currency)

    amountScale.setValue(settings.amountScale.get.toDouble)
    amountRounding.setItems(FXCollections.observableList(BigDecimal.RoundingMode.values.toList.asJava))
    amountRounding.getSelectionModel.select(settings.amountRounding.get)

    unitsScale.setValue(settings.unitsScale.get.toDouble)
    unitsRounding.setItems(FXCollections.observableList(BigDecimal.RoundingMode.values.toList.asJava))
    unitsRounding.getSelectionModel.select(settings.unitsRounding.get)

    vwapScale.setValue(settings.vwapScale.get.toDouble)
    vwapRounding.setItems(FXCollections.observableList(BigDecimal.RoundingMode.values.toList.asJava))
    vwapRounding.getSelectionModel.select(settings.vwapRounding.get)

    Spinners.handleEvents(httpClientTimeout)
    httpClientTimeout.setValueFactory(new FiniteDurationSpinnerValueFactory(httpClientTimeout))
    httpClientTimeout.getEditor.textProperty.listen(checkForm())
    httpClientTimeout.getEditor.setText(settings.httpClientTimeout.rawOpt.map(_.unwrapped.toString).getOrElse(""))
  }

  private def checkForm(): Unit = {
    val ok = Durations.parseFinite(httpClientTimeout.getEditor.getText).exists(_.length > 0)
    JFXStyles.toggleError(httpClientTimeout, !ok, I18N.Strings.durationError)
    buttonOk.setDisable(!ok)
  }

  protected def applyChanges(snapshot: SettingsSnapshot): (Boolean, Boolean) = {
    Option(currencyChoice.getEditor.getText).filterNot(_.isEmpty).foreach(settings.currency.set)
    val restart = Option(languageChoice.getValue) match {
      case Some(locale) if locale.code != I18N.setting.get =>
        I18N.setLocale(locale.code)
        true

      case _ =>
        false
    }

    settings.amountScale.set(amountScale.getValue.round.toInt)
    settings.amountRounding.set(amountRounding.getValue)

    settings.unitsScale.set(unitsScale.getValue.round.toInt)
    settings.unitsRounding.set(unitsRounding.getValue)

    settings.vwapScale.set(vwapScale.getValue.round.toInt)
    settings.vwapRounding.set(vwapRounding.getValue)

    settings.httpClientTimeout.rawSet(httpClientTimeout.getEditor.getText)

    // Caller needs to reload (view) if something changed
    val reload = snapshot.changed()

    (reload || restart, restart)
  }

}

object OptionsController {

  // Note: the result of this dialog is whether the owner window needs to be
  // reloaded (language change)

  private val settings = Main.settings

  private val stageLocation = ConfigEntry.from[StageLocation](Main.settings.settings,
    Settings.prefix ++ Seq(Settings.KEY_STAGE, "options", Settings.KEY_LOCATION))

  def buildDialog(owner: Window): Dialog[(Boolean, Boolean)] = {
    val dialog = new Dialog[(Boolean, Boolean)]()
    // Note: initializing owner resets dialog icon, so set the icon afterwards
    Stages.initOwner(dialog, owner)
    Stages.getStage(dialog).getIcons.setAll(Images.iconGear)
    dialog.setTitle(Strings.options)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    // Note: snapshot is used to check whether something changed.
    // The controller does not actually apply changes until asked to.
    val snapshot = new SettingsSnapshot()

    val loader = new FXMLLoader(getClass.getResource("/fxml/options.fxml"), I18N.getResources)
    dialog.getDialogPane.setContent(loader.load[Node]())
    val controller = loader.getController[OptionsController]
    controller.initialize(dialog, snapshot)

    Dialogs.addPersistence(dialog, controller)

    dialog.setResultConverter(resultConverter(snapshot, controller) _)

    dialog
  }

  def resultConverter(snapshot: SettingsSnapshot, controller: OptionsController)(buttonType: ButtonType): (Boolean, Boolean) =
    if (buttonType != ButtonType.OK) (false, false)
    else controller.applyChanges(snapshot)

}
