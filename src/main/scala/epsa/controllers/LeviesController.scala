package epsa.controllers

import com.typesafe.scalalogging.StrictLogging
import epsa.I18N
import epsa.I18N.Strings
import epsa.model.{Levies, Savings}
import epsa.storage.DataStore._
import epsa.util.Awaits
import java.io.File
import java.net.URL
import java.nio.file.Path
import java.util.jar.JarFile
import javafx.collections.FXCollections
import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.Node
import javafx.scene.control.{ButtonType, ComboBox, Dialog, TextArea}
import javafx.stage.{FileChooser, Stage, Window, WindowEvent}
import scala.collection.JavaConverters._
import scala.io.Source
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.scene.control.{Dialogs, ListCellEx}
import suiryc.scala.javafx.stage.{FileChoosers, Stages}
import suiryc.scala.javafx.stage.Stages.StageLocation
import suiryc.scala.settings.Preference

class LeviesController extends StrictLogging {

  import LeviesController._

  @FXML
  protected var leviesField: ComboBox[LeviesAndJson] = _

  @FXML
  protected var leviesDescField: TextArea = _

  protected var buttonOk: Node = _

  private lazy val stage = leviesField.getScene.getWindow.asInstanceOf[Stage]

  private var currentLeviesAndJson: LeviesAndJson = _

  def initialize(savings: Savings, dialog: Dialog[_]): Unit = {
    // Lookup OK dialog button
    buttonOk = dialog.getDialogPane.lookupButton(ButtonType.OK)
    // Disable OK button unless there are changes to take into account
    buttonOk.setDisable(true)

    // Note: we need to tell the combobox how to display both the 'button' area
    // (what is shown as selected) and the content (list of choices).
    leviesField.setButtonCell(new LeviesCell)
    leviesField.setCellFactory(_ => new LeviesCell)

    val json = Awaits.readAppSetting(Some(stage), AppSettings.KEY_LEVIES).getOrElse(None).getOrElse("")
    currentLeviesAndJson = LeviesAndJson(savings.levies, json)

    // Get the json files from "levies" subfolder.
    val leviesPath = new File(getClass.getResource("/levies/").getFile)
    val jsons = if (leviesPath.getPath.matches("(?i)^.*\\.jar!.*$")) {
      // We are inside a jar.
      // The URL is of the form jar:file:/path/to/jar!/internal/path
      // The File keeps the "file:/" part. We can drop it by re-creating an URL
      // from this path. Then we drop everything after '!'.
      val jar = new JarFile(new URL(leviesPath.getPath).getFile.split('!').head)
      // Then filter entries to only get json files inside the subfolder
      jar.entries().asScala.toList.filter(_.getName.matches("(?i)^/?levies/[^/]+\\.json$")).map { entry =>
        (entry, Source.fromInputStream(jar.getInputStream(entry), "UTF-8").mkString)
      }
    } else {
      // Then filter files to only keep json ones inside the subfolder
      import suiryc.scala.io.NameFilter._
      leviesPath.listFiles("(?i)^.*\\.json$".r).toList.map { file =>
        (file, Source.fromFile(file, "UTF-8").mkString)
      }
    }
    val otherLevies = jsons.flatMap { case (entry, str) =>
      try {
        import spray.json._
        import Levies.JsonProtocol._
        val levies = str.parseJson.convertTo[Levies].normalized
        Some(LeviesAndJson(levies, str))
      } catch {
        case ex: Exception =>
          logger.warn(s"Could not parse <$entry> JSON levies: ${ex.getMessage}", ex)
          None
      }
    }.filterNot(_.levies == currentLeviesAndJson.levies)

    val allLevies = currentLeviesAndJson :: otherLevies :::
      (if (currentLeviesAndJson.levies == Levies.empty) Nil
      else List(LeviesAndJson(Levies.empty, "")))
    leviesField.getItems.setAll(FXCollections.observableList(allLevies.asJava))
    leviesField.getSelectionModel.select(currentLeviesAndJson)
    onLevies(null)
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

  def onCloseRequest(dialog: Dialog[_])(event: WindowEvent): Unit = {
    persistView()

    // Default dialog window closing request is handled to close the dialog
    // when applicable.
    // We do override this behaviour and need to close the dialog ourselves
    // if applicable.
    val changed = getLevies.exists(_.levies != currentLeviesAndJson.levies)
    val canClose =
      if (changed) Form.confirmDiscardPendingChanges(stage, event)
      else true
    if (canClose) dialog.close()
  }

  def onLevies(event: ActionEvent): Unit = {
    getLevies match {
      case Some(levies) => leviesDescField.setText(levies.json)
      case None         => leviesDescField.setText("")
    }
    checkForm()
  }

  def onImport(event: ActionEvent): Unit = {
    val fileChooser = new FileChooser()
    fileChooser.setTitle(Strings.importLevies)
    fileChooser.getExtensionFilters.addAll(
      new FileChooser.ExtensionFilter(Strings.jsonFiles, "*.json")
    )
    leviesImportPath.option.foreach { path =>
      FileChoosers.setInitialPath(fileChooser, path.toFile)
    }
    val selectedFile = fileChooser.showOpenDialog(stage)
    Option(selectedFile).foreach { file =>
      leviesImportPath() = selectedFile.toPath

      try {
        import spray.json._
        import Levies.JsonProtocol._
        val str = Source.fromFile(file, "UTF-8").mkString
        val levies = str.parseJson.convertTo[Levies].normalized
        val entry = LeviesAndJson(levies, str)
        if (!leviesField.getItems.contains(entry)) {
          leviesField.getItems.add(entry)
        }
        leviesField.getSelectionModel.select(entry)
        onLevies(null)
      } catch {
        case _: Exception =>
          Dialogs.warning(
            owner = Some(stage),
            title = None,
            headerText = Some(Strings.unhandledResource),
            contentText = Some(Strings.unknownFormat)
          )
      }
    }
    checkForm()
  }

  private def checkForm(): Unit = {
    val changed = getLevies.exists(_.levies != currentLeviesAndJson.levies)
    buttonOk.setDisable(!changed)
  }

  private def getLevies: Option[LeviesAndJson] =
    Option(leviesField.getValue)

}

object LeviesController {

  import epsa.Settings.prefs
  import Preference._

  private val stageLocation = Preference.from(prefs, "stage.levies.location", null:StageLocation)

  private val leviesImportPath = Preference.from(prefs, "levies.import.path", null:Path)

  case class LeviesAndJson(levies: Levies, json: String)

  class LeviesCell extends ListCellEx[LeviesAndJson] {
    override protected def itemText(item: LeviesAndJson): String =
      if (item.levies eq Levies.empty) Strings.na
      else if (item.levies.levies.isEmpty) item.levies.name
      else s"${item.levies.name} (${item.levies.date})"
  }

  /** Builds a dialog out of this controller. */
  def buildDialog(savings: Savings, window: Window): Dialog[Boolean] = {
    val dialog = new Dialog[Boolean]()
    val stage = Stages.getStage(dialog)
    stage.getIcons.setAll(Images.iconMoneyCoin)
    dialog.setTitle(Strings.levies)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val loader = new FXMLLoader(getClass.getResource("/fxml/levies.fxml"), I18N.getResources)
    dialog.getDialogPane.setContent(loader.load())
    val controller = loader.getController[LeviesController]
    controller.initialize(savings, dialog)

    // Delegate closing request to controller
    Stages.getStage(dialog).setOnCloseRequest(controller.onCloseRequest(dialog) _)

    // Wait for dialog to be shown before restoring the view
    dialog.showingProperty().listen2 { cancellable =>
      cancellable.cancel()
      controller.restoreView()
    }

    dialog.setResultConverter(resultConverter(window, controller) _)
    Stages.trackMinimumDimensions(Stages.getStage(dialog))

    dialog
  }

  private def resultConverter(window: Window, controller: LeviesController)(buttonType: ButtonType): Boolean = {
    controller.persistView()
    // Apply changes upon validation
    if (buttonType == ButtonType.OK) {
      controller.getLevies.foreach { levies =>
        if (levies.levies == Levies.empty) {
          Awaits.deleteAppSetting(Some(window), AppSettings.KEY_LEVIES)
        } else {
          Awaits.writeAppSetting(Some(window), AppSettings.KEY_LEVIES, levies.json)
        }
      }
      true
    } else {
      false
    }
  }

}
