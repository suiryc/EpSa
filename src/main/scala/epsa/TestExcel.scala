package epsa

import java.io.File
import java.util.prefs.Preferences
import javafx.application.Application
import javafx.scene.Scene
import javafx.stage.FileChooser.ExtensionFilter
import javafx.stage.{FileChooser, Stage, WindowEvent}
import suiryc.scala.settings.Preference

object TestExcel {

  def main(args: Array[String]): Unit = {
    (new TestExcel).launch()
  }

}

class TestExcel extends Application {

  /** Settings. */
  implicit val prefs = Preferences.userRoot.node("suiryc.epsa").node("epsa")

  def launch() {
    Application.launch()
  }

  override def start(stage: Stage) {
    val supportPathFolder = Preference.forString("support.path.folder", null)
    val supportPathFile = Preference.forString("support.path.file", null)
    println(supportPathFolder())
    println(supportPathFile())

    val fileChooser = new FileChooser()
    fileChooser.setTitle("Open Support File")
    fileChooser.getExtensionFilters.addAll(
      new ExtensionFilter("Excel Files", "*.xls", "*.xlsx"),
      new ExtensionFilter("All Files", "*.*")
    )
    supportPathFolder.option.foreach { folder =>
      fileChooser.setInitialDirectory(new File(folder))
      supportPathFile.option.foreach(fileChooser.setInitialFileName)
    }
    val selectedFile = fileChooser.showOpenDialog(stage)
    Option(selectedFile).flatMap { file =>
      EsaliaSupportProber.probe(file.toPath)
    } match {
      case Some(support) =>
        // Save path in preferences
        supportPathFolder() = selectedFile.getParent
        supportPathFile() = selectedFile.getName
        // Then build and display chart
        val chartHandler = new ChartHandler(support)
        val chartPane = chartHandler.chartPane
        chartPane.setPrefSize(640, 480)
        val scene = new Scene(chartPane)
        stage.setScene(scene)
        stage.show()

      case None =>
        stage.show()
        stage.fireEvent(new WindowEvent(stage, WindowEvent.WINDOW_CLOSE_REQUEST))
    }
  }

}
