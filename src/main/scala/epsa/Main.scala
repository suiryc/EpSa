package epsa

import java.util.prefs.Preferences
import javafx.application.{Application, Platform}
import javafx.fxml.FXMLLoader
import javafx.scene.{Scene, Parent}
import javafx.stage.{Stage, WindowEvent}
import suiryc.scala.javafx.event.EventHandler._

object Main {

  var stage: Stage = _

  /** Settings. */
  implicit val prefs = Preferences.userRoot.node("suiryc.epsa").node("epsa")

  def main(args: Array[String]): Unit = {
    (new Main).launch()
  }

}

class Main extends Application {

  import Main._

  def launch(): Unit = {
    Application.launch()
  }

  override def start(primaryStage: Stage) {
    // XXX - GUI menu/option to change language
    I18N.loadLocale()

    storage.DataStore

    stage = primaryStage

    stage.setOnCloseRequest(onCloseRequest _)

    val root = FXMLLoader.load[Parent](getClass.getResource("/fxml/main.fxml"), I18N.getResources)

    /*pane.getChildren.setAll(root)
    AnchorPane.setTopAnchor(root, 0)
    AnchorPane.setRightAnchor(root, 0)
    AnchorPane.setBottomAnchor(root, 0)
    AnchorPane.setLeftAnchor(root, 0)*/

    stage.setScene(new Scene(root))

    stage.setTitle("EpSa")
    stage.show()
  }

  private def onCloseRequest(event: WindowEvent): Unit = {
    stage.close()
    Platform.exit()
  }

}
