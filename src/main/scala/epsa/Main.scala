package epsa

import akka.actor.ActorSystem
import epsa.controllers.MainController
import epsa.model.Savings
import java.util.prefs.Preferences
import javafx.event.ActionEvent
import javafx.application.{Application, Platform}
import javafx.stage.{Stage, WindowEvent}

object Main {

  var stage: Stage = _

  /** Settings. */
  implicit val prefs = Preferences.userRoot.node("suiryc.epsa").node("epsa")

  def main(args: Array[String]): Unit = {
    (new Main).launch()
  }

  object Akka {

    implicit val system = ActorSystem("epsa")
    implicit val dispatcher = system.dispatcher

  }

  def shutdown(): Unit = {
    Akka.system.shutdown()
    Option(stage).foreach(_.close())
    Platform.exit()
  }

}

class Main extends Application {

  import Main._

  def launch(): Unit = {
    Application.launch()
  }

  override def start(primaryStage: Stage) {
    I18N.loadLocale()

    stage = primaryStage

    MainController.build(primaryStage, new Savings())

    stage.setTitle("EpSa")
    stage.show()
  }

  private def onCloseRequest(controller: MainController)(event: WindowEvent): Unit =
    // Delegate closing request to controller
    controller.onExit(new ActionEvent())

}
