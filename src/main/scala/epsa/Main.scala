package epsa

import akka.actor.ActorSystem
import epsa.controllers.MainController
import epsa.model.Savings
import java.util.prefs.Preferences
import javafx.event.ActionEvent
import javafx.application.{Application, Platform}
import javafx.stage.{Stage, WindowEvent}

object Main {

  /** Settings. */
  implicit val prefs = Preferences.userRoot.node("suiryc.epsa").node("epsa")

  def main(args: Array[String]): Unit = {
    (new Main).launch()
  }

  object Akka {

    implicit val system = ActorSystem("epsa")
    implicit val dispatcher = system.dispatcher

  }

  def shutdown(stage: Stage): Unit = {
    stage.close()
    shutdown()
  }

  def shutdown(): Unit = {
    Akka.system.shutdown()
    Platform.exit()
  }

}

class Main extends Application {

  def launch(): Unit = {
    Application.launch()
  }

  override def start(stage: Stage) {
    I18N.loadLocale()

    val savingsInit = new Savings()
    val state = MainController.State(
      stage = stage,
      savingsInit = savingsInit,
      eventsUpd = Nil,
      savingsUpd = savingsInit
    )
    stage.setTitle("EpSa")
    MainController.build(state)
  }

  private def onCloseRequest(controller: MainController)(event: WindowEvent): Unit =
    // Delegate closing request to controller
    controller.onExit(new ActionEvent())

}
