package epsa

import akka.actor.ActorSystem
import epsa.controllers.MainController
import epsa.model.Savings
import epsa.util.Awaits
import javafx.application.{Application, Platform}
import javafx.stage.Stage

object Main {

  val name = "EpSa"

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
    Akka.system.terminate()
    Platform.exit()
  }

}

class Main extends Application {

  def launch(): Unit = {
    Application.launch()
  }

  override def start(stage: Stage) {
    I18N.loadLocale()

    def startController(): Unit = {
      // Note: actual state (account events) will be built by controller.
      val savingsInit = Savings()
      val state = MainController.State(
        stage = stage,
        savingsInit = savingsInit,
        savingsUpd = savingsInit
      )
      MainController.build(state, applicationStart = true)
    }

    // Note: if stage has no Scene, have it owns a Dialog fails.
    // In any case, we have yet to build and show the stage.
    Awaits.openDataStore(None)
    // If we failed to read events, user was warned.
    startController()
  }

}
