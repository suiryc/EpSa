package epsa

import akka.actor.ActorSystem
import epsa.controllers.MainController
import epsa.model.Savings
import epsa.storage.DataStore
import java.util.prefs.Preferences
import javafx.application.{Application, Platform}
import javafx.stage.Stage
import scala.util.Failure
import scala.util.Success
import suiryc.scala.javafx.concurrent.JFXExecutor
import suiryc.scala.javafx.scene.control.Dialogs

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

    def startController(events: Seq[Savings.Event] = Nil): Unit = {
      val savingsInit = Savings.processEvents(new Savings(), events:_*)
      val state = MainController.State(
        stage = stage,
        savingsInit = savingsInit,
        eventsUpd = Nil,
        savingsUpd = savingsInit
      )
      stage.setTitle("EpSa")
      MainController.build(state)
    }

    // Note: if stage has no Scene, have it owns a Dialog fails.
    // In any case, we have yet to build and show the stage.
    DataStore.open(None, change = false) match {
      case Some(r) =>
        // Note: startController must be executed within JavaFX thread
        import JFXExecutor.executor

        r.onComplete {
          case Success(_) =>
            // Data store opening succeeded: read events to replay
            DataStore.EventSource.readEvents().onComplete {
              case Success(events) =>
                // Apply read events for initial savings
                startController(events)

              case Failure(ex) =>
                // Failed to read events: warn user
                Dialogs.error(None, None, Some(I18N.getResources.getString("Could not read data store")), ex)
                startController()
            }

          case Failure(ex) =>
            // Failed to open data store. User was warned. Start anew.
            startController()
        }

      case None =>
        // No default data store
        startController()
    }
  }

}
