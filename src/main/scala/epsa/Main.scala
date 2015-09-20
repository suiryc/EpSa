package epsa

import akka.actor.ActorSystem
import epsa.controllers.MainController
import epsa.model.Savings
import epsa.util.Awaits
import java.util.prefs.Preferences
import javafx.application.{Application, Platform}
import javafx.stage.Stage
import scala.util.Success

object Main {

  val name = "EpSa"

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

    def startController(dbOpened: Option[String], events: Seq[Savings.Event] = Nil): Unit = {
      val savingsInit = Savings().processEvents(events:_*)
      val state = MainController.State(
        stage = stage,
        savingsInit = savingsInit,
        savingsUpd = savingsInit,
        dbOpened = dbOpened
      )
      MainController.build(state)
    }

    // Note: if stage has no Scene, have it owns a Dialog fails.
    // In any case, we have yet to build and show the stage.
    Awaits.openDataStore(None, change = false, save = false) match {
      case Some(Success(name)) =>
        val dbOpened = Some(name)
        // Data store opening succeeded: read events to replay
        Awaits.readDataStoreEvents(None) match  {
          case Success(events) =>
            // Apply read events for initial savings
            startController(dbOpened = dbOpened, events)

          case _ =>
            // Failed to read events. User was warned.
            startController(dbOpened = dbOpened)
        }

      case _ =>
        // Either there was an issue (notified to user) or no default data store
        startController(dbOpened = None)
    }
  }

}
