package epsa

import akka.actor.ActorSystem
import epsa.controllers.MainController
import epsa.model.Savings
import epsa.util.Awaits
import javafx.application.{Application, Platform}
import javafx.stage.Stage
import scala.util.Success

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

    def startController(events: Seq[Savings.Event], outOfOrder: Boolean): Unit = {
      val savingsInit = Savings().processEvents(events)
      val state = MainController.State(
        stage = stage,
        savingsInit = savingsInit,
        savingsUpd = savingsInit
      )
      MainController.build(state, reorder = outOfOrder, applicationStart = true)
    }

    // Note: if stage has no Scene, have it owns a Dialog fails.
    // In any case, we have yet to build and show the stage.
    val events0 = Awaits.openDataStore(None, change = false, save = false) match {
      case Some(Success(())) =>
        // Data store opening succeeded: read events to replay
        // If we failed to read events, user was warned.
        Awaits.readDataStoreEvents(None).getOrElse(Nil)
      // Either there was an issue (notified to user) or no default data store
      case _ => Nil
    }
    val (events, outOfOrder) = Savings.sortEvents(events0)
    startController(events, outOfOrder)
  }

}
