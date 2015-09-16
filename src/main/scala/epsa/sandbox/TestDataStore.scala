package epsa.sandbox

import epsa.model.Savings
import epsa.storage.DataStore
import javafx.application.Application
import javafx.stage.Stage
import scala.concurrent.{Await, Promise}
import scala.concurrent.duration.Duration

object TestDataStore {

  def main(args: Array[String]): Unit = {
    (new TestDataStore).launch()
  }

}

class TestDataStore extends Application {

  def launch(): Unit = {
    Application.launch()
  }

  override def start(stage: Stage) {
    import epsa.Main.Akka._

    val promise = Promise[Unit]()
    promise.future.onComplete {
      case _ => epsa.Main.shutdown()
    }

    try {
      if (DataStore.open(None, change = false).isEmpty) {
        DataStore.open(None, change = true).foreach { r =>
          Await.ready(r, Duration.Inf)
        }
      }
      DataStore.EventSource.readEvents().onComplete {
        case read =>
          println(s"EventSource.readEvents => $read")
          read.toOption.foreach { events =>
            println(Savings.processEvents(Savings(), events:_*))
          }
          DataStore.EventSource.writeEvents(
            Savings().createSchemeEvent("Scheme 1"),
            Savings().createFundEvent("Fund 1")
          ).onComplete {
            case write =>
              println(s"EventSource.writeEvents => $write")
              promise.success(())
          }
      }
    } catch {
      case ex: Throwable =>
        ex.printStackTrace()
        promise.failure(ex)
    }
  }

}
