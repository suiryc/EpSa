package epsa.sandbox

import epsa.{model, storage}
import scala.concurrent.Promise

object TestDataStore {

  def main(args: Array[String]): Unit = {
    import epsa.Main.Akka._

    val promise = Promise[Unit]()
    promise.future.onComplete {
      case _ => epsa.Main.shutdown()
    }

    try {
      //val dataStore: storage.DataStore = storage.ScalikeJDBCDataStore
      val dataStore: storage.DataStore = storage.SlickDataStore

      dataStore.eventSource.readEvents().onComplete {
        case read =>
          println(s"EventSource.readEvents => $read")
          read.toOption.foreach { events =>
            println(model.Savings.processEvents(model.Savings(), events:_*))
          }
          dataStore.eventSource.writeEvents(
            model.Savings().createSchemeEvent("Scheme 1"),
            model.Savings().createFundEvent("Fund 1")
          ).onComplete {
            case write =>
              println(s"EventSource.writeEvents => $write")
              promise.success()
          }
      }
    } catch {
      case ex: Throwable =>
        ex.printStackTrace()
        promise.failure(ex)
    }
  }

}
