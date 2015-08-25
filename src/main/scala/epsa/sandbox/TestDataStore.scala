package epsa.sandbox

import epsa.model.Savings
import epsa.storage.DataStore
import scala.concurrent.Promise

object TestDataStore {

  def main(args: Array[String]): Unit = {
    import epsa.Main.Akka._

    val promise = Promise[Unit]()
    promise.future.onComplete {
      case _ => epsa.Main.shutdown()
    }

    try {
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
