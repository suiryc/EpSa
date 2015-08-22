package epsa.storage

import epsa.model.Savings
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.concurrent.Future

trait DataStore {

  protected val defaultPath = {
    // See: http://stackoverflow.com/a/12733172
    val path = Paths.get(getClass.getProtectionDomain.getCodeSource.getLocation.toURI)
    if (Files.isDirectory(path)) path
    else path.getParent
  }.resolve("default")

  def changePath(path: Path): Future[Unit]

  val eventSource: EventSource

  trait EventSource {

    protected[storage] val tableName = "eventsource"

    def readEvents(): Future[Seq[Savings.Event]]

    def writeEvents(events: Savings.Event*): Future[Unit]

    def writeEvents(events: List[Savings.Event]): Future[Unit] =
      writeEvents(events:_*)

  }

}
