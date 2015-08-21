package epsa.storage

import epsa.model.Savings
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.concurrent.Future

trait DataStore[S] {

  // Note: using DataStore[_] functions does not work (compilation fails to
  // find implicit session parameter) if not defining Session as a type here.
  // If only having 'Session' as type parameter (that is DataStore[Session]),
  // 'session' parameter appears as 'Any', while it appears as
  // 'DataStore.this.Session' when using a type here.
  type Session = S

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
