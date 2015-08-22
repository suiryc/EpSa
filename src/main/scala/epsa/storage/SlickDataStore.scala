package epsa.storage

import epsa.model.Savings
import java.nio.file.Path
import java.sql.Timestamp
import java.time.Instant
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import slick.driver.H2Driver.api._
import slick.driver.H2Driver.backend.DatabaseDef
import slick.jdbc.meta.MTable

object SlickDataStore extends DataStore {

  import epsa.Main.Akka._

  protected case class DB(db: DatabaseDef, path: Path)

  protected var db: Option[DB] = None

  /** Gets the DB (opening it if not done). */
  protected def getDB(): DB =
    db match {
      case Some(v) => v
      case None    => Await.result(dbOpen(defaultPath), Duration.Inf)
    }

  def changePath(newPath: Path): Future[Unit] = {
    val oldDB = getDB()
    val oldPath = oldDB.path
    if (newPath.compareTo(oldPath) != 0) {
      try {
        val r = dbOpen(newPath).map(_ => ())
        oldDB.db.close()
        r
      } catch {
        case ex: Throwable => Future.failed(ex)
      }
    } else Future.successful()
  }

  protected def dbOpen(path: Path): Future[DB] = {
    // Open the new DB, and create missing tables
    val refNew = Database.forURL(s"jdbc:h2:$path", user = "user", password = "pass", driver = "org.h2.Driver")

    refNew.run(MTable.getTables(eventSource.tableName)).flatMap { tables =>
      if (tables.nonEmpty) Future.successful()
      else refNew.run(eventSource.entries.schema.create)
    }.map { _ =>
      // Automatically keep in mind the new DB
      val dbNew = DB(refNew, path)
      db = Some(dbNew)
      dbNew
    }
  }

  val eventSource = EventSource

  // Yes the EventSource object extends the inner EventSource trait from the
  // implemented DataSource trait.
  object EventSource extends EventSource {

    import spray.json._
    import Savings.JsonProtocol._

    // Note: we manipulate 'Savings.Event' objects, but store them as String
    // so we need to define a mapping between the two.
    protected implicit val eventColumnType = MappedColumnType.base[Savings.Event, String](
      { e => e.toJson.compactPrint },
      { s => s.parseJson.convertTo[Savings.Event] }
    )

    protected class Entries(tag: Tag) extends Table[(Long, Savings.Event, Timestamp)](tag, tableName) {
      def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
      def event = column[Savings.Event]("event")
      def createdAt = column[Timestamp]("created_at")
      def * = (id, event, createdAt)
    }

    val entries = TableQuery[Entries]

    override def readEvents(): Future[Seq[Savings.Event]] =
      getDB().db.run(entries.sortBy(_.id).map(_.event).result)

    override def writeEvents(events: Savings.Event*): Future[Unit] =
      getDB().db.run {
        entries ++= events.map { event =>
          // Note: "auto increment" field value will be ignored
          (0L, event, Timestamp.from(Instant.now))
        }
      }.map(_ => ())

  }

}
