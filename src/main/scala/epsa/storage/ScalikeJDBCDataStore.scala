package epsa.storage

import akka.actor.{Actor, Props}
import epsa.model.Savings
import java.nio.file.Path
import java.sql.Timestamp
//import org.joda.time.DateTime
import scala.concurrent.{Future, Promise}
import scalikejdbc._

object ScalikeJDBCDataStore extends DataStore {

  import epsa.Main.Akka._

  GlobalSettings.loggingSQLAndTime = LoggingSQLAndTimeSettings(
    enabled = true,
    singleLineMode = true,
    logLevel = 'debug,
    //printUnprocessedStackTrace = false,
    //stackTraceDepth= 15,
    warningEnabled = false,
    warningThresholdMillis = 3000L,
    warningLogLevel = 'warn
  )

  // Load H2 driver
  Class.forName("org.h2.Driver")

  // XXX - make sure to go up caller if actor creation fails (database init actually)
  protected lazy val dbActor = system.actorOf(DSActor.props(defaultPath))

  override def changePath(path: Path): Future[Unit] = {
    val promise = Promise[Unit]()
    dbActor ! DBChangePath(path, promise)
    promise.future
  }

  protected def doAction[A](action: DBSession => A): Future[A] = {
    val promise = Promise[A]()
    dbActor ! DBAction(action, promise)
    promise.future
  }

  val eventSource = EventSource

  protected case class DBAction[A](action: DBSession => A, promise: Promise[A])
  protected case class DBChangePath(path: Path, promise: Promise[Unit])

  protected object DSActor {
    def props(defaultDBPath: Path) = Props(new DSActor(defaultDBPath))
  }

  protected class DSActor(defaultPath: Path) extends Actor {

    // First open DB (and change 'receive' logic)
    dbOpen(defaultPath)

    // No implementation by default (is changed upon creation)
    override def receive: Receive = ???

    def receive(dbPath: Path)(implicit session: DBSession): Receive = {
      case DBAction(action, promise) => executeAction(action, promise)
      case DBChangePath(path, promise) => changePath(dbPath, path, promise)
    }

    def executeAction[A](action: DBSession => A, promise: Promise[A])(implicit session: DBSession): Unit =
      try {
        promise.success(action(session))
      } catch {
        case ex: Throwable => promise.failure(ex)
      }

    def executeQuery[A](label: String, sql: SQL[A, NoExtractor])(implicit session: DBSession): Boolean =
      try {
        sql.execute().apply()
      } catch {
        case ex: Throwable => throw new Exception(s"Could not $label", ex)
      }

    def dbOpen(path: Path): Unit = {
      ConnectionPool.singleton(s"jdbc:h2:$path", "user", "pass")

      // Note: it is important to get the DB session after initializing the
      // DB pool.
      implicit val session = AutoSession

      executeQuery(s"create '${EventSource.tableName}' table", EventSource.sqlCreateTable)

      context.become(receive(path))
    }

    def changePath(oldPath: Path, newPath: Path, promise: Promise[Unit]): Unit = {
      if (newPath.compareTo(oldPath) != 0) {
        try {
          dbOpen(newPath)
          promise.success()
        } catch {
          case ex: Throwable => promise.failure(ex)
        }
      }
    }

  }

  // Yes the EventSource object extends the inner EventSource trait from the
  // implemented DataSource trait.
  object EventSource extends EventSource {

    import spray.json._
    import Savings.JsonProtocol._

    protected[ScalikeJDBCDataStore] val sqlCreateTable = SQL(s"""
create table if not exists $tableName (
  id bigint auto_increment primary key,
  event varchar not null,
  created_at timestamp not null
)
""")

    protected val entry = Entries.syntax("entry")
    protected val column = Entries.column

    protected case class Entry(id: Long, event: Savings.Event, createdAt: Timestamp)

    protected object Entries extends SQLSyntaxSupport[Entry] {

      override val tableName = EventSource.tableName

      def apply(rs: WrappedResultSet): Entry = Entry(
        id = rs.long("id"),
        event = rs.string("event").parseJson.convertTo[Savings.Event],
        createdAt = rs.timestamp("created_at")//rs.jodaDateTime("created_at")
      )

    }

    protected def _readEvents()(implicit session: DBSession): Seq[Savings.Event] = withSQL {
      select.from(Entries as entry).orderBy(column.id)
    }.map { rs =>
      Entries(rs)
    }.list().apply().map(_.event)
    //sql"""select * from $tableName order by id""".map { rs =>
    //  EventEntry(rs)
    //}.list().apply().map(_.event)

    override def readEvents(): Future[Seq[Savings.Event]] =
      doAction { implicit session =>
        _readEvents()
      }

    protected def _writeEvents(events: Savings.Event*)(implicit session: DBSession): Unit = {
      val batchEvents = events.map { event =>
        Seq(event.toJson.compactPrint)
      }
      withSQL {
        insert.into(Entries).namedValues(column.event -> sqls.?, column.createdAt -> sqls.currentTimestamp)
      }.batch(batchEvents: _*).apply()
    }

    override def writeEvents(events: Savings.Event*): Future[Unit] =
      doAction { implicit session =>
        _writeEvents(events:_*)
      }

  }

}
