package epsa.storage

import akka.actor.Actor
import akka.actor.Props
import epsa.model.Savings
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import org.joda.time._
import scala.concurrent.Future
import scala.concurrent.Promise
import scalikejdbc._
import spray.json._

object DataStore {

  import epsa.Main.Akka._

  protected val defaultPath = {
    // See: http://stackoverflow.com/a/12733172
    val path = Paths.get(getClass.getProtectionDomain.getCodeSource.getLocation.toURI)
    if (Files.isDirectory(path)) path
    else path.getParent
  }.resolve("default")

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

  protected lazy val dbActor = system.actorOf(DSActor.props(defaultPath))

  def changePath(path: Path): Future[Unit] = {
    val promise = Promise[Unit]()
    dbActor ! DBChangePath(path, promise)
    promise.future
  }

  def doAction[A](action: DBSession => A): Future[A] = {
    val promise = Promise[A]()
    dbActor ! DBAction(action, promise)
    promise.future
  }

  val f1 = doAction { implicit session =>
    EventSource.readEvents()
  }
  f1.onComplete {
    case v =>
      println(s"EventSource.readEvents => $v")
      v.toOption.map { events =>
        println(Savings.processEvents(Savings(), events))
      }
      val f2 = doAction { implicit session =>
        EventSource.writeEvents(
          Savings().createSchemeEvent("Scheme 1"),
          Savings().createFundEvent("Fund 1")
        )
      }
      f2.onComplete {
        case v => println(s"EventSource.writeEvents => $v")
      }
  }

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

  object EventSource {

    import Savings.JsonProtocol._

    protected[DataStore] val tableName = "eventsource"

    protected[DataStore] val sqlCreateTable = SQL(s"""
create table if not exists $tableName (
  id bigint auto_increment primary key,
  event varchar not null,
  created_at timestamp not null
)
""")

    val entry = EventEntry.syntax("entry")
    val column = EventEntry.column

    protected case class EventEntry(id: Long, event: Savings.Event, createdAt: DateTime)

    protected object EventEntry extends SQLSyntaxSupport[EventEntry] {

      override val tableName = EventSource.tableName

      def apply(rs: WrappedResultSet): EventEntry = EventEntry(
        id = rs.long("id"),
        event = rs.string("event").parseJson.convertTo[Savings.Event],
        createdAt = rs.jodaDateTime("created_at")
      )

    }

    def readEvents()(implicit session: DBSession): List[Savings.Event] = withSQL {
      select.from(EventEntry as entry).orderBy(column.id)
    }.map { rs =>
      EventEntry(rs)
    }.list().apply().map(_.event)
    //sql"""select * from $tableName order by id""".map { rs =>
    //  EventEntry(rs)
    //}.list().apply().map(_.event)

    def writeEvents(events: Savings.Event*)(implicit session: DBSession): Unit = {
      val batchEvents = events.map { event =>
        Seq(event.toJson.compactPrint)
      }
      withSQL {
        insert.into(EventEntry).namedValues(column.event -> sqls.?, column.createdAt -> sqls.currentTimestamp)
      }.batch(batchEvents: _*).apply()
    }

  }

}
