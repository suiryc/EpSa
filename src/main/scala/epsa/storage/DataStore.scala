package epsa.storage

import epsa.I18N
import epsa.model.Savings
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.sql.Timestamp
import java.time.Instant
import javafx.stage.{FileChooser, Window}
import scala.concurrent.Future
import slick.driver.H2Driver.api._
import slick.driver.H2Driver.backend.DatabaseDef
import slick.jdbc.meta.MTable
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.settings.Preference

object DataStore {

  import epsa.Main.Akka._
  import epsa.Main.prefs
  import Preference._

  private val dbExtension = ".mv.db"

  protected val dbPathPref = Preference.from("datastore.path", null:Path)

  protected[epsa] val defaultPath = dbPathPref.option.getOrElse {
    val path = {
      // See: http://stackoverflow.com/a/12733172
      val appPath = Paths.get(getClass.getProtectionDomain.getCodeSource.getLocation.toURI)
      if (Files.isDirectory(appPath)) appPath
      else appPath.getParent
    }
    path.resolve(s"default$dbExtension")
  }

  protected case class DBInfo(db: DatabaseDef, path: Path)

  protected var dbInfoOpt: Option[DBInfo] = None

  /**
   * Gets the DB.
   *
   * Throws an exception if DB is not opened.
   * Caller is responsible to ensure DB has been opened beforehand.
   */
  protected def getDBInfo: DBInfo =
    dbInfoOpt match {
      case Some(v) => v
      case None    => throw new Exception(I18N.getResources.getString("No data store selected"))
    }

  def open(owner: Option[Window], change: Boolean, save: Boolean): Option[Future[String]] = {
    val resources = I18N.getResources

    if (change) {
      val fileChooser = new FileChooser()
      fileChooser.setTitle(resources.getString("Select data store"))
      fileChooser.getExtensionFilters.addAll(
        new FileChooser.ExtensionFilter(resources.getString("Data store"), "*.mv.db")
      )
      fileChooser.setInitialDirectory(defaultPath.getParent.toFile)
      fileChooser.setInitialFileName(defaultPath.toFile.getName)

      // Note: file chooser must operate within JavaFX thread
      val selectedFile = if (save) {
        JFXSystem.await(fileChooser.showSaveDialog(owner.orNull), logReentrant = false)
      } else {
        JFXSystem.await(fileChooser.showOpenDialog(owner.orNull), logReentrant = false)
      }
      Option(selectedFile).flatMap { file =>
        // First delete existing file if saving
        val r0: Option[Future[String]] = if (save && file.exists) {
          try {
            if (file.delete()) None
            else Some(Future.failed(new Exception("Could not delete pre-existing data store")))
          } catch {
            case ex: Throwable => Some(Future.failed(ex))
          }
        } else None
        // If OK, do change db path
        r0.orElse(Some(changePath(file.toPath)))
      }
    }
    else if (Files.exists(defaultPath)) {
      Some(changePath(defaultPath))
    } else {
      None
    }
  }

  def close(): Unit = {
    dbInfoOpt.foreach(_.db.close())
    dbInfoOpt = None
  }

  protected def getName(path: Path): String = {
    val name0 = path.toFile.getName
    name0.substring(0, name0.length - dbExtension.length)
  }

  /**
   * Changes db path.
   *
   * If not equal to previous path, closes db and opens new path.
   */
  protected def changePath(newPath: Path): Future[String] =
    if (!dbInfoOpt.map(_.path).exists(_.compareTo(newPath) == 0)) {
      try {
        close()
        dbOpen(newPath).map(_ => getName(newPath))
      } catch {
        case ex: Throwable => Future.failed(ex)
      }
    } else Future.successful(getName(dbInfoOpt.get.path))

  protected def dbOpen(path: Path): Future[DBInfo] = {
    // Note: path contains extension, which is added by driver.
    val name = getName(path)
    val dbPath = path.getParent.resolve(name)

    // Open the new DB, and create missing tables
    val refNew = Database.forURL(s"jdbc:h2:$dbPath", user = "user", password = "pass", driver = "org.h2.Driver")

    refNew.run(MTable.getTables(EventSource.tableName)).flatMap { tables =>
      if (tables.nonEmpty) Future.successful(())
      else refNew.run(EventSource.entries.schema.create)
    }.map { _ =>
      // Automatically keep in mind the new DB
      val dbInfoNew = DBInfo(refNew, path)
      dbInfoOpt = Some(dbInfoNew)

      // Success, so save path
      dbPathPref() = path

      dbInfoNew
    }
  }

  object EventSource {

    import spray.json._
    import Savings.JsonProtocol._

    protected[DataStore] val tableName = "eventsource"

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

    def readEvents(): Future[Seq[Savings.Event]] =
      getDBInfo.db.run(entries.sortBy(_.id).map(_.event).result)

    def writeEvents(events: Savings.Event*): Future[Unit] =
      getDBInfo.db.run {
        entries ++= events.map { event =>
          // Note: "auto increment" field value will be ignored
          (0L, event, Timestamp.from(Instant.now))
        }
      }.map(_ => ())

    def writeEvents(events: List[Savings.Event]): Future[Unit] =
      writeEvents(events:_*)

  }

}
