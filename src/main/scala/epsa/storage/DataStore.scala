package epsa.storage

import epsa.I18N
import epsa.model.Savings
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.sql.{Date, Timestamp}
import java.time.{Instant, LocalDate}
import java.util.UUID
import javafx.stage.{FileChooser, Window}
import org.h2.engine.Constants
import scala.concurrent.Future
import slick.driver.H2Driver.api._
import slick.driver.H2Driver.backend.DatabaseDef
import slick.jdbc.meta.MTable
import suiryc.scala.concurrent.RichFuture
import suiryc.scala.concurrent.RichFuture.Action
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.stage.FileChoosers
import suiryc.scala.settings.Preference

object DataStore {

  import epsa.Main.Akka._
  import epsa.Settings.prefs
  import Preference._

  private val dbExtension = Constants.SUFFIX_MV_FILE

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

  protected type DBAction[A] = DatabaseDef => Future[A]

  /**
   * Temporary database.
   *
   * Keeps in mind actions performed (in memory) to apply them if requested in
   * real database.
   */
  protected case class DBTemp(db: DatabaseDef) {

    /** Changes applied since creation. */
    var changes = Map[DataStoreTable, Seq[DBAction[_]]]()

    def hasPendingChanges = changes.nonEmpty

    def resetActions(table: DataStoreTable): Unit =
      changes -= table

    def addAction[A](table: DataStoreTable, action: DBAction[A]): Unit =
      changes += table -> (changes.getOrElse(table, Nil) :+ action)

  }

  /** Real database. */
  protected case class DBInfo(db: DatabaseDef, path: Path)

  /** Temporary (in-memory) DB. */
  protected var dbTempOpt: Option[DBTemp] = None
  /** Real (physical) DB. */
  protected var dbRealOpt: Option[DBInfo] = None

  /** Gets name (to display) from database path. */
  protected def getName(path: Path): String = {
    val name = path.toFile.getName
    name.substring(0, name.length - dbExtension.length)
  }

  /** Whether a real database is opened. */
  def dbOpened: Option[String] =
    dbRealOpt.map { real =>
      getName(real.path)
    }

  /** Whether there are pending changes (in temporary database). */
  def hasPendingChanges: Boolean =
    dbTempOpt.exists(_.hasPendingChanges)

  /**
   * Gets database reference for reading.
   *
   * If temporary db exists, use it.
   * Otherwise, if physical db exists, use it.
   * Otherwise create an empty temporary db to use it.
   */
  protected def getDBRead: Future[DatabaseDef] =
    dbTempOpt match {
      case Some(v) => Future.successful(v.db)
      case None    => dbRealOpt match {
        case Some(v) => Future.successful(v.db)
        case None    => buildTempDB().map(_.db)
      }
    }

  /**
   * Gets temporary database.
   *
   * Builds it (with physical one content if any) if necessary.
   */
  protected def getDBTemp: Future[DBTemp] =
    dbTempOpt match {
      case Some(v) => Future.successful(v)
      case None    => buildTempDB()
    }

  /**
   * Gets the real database.
   *
   * Throws an exception if DB is not opened.
   * Caller is responsible to ensure DB has been opened beforehand.
   */
  def getRealDB: DBInfo =
    dbRealOpt match {
      case Some(v) => v
      case None    => throw new Exception(I18N.getResources.getString("No data store selected"))
    }

  def open(owner: Option[Window], change: Boolean, save: Boolean): Option[Future[Unit]] = {
    val resources = I18N.getResources

    // Close temporary database unless we are about to save
    if (!save) {
      closeTempDB()
    }

    if (change) {
      val fileChooser = new FileChooser()
      fileChooser.setTitle(resources.getString("Select data store"))
      fileChooser.getExtensionFilters.addAll(
        new FileChooser.ExtensionFilter(resources.getString("Data store"), "*.mv.db")
      )
      FileChoosers.setInitialPath(fileChooser, defaultPath.toFile)

      // Note: file chooser must operate within JavaFX thread
      val selectedFile = if (save) {
        JFXSystem.await(fileChooser.showSaveDialog(owner.orNull), logReentrant = false)
      } else {
        JFXSystem.await(fileChooser.showOpenDialog(owner.orNull), logReentrant = false)
      }
      Option(selectedFile).flatMap { file =>
        // First delete existing file if saving
        val r0: Option[Future[Unit]] = if (save && file.exists) {
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

  /** Builds temporary DB out of physical one. */
  protected def buildTempDB(): Future[DBTemp] = {
    dbOpen().flatMap { tmp =>
      val copy = dbRealOpt match {
        case Some(dbInfo) => copyDB(dbInfo.db, tmp.db)
        case None         => Future.successful(())
      }
      copy.map { _ =>
        tmp
      }
    }
  }

  /** Saves temporary DB changes to physical one. */
  def saveChanges(): Future[Unit] = {
    dbTempOpt match {
      case Some(tmp) if tmp.hasPendingChanges =>
        val real = getRealDB
        val actions = tmp.changes.map {
          case (table, dbActions) =>
            // Execute table changes sequentially.
            val actions = dbActions.map { action =>
              Action(action(real.db))
            }
            Action {
              RichFuture.executeSequentially(stopOnError = true, actions:_*).map(_ => ()).recover {
                case ex: Throwable =>
                  throw new Exception(s"Could not apply changes in table[${table.tableName}]", ex)
              }
            }
        }.toSeq
        // Execute tables changes sequentially, then close temporary db.
        RichFuture.executeSequentially(stopOnError = true, actions:_*).map(_ => ()).andThen {
          case _ => closeTempDB()
        }

      case _ =>
        // Nothing to do since there are no changes to apply
        closeTempDB()
        Future.successful(())
    }
  }

  /**
   * Undo pending changes.
   *
   * Simply closes temporary database.
   */
  def undoChanges(): Unit = {
    closeTempDB()
  }

  /** Copies DB. */
  protected def copyDB(from: DatabaseDef, to: DatabaseDef): Future[Unit] = {
    val actions = tables.map { table =>
      Action {
        // First read source entries
        table.readEntries(from).flatMap { entries =>
          // Then clean destination
          table.deleteEntries(to).map(_ => entries)
        }.flatMap { entries =>
          // And finally write entries in destination
          table.writeEntries(to, entries)
        }
      }
    }
    RichFuture.executeSequentially(stopOnError = true, actions: _*).map(_ => ())
  }

  /** Closes temporary database. */
  protected def closeTempDB(): Unit = {
    dbTempOpt.foreach(_.db.close())
    dbTempOpt = None
  }

  /** Closes real database. */
  protected def closeRealDB(): Unit = {
    dbRealOpt.foreach(_.db.close())
    dbRealOpt = None
  }

  /** Close data store. */
  def close(): Unit = {
    closeTempDB()
    closeRealDB()
  }

  /**
   * Changes db path.
   *
   * If not equal to previous path, closes real db and opens new path.
   * Caller is expected to have closed temporary database when necessary.
   */
  protected def changePath(newPath: Path): Future[Unit] =
    if (!dbRealOpt.map(_.path).exists(_.compareTo(newPath) == 0)) {
      try {
        closeRealDB()
        dbOpen(newPath).map(_ => ())
      } catch {
        case ex: Throwable => Future.failed(ex)
      }
    } else Future.successful(())

  /** Opens real database. */
  protected def dbOpen(path: Path): Future[DBInfo] = {
    // Note: path contains extension, which is added by driver.
    val name = getName(path)
    val dbPath = path.getParent.resolve(name)

    // Open the new DB, and create missing tables
    val ref = Database.forURL(s"jdbc:h2:$dbPath", user = "user", password = "pass", driver = "org.h2.Driver")
    dbOpen(ref).map { _ =>
      // Automatically keep in mind the new DB
      val dbInfoNew = DBInfo(ref, path)
      dbRealOpt = Some(dbInfoNew)

      // Success, so save path
      dbPathPref() = path

      dbInfoNew
    }
  }

  /** Opens temporary database. */
  protected def dbOpen(): Future[DBTemp] = {
    // Note: by default in-memory db disappears when the last connection to it
    // is closed (which appears to be once a query is done). So we need to
    // prevent it with 'DB_CLOSE_DELAY=-1'.
    // See: http://stackoverflow.com/a/5936988
    val ref = Database.forURL("jdbc:h2:mem:temporary;DB_CLOSE_DELAY=-1", driver = "org.h2.Driver")
    dbOpen(ref).map { _ =>
      // Automatically keep in mind the new DB
      val dbTempNew = DBTemp(ref)
      dbTempOpt = Some(dbTempNew)

      dbTempNew
    }
  }

  /**
   * Prepares open database.
   *
   * Ensures tables are created.
   */
  protected def dbOpen(ref: DatabaseDef): Future[DatabaseDef] = {
    // First, get existing tables
    ref.run(MTable.getTables).flatMap { mTables =>
      // We only want the names
      val tableNames = mTables.map(_.name.name).toSet

      // Determine missing tables, and create them (sequentially)
      val actions = tables.filterNot { table =>
        tableNames.contains(table.tableName)
      }.map { table =>
        Action(ref.run(table.schema.create))
      }
      RichFuture.executeSequentially(stopOnError = true, actions: _*)
      // Result: future completed after missing tables (if any) creation
    }.map { _ =>
      ref
    }
  }

  protected val tables = List[DataStoreTable](EventSource, AssetHistory)

  protected trait DataStoreTable {

    protected[DataStore] val tableName: String

    protected type Entry
    protected type Entries <: Table[Entry]

    protected val entries: TableQuery[Entries]

    protected[DataStore] def schema = entries.schema

    protected[DataStore] def deleteEntries(db: DatabaseDef): Future[Int] =
      db.run(entries.delete)

    protected[DataStore] def readEntries(db: DatabaseDef): Future[Seq[Entries#TableElementType]] =
      db.run(entries.result)

    protected[DataStore] def writeEntries(db: DatabaseDef, values: Seq[Entries#TableElementType]): Future[Unit] =
      db.run {
        entries ++= values
      }.map(_ => ())

  }

  object EventSource extends DataStoreTable {

    import spray.json._
    import Savings.JsonProtocol._

    override protected[DataStore] val tableName = "eventSource"

    // Note: we manipulate 'Savings.Event' objects, but store them as String
    // so we need to define a mapping between the two.
    protected implicit val eventColumnType = MappedColumnType.base[Savings.Event, String](
      { e => e.toJson.compactPrint },
      { s => s.parseJson.convertTo[Savings.Event] }
    )

    protected type Entry = (Long, Savings.Event, Timestamp)

    protected class Entries(tag: Tag) extends Table[Entry](tag, tableName) {
      def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
      def event = column[Savings.Event]("event")
      def createdAt = column[Timestamp]("created_at")
      def * = (id, event, createdAt)
    }

    override protected val entries = TableQuery[Entries]

    def readEvents(): Future[Seq[Savings.Event]] =
      getDBRead.flatMap { db =>
        db.run(entries.sortBy(_.id).map(_.event).result)
      }

    protected def writeEvents(dbOpt: Option[DatabaseDef], events: Savings.Event*): Future[Unit] = {
      def write(db: DatabaseDef): Future[Unit] =
        db.run {
          entries ++= events.map { event =>
            // Note: "auto increment" field value will be ignored
            (0L, event, Timestamp.from(Instant.now))
          }
        }.map(_ => ())

      // Note: caller actually only writes events when user requested to save
      // changes, which means it will be done directly in real database, not
      // in memory. But keep the code generic and handle both cases.
      dbOpt match {
        case Some(db) => write(db)
        case None     =>
          getDBTemp.flatMap { tmp =>
            tmp.addAction(this, write)
            write(tmp.db)
          }
      }
    }

    def writeEvents(events: List[Savings.Event])(implicit dbOpt: Option[DatabaseDef] = None): Future[Unit] =
      writeEvents(dbOpt, events:_*)

  }

  object AssetHistory extends DataStoreTable {

    override protected[DataStore] val tableName = "assetHistory"

    // Note: slick does not handle java.time classes (support may appear in
    // v3.2). java.sql.Date does handle conversion, so use it explicitly in
    // the meantime.
    protected implicit val localDateColumnType = MappedColumnType.base[LocalDate, Date](
      { d => Date.valueOf(d) },
      { d => d.toLocalDate }
    )

    // Note: there are 2 ways to associate a case class to a table:
    // http://slick.typesafe.com/doc/3.1.0/schemas.html#mapped-tables
    // http://slick.typesafe.com/doc/3.1.0/userdefined.html#monomorphic-case-classes
    // The monomorphic variant compiles but IntelliJ complains for the '*'
    // function type.
    // In our case, we mix column and case class:
    // http://slick.typesafe.com/doc/3.1.0/userdefined.html#combining-mapped-types

    protected case class Entry(fundId: UUID, assetValue: Savings.AssetValue)

    protected case class LiftedAssetValue(date: Rep[LocalDate], value: Rep[BigDecimal])
    protected case class LiftedEntry(fundId: Rep[UUID], assetValue: LiftedAssetValue)

    implicit protected object AssetValueShape extends CaseClassShape(LiftedAssetValue.tupled, Savings.AssetValue.tupled)
    implicit protected object EntryShape extends CaseClassShape(LiftedEntry.tupled, Entry.tupled)

    protected class Entries(tag: Tag) extends Table[Entry](tag, tableName) {
      def fundId = column[UUID]("fundId")
      def date = column[LocalDate]("date")
      // TODO - 'scale' shall be configurable ? (usually 4)
      def value = column[BigDecimal]("value", O.SqlType("DECIMAL(21,6)"))
      def pk = primaryKey("pk", (fundId, date))
      def * = LiftedEntry(fundId, LiftedAssetValue(date, value))
    }

    override protected val entries = TableQuery[Entries]

    private def getAssetValue(r: Seq[Entry]): Seq[Savings.AssetValue] =
      r.map { entry =>
        entry.assetValue.copy(value = entry.assetValue.value.underlying().stripTrailingZeros)
      }

    def readValues(fundId: UUID): Future[Seq[Savings.AssetValue]] =
      getDBRead.flatMap { db =>
        db.run {
          entries.filter(_.fundId === fundId).sortBy(_.date).result
        }.map(getAssetValue)
      }

    def readValue(fundId: UUID, date: LocalDate, exactDate: Boolean = false): Future[Option[Savings.AssetValue]] =
      getDBRead.flatMap { db =>
        db.run {
          // Ideally we would like the value for the given date, but any
          // preceding date could do too.
          if (exactDate) {
            entries.filter(v => (v.fundId === fundId) && (v.date === date)).result
          } else {
            entries.filter(v => (v.fundId === fundId) && (v.date <= date)).sortBy(_.date.desc).result
          }
        }.map(r => getAssetValue(r).headOption)
      }

    def writeValues(fundId: UUID, values: Savings.AssetValue*)(implicit dbOpt: Option[DatabaseDef] = None): Future[Unit] = {
      // Note: we may have to update existing values, not only insert.
      // The easiest way to do that is to call 'insertOrUpdate' for every entry
      // to process, then group all those actions in a DBIO.sequence to run,
      // possibly with 'transactionally' or 'withPinnedSession'.
      // To be maybe faster, we could also do that directly as a JDBC batch,
      // but that's not really worth it here.
      // See: http://stackoverflow.com/a/35006433
      def write(db: DatabaseDef): Future[Unit] =
        db.run {
          DBIO.sequence(values.map { value =>
            entries.insertOrUpdate(Entry(fundId, value))
          }).transactionally
        }.map(_ => ())

      dbOpt match {
        case Some(db) => write(db)
        case None     =>
          getDBTemp.flatMap { tmp =>
            tmp.addAction(this, write)
            write(tmp.db)
          }
      }
    }

    def writeValues(fundId: UUID, values: Seq[Savings.AssetValue]): Future[Unit] =
      writeValues(fundId, values:_*)

    def deleteValues(fundId: UUID)(implicit dbOpt: Option[DatabaseDef] = None): Future[Int] = {
      def delete(db: DatabaseDef): Future[Int] =
        db.run(entries.filter(_.fundId === fundId).delete)

      dbOpt match {
        case Some(db) => delete(db)
        case None     =>
          getDBTemp.flatMap { tmp =>
            // Note: caller is expected to call us *because* there are data to
            // delete. If needed, we could still check we really have entries
            // with 'db.run(entries.length.result)'.

            // No need to keep previous actions since we delete everything
            tmp.resetActions(this)
            // TODO: should we keep in mind real db we are based on (if any)
            // and not register the delete action if there was originally no
            // data ?
            tmp.addAction(this, delete)
            delete(tmp.db)
          }
      }
    }

  }

  private def issueMsg(msg: String, real: Boolean): String = {
    if (real) s"$msg\n$defaultPath"
    else msg
  }

  /**
   * Formats writing issue message.
   *
   * By default, writing is performed in-memory, so don't indicate real
   * database path.
   */
  def writeIssueMsg(real: Boolean = false): String =
    issueMsg(I18N.getResources.getString("Could not write data store"), real)

  /**
   * Formats reading issue message.
   *
   * By default, reading is performed on real database if it is opened and no
   * in-memory one exists; so indicate its path in this case.
   */
  def readIssueMsg(real: Boolean = dbTempOpt.isEmpty && dbRealOpt.nonEmpty): String =
    issueMsg(I18N.getResources.getString("Could not read data store"), real)

}
