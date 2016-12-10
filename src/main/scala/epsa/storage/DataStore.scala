package epsa.storage

import epsa.I18N.Strings
import epsa.model.Savings
import epsa.model.Savings.{Event, UnavailabilityPeriod}
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.sql.Date
import java.time.{LocalDate, Month}
import java.util.UUID
import javafx.stage.{FileChooser, Window}
import org.h2.engine.Constants
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Success
import slick.jdbc.H2Profile.api._
import slick.jdbc.H2Profile.backend.DatabaseDef
import slick.jdbc.meta.MTable
import slick.lifted.{PrimaryKey, ProvenShape}
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

  protected[epsa] def defaultPath = dbPathPref.option.getOrElse {
    val path = {
      // See: http://stackoverflow.com/a/12733172
      val appPath = Paths.get(getClass.getProtectionDomain.getCodeSource.getLocation.toURI)
      // We either got the application running jar (file), or the running folder
      if (Files.isDirectory(appPath)) appPath
      else appPath.getParent
    }
    path.resolve(s"default$dbExtension")
  }

  protected[storage] type DBAction[A] = DatabaseDef => Future[A]

  protected case class DBChange[A](action: DBAction[A]) {
    var applied: Boolean = false
  }

  /**
   * Temporary database.
   *
   * Keeps in mind actions performed (in memory) to apply them if requested in
   * real database.
   */
  protected case class DBTemp(db: DatabaseDef) {

    /** Changes applied since creation. */
    var changes: Map[DataStoreTable, Seq[DBChange[_]]] = Map[DataStoreTable, Seq[DBChange[_]]]()

    def hasPendingChanges: Boolean = changes.nonEmpty

    def resetChanges(table: DataStoreTable): Unit =
      changes -= table

    def addAction[A](table: DataStoreTable, action: DBAction[A]): Unit =
      changes += table -> (changes.getOrElse(table, Nil) :+ DBChange(action))

  }

  /** Real database. */
  protected[storage] case class DBInfo(db: DatabaseDef, path: Path)

  /** Temporary (in-memory) DB. */
  protected var dbTempOpt: Option[DBTemp] = None
  /** Real (physical) DB. */
  protected[storage] var dbRealOpt: Option[DBInfo] = None

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
  def getDBTemp: Future[DBTemp] =
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
      case None    => throw new Exception(Strings.noDataStoreSelected)
    }

  def open(owner: Option[Window], change: Boolean, save: Boolean): Option[Future[Unit]] = {
    // Close temporary database unless we are about to save
    if (!save) {
      closeTempDB()
    }

    if (change) {
      val fileChooser = new FileChooser()
      // Note: there is no way to set the fileChooser icon other than have it
      // inherit from its owner.
      fileChooser.setTitle(Strings.selectDataStore)
      fileChooser.getExtensionFilters.addAll(
        new FileChooser.ExtensionFilter(Strings.dataStore, "*.mv.db")
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
            case ex: Exception => Some(Future.failed(ex))
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
    dbOpen("temporary").flatMap { tmp =>
      dbTempOpt = Some(tmp)
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
        val actions = tmp.changes.flatMap {
          case (table, dbChanges) =>
            dbChanges.map { change =>
              Action(change.action(real.db).recover {
                // Wrap issue to indicate which table was not updated
                case ex: Exception =>
                  throw new Exception(s"Could not apply changes in table[${table.tableName}]", ex)
              }.andThen {
                case Success(_) =>
                  // Upon success, indicate the change as applied
                  change.applied = true
              })
            }
        }.toSeq
        // Execute tables changes sequentially.
        RichFuture.executeAllSequentially(stopOnError = true, actions).map(_ => ()).andThen {
          case Success(_) =>
            // Upon success close temporary db
            closeTempDB()

          case _ =>
            // Upon failure, only keep the changes that were not applied.
            // It should be safe since we try to apply changes sequentially.
            tmp.changes = tmp.changes.flatMap {
              case (table, dbChanges) =>
                val remaining = dbChanges.dropWhile(_.applied)
                if (remaining.isEmpty) None
                else Some(table -> remaining)
            }
        }

      case _ =>
        // Nothing to do since there are no changes to apply
        closeTempDB()
        Future.successful(())
    }
  }

  /** Saves temporary DB to physical one. */
  def save(): Future[Unit] = {
    dbRealOpt match {
      case Some(dbInfo) =>
        getDBTemp.flatMap { tmp =>
          copyDB(tmp.db, dbInfo.db)
        }.andThen {
          case _ => closeTempDB()
        }

      case None =>
        // There is nothing to save because there is no real db opened!
        throw new Exception("Cannot save data store because there is none opened yet")
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
    RichFuture.executeAllSequentially(stopOnError = true, actions: _*).map(_ => ())
  }

  /** Closes temporary database. */
  protected def closeTempDB(): Unit = {
    dbTempOpt.foreach { tmp =>
      // We really want to forget the in-memory DB, which is not the case by
      // simply calling 'close' because we had to set 'DB_CLOSE_DELAY' to '-1'.
      // There are various ways it can be achieved:
      // 1. Purging our tables
      // 2. Changing DB_CLOSE_DELAY to 0 before closing: 'SET DB_CLOSE_DELAY 0'
      // 3. Dropping all objects: 'DROP ALL OBJECTS'
      // Note: shutting down the DB does not purge the in-memory content.
      //
      // Tested with some real data (dozens of events, thousands of NAVs):
      // 1. ~100ms (250ms the first time)
      // 2. ~1ms (10ms the first time)
      // 3. 2-3ms (15ms the first time)
      Await.result(tmp.db.run(sql"SET DB_CLOSE_DELAY 0".as[String]), 10.seconds)
      tmp.db.close()
    }
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
        case ex: Exception => Future.failed(ex)
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
  protected[storage] def dbOpen(name: String): Future[DBTemp] = {
    // Note: by default in-memory db disappears when the last connection to it
    // is closed (which appears to be once a query is done). So we need to
    // prevent it with 'DB_CLOSE_DELAY=-1'.
    // See: http://stackoverflow.com/a/5936988
    val ref = Database.forURL(s"jdbc:h2:mem:$name;DB_CLOSE_DELAY=-1", driver = "org.h2.Driver")
    dbOpen(ref).map(DBTemp)
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
      RichFuture.executeAllSequentially(stopOnError = true, actions: _*)
      // Result: future completed after missing tables (if any) creation
    }.map { _ =>
      ref
    }
  }

  protected val tables = List[DataStoreTable](AppSettings, EventSource, AssetHistory, UnavailabilityPeriods)

  protected trait DataStoreTable {

    protected[DataStore] val tableName: String

    protected type Entry
    protected type Entries <: Table[Entry]

    protected val entries: TableQuery[Entries]

    protected[DataStore] def schema = entries.schema

    protected[storage] def newAction[A](action: DBAction[A], reset: Boolean = false, add: Boolean = true): Future[A] =
      getDBTemp.flatMap { tmp =>
        if (reset) tmp.resetChanges(this)
        if (add) tmp.addAction(this, action)
        action(tmp.db)
      }

    protected[DataStore] def deleteEntries(db: DatabaseDef): Future[Int] =
      db.run(entries.delete)

    def deleteEntries(): Future[Int] = {
      // We register action only if there was data to begin with, which
      // is not the case if there is no real db (yet) or real db had no
      // data.
      val hadData = dbRealOpt.map { dbReal =>
        dbReal.db.run(entries.take(1).result).map(_.nonEmpty)
      }.getOrElse(Future.successful(false))
      hadData.flatMap { v =>
        // Drop pending actions on this table since we are emptying it
        newAction(deleteEntries, reset = true, add = v)
      }
    }

    protected[storage] def readEntries(db: DatabaseDef): Future[Seq[Entry]] =
      db.run(entries.result)

    def readEntries(): Future[Seq[Entry]] =
      getDBRead.flatMap(readEntries)

    protected[storage] def writeEntry(db: DatabaseDef, value: Entry): Future[Unit] =
      db.run {
        entries.insertOrUpdate(value)
      }.map(_ => ())

    protected[DataStore] def writeEntries(db: DatabaseDef, values: Seq[Entry]): Future[Unit] =
      db.run {
        entries ++= values
      }.map(_ => ())

    def writeEntry(value: Entry): Future[Unit] =
      newAction(writeEntry(_, value))

    def writeEntries(values: Seq[Entry]): Future[Unit] = {
      // Don't do anything (and especially don't create a new pending action)
      // if there are no entries to write.
      if (values.isEmpty) Future.successful(())
      else newAction(writeEntries(_, values))
    }

  }

  object AppSettings extends DataStoreTable {

    override protected[DataStore] val tableName = "appSettings"

    val KEY_LEVIES = "levies"

    protected type Entry = (String, String)

    protected class Entries(tag: Tag) extends Table[Entry](tag, tableName) {
      def key: Rep[String] = column[String]("key", O.PrimaryKey)
      def value: Rep[String] = column[String]("value")
      def * : ProvenShape[(String, String)] = (key, value)
    }

    override protected val entries = TableQuery[Entries]

    def readEntry(key: String): Future[Option[String]] =
      getDBRead.flatMap { db =>
        db.run(entries.filter(_.key === key).map(_.value).take(1).result).map(_.headOption)
      }

    def deleteEntry(key: String): Future[Int] = {
      def delete(db: DatabaseDef): Future[Int] =
        db.run(entries.filter(_.key === key).delete)

      newAction(delete)
    }

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

    protected type Entry = (Long, Savings.Event)

    protected class Entries(tag: Tag) extends Table[Entry](tag, tableName) {
      def id: Rep[Long] = column[Long]("id", O.PrimaryKey, O.AutoInc)
      def event: Rep[Event] = column[Savings.Event]("event")
      def * : ProvenShape[(Long, Event)] = (id, event)
    }

    override protected val entries = TableQuery[Entries]

    def hasEvents(): Future[Boolean] =
      getDBRead.flatMap { db =>
        db.run(entries.size.result)
      }.map(_ > 0)

    def readEvents(): Future[Seq[Savings.Event]] =
      getDBRead.flatMap { db =>
        db.run(entries.sortBy(_.id).map(_.event).result)
      }

    def writeEvents(events: Seq[Savings.Event]): Future[Unit] =
      writeEntries(events.map { event =>
        // Note: "auto increment" field value will be ignored
        (0L, event)
      })

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
      def fundId: Rep[UUID] = column[UUID]("fundId")
      def date: Rep[LocalDate] = column[LocalDate]("date")
      def value: Rep[BigDecimal] = column[BigDecimal]("value", O.SqlType("DECIMAL(21,6)"))
      def pk: PrimaryKey = primaryKey("pk", (fundId, date))
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
            entries.filter(v => (v.fundId === fundId) && (v.date <= date)).sortBy(_.date.desc).take(1).result
          }
        }.map(r => getAssetValue(r).headOption)
      }

    def writeValues(fundId: UUID, values: Seq[Savings.AssetValue]): Future[Unit] = {
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

      // Don't do anything (and especially don't create a new pending action)
      // if there are no entries to write.
      if (values.isEmpty) Future.successful(())
      else newAction(write)
    }

    def writeValues(fundId: UUID, values: Savings.AssetValue*)(implicit d: DummyImplicit): Future[Unit] =
      writeValues(fundId, values)

    def deleteValues(fundId: UUID): Future[Int] = {
      def delete(db: DatabaseDef): Future[Int] =
        db.run(entries.filter(_.fundId === fundId).delete)

      getDBTemp.flatMap { tmp =>
        // We register action only if there was data to begin with, which
        // is not the case if there is no real db (yet) or real db had no
        // data for this fund.
        val hadData = dbRealOpt.map { dbReal =>
          dbReal.db.run(entries.filter(_.fundId === fundId).take(1).result).map(_.nonEmpty)
        }.getOrElse(Future.successful(false))
        hadData.flatMap { v =>
          if (v) tmp.addAction(this, delete)
          delete(tmp.db)
        }
      }
    }

    def cleanup(fundIds: List[UUID]): Future[Set[UUID]] = {
      val known = fundIds.toSet
      getDBRead.flatMap { db =>
        entries.distinctOn(_.fundId)
        db.run(entries.distinctOn(_.fundId).map(_.fundId).result).map { existing =>
          val orphans = existing.toSet -- known
          // Delete orphans if any
          if (orphans.nonEmpty) {
            val actions = orphans.toSeq.map { orphan =>
              Action(deleteValues(orphan))
            }
            // Delete as many as possible
            RichFuture.executeAllSequentially(stopOnError = false, actions)
          }
          orphans
        }
      }
    }

  }

  object UnavailabilityPeriods extends DataStoreTable {

    override protected[DataStore] val tableName = "unavailabilityPeriods"

    import scala.reflect._

    protected implicit def enumColumnType[A <: Enum[A]](implicit tag: ClassTag[A]) = MappedColumnType.base[A, String](
      { d => d.name },
      { d => Enum.valueOf(classTag[A].runtimeClass.asInstanceOf[Class[A]], d) }
    )

    protected type Entry = Savings.UnavailabilityPeriod

    protected class Entries(tag: Tag) extends Table[Entry](tag, tableName) {
      def id: Rep[String] = column[String]("id", O.PrimaryKey)
      def years: Rep[Int] = column[Int]("years")
      def month: Rep[Option[Month]] = column[Option[Month]]("month")
      def * : ProvenShape[UnavailabilityPeriod] = (id, years, month) <> (Savings.UnavailabilityPeriod.tupled, Savings.UnavailabilityPeriod.unapply)
    }

    override protected val entries = TableQuery[Entries]

    def updateEntry(id1: String, value: Entry): Future[Unit] = {
      def update(db: DatabaseDef): Future[Unit] =
        db.run {
          DBIO.sequence(List(
            entries.filter(_.id === id1).delete,
            entries += value
          )).transactionally
        }.map(_ => ())

      newAction(update)
    }

    def deleteEntry(id: String): Future[Int] = {
      def delete(db: DatabaseDef): Future[Int] =
        db.run(entries.filter(_.id === id).delete)

      newAction(delete)
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
    issueMsg(Strings.dataStoreWriteError, real)

  /**
   * Formats reading issue message.
   *
   * By default, reading is performed on real database if it is opened and no
   * in-memory one exists; so indicate its path in this case.
   */
  def readIssueMsg(real: Boolean = dbTempOpt.isEmpty && dbRealOpt.nonEmpty): String =
    issueMsg(Strings.dataStoreReadError, real)

  /**
   * Formats events reordering message.
   *
   * Reordering is based on real database.
   */
  def eventsReorderedMsg: String =
    issueMsg(Strings.dataStoreEventsReordered, real = true)

  /**
   * Formats cleanup message.
   *
   * Cleanup is based on real database.
   */
  def cleanupMsg: String =
    issueMsg(Strings.dataStoreCleaned, real = true)

  /**
   * Formats cleanup issue message.
   *
   * Cleanup is based on real database.
   */
  def cleanupIssueMsg: String =
    issueMsg(Strings.dataStoreCleanError, real = true)

}
