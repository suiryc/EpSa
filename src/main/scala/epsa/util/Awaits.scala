package epsa.util

import epsa.model.Savings
import epsa.storage.DataStore
import java.time.LocalDate
import java.util.UUID
import javafx.stage.Window
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}
import suiryc.scala.concurrent.RichFuture
import suiryc.scala.concurrent.RichFuture.Action
import suiryc.scala.javafx.scene.control.Dialogs
import suiryc.scala.math.Ordered._

object Awaits {

  import epsa.Main.Akka._

  def orError[A](future: Future[A], owner: Option[Window], msg: => String): Try[A] = {
    val r = Await.ready(future, Duration.Inf).value.get
    r match {
      case Failure(ex) => Dialogs.error(owner = owner, title = None, contentText = Some(msg), ex = Some(ex))
      case _           =>
    }
    r
  }

  def openDataStore(owner: Option[Window], change: Boolean = false, save: Boolean = false, loadTmp: Boolean = false): Option[Try[Unit]] = {
    // First copy real DB to tmp one if requested
    if (save && loadTmp) {
      orError(DataStore.getDBTemp, owner, DataStore.readIssueMsg(loadTmp))
    }

    // Then open database
    DataStore.open(owner, change, save).map { future =>
      orError(future, owner, {
        // Note: if changing, we don't know what path we tried to open, but
        // the user shall know. Otherwise remind the real path.
        if (change && save) DataStore.writeIssueMsg(!change)
        else DataStore.readIssueMsg(!change)
      })
    }
  }

  def hasDataStoreEvents(owner: Window): Try[Boolean] =
    orError(DataStore.EventSource.hasEvents, Some(owner), DataStore.readIssueMsg())

  def readDataStoreEvents(owner: Window): Try[List[Savings.Event]] =
    orError(DataStore.EventSource.readEvents(), Some(owner), DataStore.readIssueMsg()).map(_.toList)

  def getEventsHistory(owner: Window, extra: List[Savings.Event] = Nil, upTo: Option[LocalDate] = None): List[Savings.Event] = {
    val events0 = readDataStoreEvents(owner).getOrElse(Nil)
    val events1 =
      if (extra.isEmpty) events0
      else events0 ::: extra
    val events2 = Savings.normalizeEvents(events1)._1
    upTo match {
      case Some(date) =>
        events2.takeWhile {
          case e: Savings.AssetEvent => e.date <= date
          case _ => true
        }

      case None =>
        events2
    }
  }

  def writeDataStoreEvents(owner: Window, events: Seq[Savings.Event]): Try[Unit] =
    orError(DataStore.EventSource.writeEvents(events), Some(owner), DataStore.writeIssueMsg())

  def purgeDataStoreEvents(owner: Window): Try[Int] =
    orError(DataStore.EventSource.deleteEntries(), Some(owner), DataStore.writeIssueMsg())

  def saveDataStoreChanges(owner: Window, fullDb: Boolean): Try[Unit] = {
    // Note: we are supposed to have a real database opened by now.
    // We can now apply any pending changes from temporary database.
    val dbSave =
      if (fullDb) DataStore.save()
      else DataStore.saveChanges()
    orError(dbSave, Some(owner), DataStore.writeIssueMsg(real = true))
  }

  def applyDataStoreChanges(owner: Window, actions: List[RichFuture.Action[Unit, AnyVal]]): Try[Unit] = {
    val f = RichFuture.executeAllSequentially(stopOnError = true, actions).map(_ => ())
    orError(f, Some(owner), DataStore.writeIssueMsg())
  }

  def readDataStoreNAV(owner: Option[Window], fundId: UUID, date: LocalDate, exactDate: Boolean = false): Try[Option[Savings.AssetValue]] =
    orError(DataStore.AssetHistory.readValue(fundId, date, exactDate), owner, DataStore.readIssueMsg())

  def readDataStoreNAVs(owner: Window, fundId: UUID): Try[Seq[Savings.AssetValue]] =
    orError(DataStore.AssetHistory.readValues(fundId), Some(owner), DataStore.readIssueMsg())

  def saveDataStoreNAV(owner: Window, fundId: UUID, nav: Savings.AssetValue): Try[Unit] =
    orError(DataStore.AssetHistory.writeValues(fundId, nav), Some(owner), DataStore.writeIssueMsg())

  def readDataStoreUnavailabilityPeriods(owner: Window): Try[Seq[Savings.UnavailabilityPeriod]] =
    orError(DataStore.UnavailabilityPeriods.readEntries(), Some(owner), DataStore.readIssueMsg())

  def cleanupDataStore(owner: Window, fundIds: List[UUID], normalize: Boolean): Unit = {
    if (normalize) {
      val events0 = readDataStoreEvents(owner).getOrElse(Nil)
      val (events, modified) = Savings.normalizeEvents(events0)
      if (modified) {
        val actions = List(
          Action(DataStore.EventSource.deleteEntries()),
          Action(DataStore.EventSource.writeEvents(events))
        )
        val f = RichFuture.executeAllSequentially(stopOnError = true, actions).map(_ => ())
        orError(f, Some(owner), DataStore.writeIssueMsg()) match {
          case Success(_) =>
            Dialogs.information(owner = Some(owner), title = None, contentText = Some(DataStore.eventsReorderedMsg))

          case _ =>
            // Failure was already notified
        }
      }
    }
    orError(DataStore.AssetHistory.cleanup(fundIds), Some(owner), DataStore.cleanupIssueMsg) match {
      case Success(v) =>
        if (v.nonEmpty) Dialogs.information(owner = Some(owner), title = None, contentText = Some(DataStore.cleanupMsg))
        ()

      case _ =>
        // Failure was already notified
    }
  }

  def readAppSetting(owner: Window, key: String): Try[Option[String]] =
    orError(DataStore.AppSettings.readEntry(key), Some(owner), DataStore.readIssueMsg())

  def writeAppSetting(owner: Window, key: String, value: String): Try[Unit] =
    orError(DataStore.AppSettings.writeEntry((key, value)), Some(owner), DataStore.writeIssueMsg())

  def deleteAppSetting(owner: Window, key: String): Try[Int] =
    orError(DataStore.AppSettings.deleteEntry(key), Some(owner), DataStore.writeIssueMsg())

}
