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

object Awaits {

  import epsa.Main.Akka._

  def orError[A](future: Future[A], owner: Option[Window], msg: => String): Try[A] = {
    val r = Await.ready(future, Duration.Inf).value.get
    r match {
      case Failure(ex) => Dialogs.error(owner = owner, title = None, headerText = Some(msg), ex = Some(ex))
      case _           =>
    }
    r
  }

  def openDataStore(owner: Option[Window], change: Boolean, save: Boolean): Option[Try[Unit]] =
    DataStore.open(owner, change, save).map { future =>
      orError(future, owner, {
        // Note: if changing, we don't know what path we tried to open, but
        // the user shall know. Otherwise remind the real path.
        if (change && save) DataStore.writeIssueMsg(!change)
        else DataStore.readIssueMsg(!change)
      })
    }

  def hasDataStoreEvents(owner: Option[Window]): Try[Boolean] =
    orError(DataStore.EventSource.hasEvents(), owner, DataStore.readIssueMsg())

  def readDataStoreEvents(owner: Option[Window]): Try[Seq[Savings.Event]] =
    orError(DataStore.EventSource.readEvents(), owner, DataStore.readIssueMsg())

  def saveDataStoreChanges(owner: Option[Window], events: List[Savings.Event]): Try[Unit] = {
    // Note: we are supposed to have a real database opened by now. So we have
    // to first apply any pending changes from temporary database, then write
    // pending events in real database directly.
    val actions = Seq(Action(DataStore.saveChanges())) ++ (if (events.nonEmpty) {
      Some(Action(DataStore.EventSource.writeEvents(events)(Some(DataStore.getRealDB.db))))
    } else {
      None
    }).toSeq
    val f = RichFuture.executeSequentially(stopOnError = true, actions: _*).map(_ => ())
    orError(f, owner, DataStore.writeIssueMsg(real = true))
  }

  def readDataStoreNAV(owner: Option[Window], fundId: UUID, date: LocalDate, exactDate: Boolean = false): Try[Option[Savings.AssetValue]] =
    orError(DataStore.AssetHistory.readValue(fundId, date, exactDate), owner, DataStore.readIssueMsg())

  def saveDataStoreNAV(owner: Option[Window], fundId: UUID, nav: Savings.AssetValue): Try[Unit] =
    orError(DataStore.AssetHistory.writeValues(fundId, nav), owner, DataStore.writeIssueMsg())

  def cleanupDataStore(owner: Option[Window], fundIds: List[UUID]): Unit = {
    orError(DataStore.AssetHistory.cleanup(fundIds), owner, DataStore.cleanupIssueMsg) match {
      case Success(v) =>
        if (v.nonEmpty) Dialogs.information(owner = owner, title = None, headerText = None, contentText = Some(DataStore.cleanupMsg))

      case _ =>
        // Failure was already notified
    }
  }

}
