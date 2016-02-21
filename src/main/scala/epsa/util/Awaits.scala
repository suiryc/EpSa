package epsa.util

import epsa.model.Savings
import epsa.storage.DataStore
import javafx.stage.Window
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Try}
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
        // the user shall know. Otherwise remind the default path.
        val msg =
          if (change && save) DataStore.writeIssueMsg
          else DataStore.readIssueMsg
        if (change) msg
        else s"$msg\n${DataStore.defaultPath}"
      })
    }

  def readDataStoreEvents(owner: Option[Window]): Try[Seq[Savings.Event]] =
    orError(DataStore.EventSource.readEvents(), owner, {
      DataStore.readIssueMsg +
        s"\n${DataStore.defaultPath}"
    })

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
    orError(f, owner, {
      DataStore.writeIssueMsg +
        s"\n${DataStore.defaultPath}"
    })
  }

}
