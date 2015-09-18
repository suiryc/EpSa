package epsa.util

import epsa.I18N
import epsa.model.Savings
import epsa.storage.DataStore
import javafx.stage.Window
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Try}
import suiryc.scala.javafx.scene.control.Dialogs

object Awaits {

  def orError[A](future: Future[A], owner: Option[Window], msg: => String): Try[A] = {
    val r = Await.ready(future, Duration.Inf).value.get
    r match {
      case Failure(ex) => Dialogs.error(owner, None, Some(msg), ex)
      case _           =>
    }
    r
  }

  def openDataStore(owner: Option[Window], change: Boolean, save: Boolean): Option[Try[String]] =
    DataStore.open(owner, change, save).map { future =>
      Awaits.orError(future, owner, {
        // Note: if changing, we don't know what path we tried to open, but
        // the user shall know. Otherwise remind the default path.
        val msg = I18N.getResources.getString("Could not read data store")
        if (change) msg
        else s"$msg\n${DataStore.defaultPath}"
      })
    }

  def readDataStoreEvents(owner: Option[Window]): Try[Seq[Savings.Event]] =
    Awaits.orError(DataStore.EventSource.readEvents(), owner, {
      I18N.getResources.getString("Could not read data store") +
        s"\n${DataStore.defaultPath}"
    })

  def writeDataStoreEvents(owner: Option[Window], events: List[Savings.Event]): Try[Unit] =
    Awaits.orError(DataStore.EventSource.writeEvents(events), owner, {
      I18N.getResources.getString("Could not write data store") +
        s"\n${DataStore.defaultPath}"
    })

}
