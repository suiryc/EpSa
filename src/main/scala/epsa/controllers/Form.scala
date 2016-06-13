package epsa.controllers

import epsa.I18N.Strings
import epsa.model.Savings
import java.time.LocalDate
import javafx.event.Event
import javafx.scene.control.ButtonType
import javafx.stage.Window
import suiryc.scala.javafx.scene.control.Dialogs

object Form {

  def textOrNone(v: String): Option[String] =
    Option(v).map(_.trim).find(_.nonEmpty)

  def formatAvailability(availability: Option[LocalDate], date: Option[LocalDate]): String =
    availability.map { avail =>
      Savings.resolveAvailability(availability, date) match {
        case Some(_) =>
          avail.toString

        case None =>
          // Actually available
          Strings.available
      }
    }.getOrElse(Strings.available)

  def formatAmount(amount: BigDecimal, suffix: String): String =
    Option(suffix).find(!_.isEmpty).map { _ =>
      s"$amount $suffix"
    }.getOrElse(amount.toString)

  /**
   * Asks user confirmation to discard pending changes.
   *
   * If user does not confirm, the event is consumed.
   *
   * @param owner the dialog owner window
   * @param event the event that triggered the confirmation
   * @return whether user confirmed discarding pending changes
   */
  def confirmDiscardPendingChanges(owner: Window, event: Event): Boolean = {
    val resp = Dialogs.confirmation(
      owner = Some(owner),
      title = None,
      headerText = Some(Strings.pendingChanges)
    )

    if (!resp.contains(ButtonType.OK)) {
      event.consume()
      false
    }
    else true
  }

  /**
   * Converts sequence of lists into list of options.
   *
   * Flattens elements into a list of options containing original elements
   * in order separated by Nones.
   * e.g. (a, b, c), (d, e) gives (Some(a), Some(b), Some(c), None, Some(d), Some(e))
   */
  def buildOptions[A](lists: List[A]*): List[Option[A]] =
    lists.foldLeft(List[Option[A]]()) { (acc, list) =>
      val rest = list.map(Some(_))
      if (acc.isEmpty || list.isEmpty) acc ::: rest
      else acc ::: None :: rest
    }

}

object AssetActionKind extends Enumeration {
  val Payment = Value
  val Transfer = Value
  val Refund = Value
}

case class SchemeAndFund(scheme: Savings.Scheme, fund: Savings.Fund) extends Ordered[SchemeAndFund] {

  val id = Savings.AssetId(scheme.id, fund.id)

  def compare(other: SchemeAndFund): Int = {
    val c1 = scheme.name.compare(other.scheme.name)
    if (c1 != 0) c1
    else fund.name.compare(other.fund.name)
  }

}
