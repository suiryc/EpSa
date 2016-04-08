package epsa.controllers

import epsa.I18N.Strings
import epsa.model.Savings
import java.time.LocalDate

object Form {

  def formatAvailability(availability: Option[LocalDate], date: Option[LocalDate], long: Boolean): String =
    availability.map { avail =>
      Savings.resolveAvailability(availability, date) match {
        case Some(_) =>
          avail.toString

        case None =>
          // Actually available
          Strings.available +
            (if (long) s" ($avail)" else "")
      }
    }.getOrElse(Strings.available)

  def formatAmount(amount: BigDecimal, currency: String): String =
    s"$amount $currency"

}

object AssetActionKind extends Enumeration {
  val Payment = Value
  val Transfer = Value
  val Refund = Value
}

case class SchemeAndFund(scheme: Savings.Scheme, fund: Savings.Fund) extends Ordered[SchemeAndFund] {

  def compare(other: SchemeAndFund): Int = {
    val c1 = scheme.name.compare(other.scheme.name)
    if (c1 != 0) c1
    else fund.name.compare(other.fund.name)
  }

}
