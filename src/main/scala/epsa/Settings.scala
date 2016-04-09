package epsa

import java.util.prefs.Preferences
import suiryc.scala.settings.Preference
import suiryc.scala.settings.Preference._

/** Settings. */
object Settings {

  implicit val prefs = Preferences.userRoot.node("suiryc.epsa").node("epsa")

  val preferredCurrencies = List("€", "$", "£", "￥", "฿")
  val defaultCurrency = preferredCurrencies.head

  val currency = Preference.from("currency", defaultCurrency)

  implicit private val roundingMode = BigDecimal.RoundingMode

  val amountScale = Preference.from("amount.scale", 2)
  val amountRounding = Preference.from("amount.rounding", BigDecimal.RoundingMode.HALF_EVEN)

  val unitsScale = Preference.from("units.scale", 4)
  val unitsRounding = Preference.from("units.rounding", BigDecimal.RoundingMode.HALF_EVEN)

  // Note: not really useful to let user change percents scale/rounding
  val percentsScale = 2
  val percentsRounding = BigDecimal.RoundingMode.HALF_EVEN

  def scaleAmount(v: BigDecimal): BigDecimal =
    v.setScale(amountScale(), amountRounding())

  def scaleUnits(v: BigDecimal): BigDecimal =
    v.setScale(unitsScale(), unitsRounding())

  def scalePercents(v: BigDecimal): BigDecimal = {
    // Adapt scale according to value.
    // e.g. a value below 10 have the requested scale, a value below 100 have
    // its scale down by 1, a value below 1000 down by 2, etc.
    val scale = scala.math.max(0, percentsScale - (v.abs.intValue.toString.length - 1))
    v.setScale(scale, percentsRounding)
  }

}
