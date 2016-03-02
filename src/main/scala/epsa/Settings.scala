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

  def scaleAmount(v: BigDecimal): BigDecimal =
    v.setScale(amountScale(), amountRounding())

  def scaleUnits(v: BigDecimal): BigDecimal =
    v.setScale(unitsScale(), unitsRounding())

}
