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

}
