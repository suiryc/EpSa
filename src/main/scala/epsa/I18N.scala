package epsa

import suiryc.scala.util.I18NWithPreference

object I18N extends suiryc.scala.util.I18N("i18n.epsa") with I18NWithPreference {

  import epsa.Main.prefs
  import suiryc.scala.settings.Preference
  import suiryc.scala.settings.Preference._

  override val pref = Preference.from("locale.code", "en")

}
