package epsa

import java.nio.file.Paths

import grizzled.slf4j.Logging
import java.io.File
import java.net.JarURLConnection
import java.util.{Locale, ResourceBundle}
import java.util.zip.ZipFile
import suiryc.scala.io.NameFilter._
import suiryc.scala.io.PathFinder
import suiryc.scala.io.PathFinder._
import suiryc.scala.settings.Preference
import suiryc.scala.util.UTF8Control

object I18N extends Logging {

  import Preference._
  import epsa.Main.prefs

  private[epsa] val localeCodePref = Preference.from("locale.code", "en")

  private val baseName = "i18n.epsa"

  private val resourceNameSuffix = "properties"

  /** Resource bundle path and name prefix. */
  val (resourcePath, resourceNamePrefix) = {
    // Use resource 'Control' to translate base name into resource name
    val fullName = UTF8Control.toResourceName(baseName, resourceNameSuffix)
    // Then get base path and filename from it
    val path = Paths.get(fullName)
    val name = path.getFileName.toString
    (s"${path.getParent}/", name.substring(0, name.length - (resourceNameSuffix.length + 1)))
  }

  /** Resource bundle name format (non-ROOT). */
  private val resourceNameFormat = s"${resourceNamePrefix}_.*\\.$resourceNameSuffix"

  /** Gets 'language' from resource name. */
  private def getLanguage(name: String): String =
    name.substring(5, name.length - 11)

  /**
   * Languages that we handle.
   *
   * We search for the I18N resources path URL, which may be a standard file
   * directory, or a jar (zip) file entry.
   * Then we list files/entries relatively to this URL that do match the
   * bundle resource name format, and extract the 'language' from its name.
   * We also add the 'en' language which is the default.
   *
   * Note: we could use a virtual file system framework (like common-vfs), but
   * we only search for file/entry names.
   */
  private val languages: Set[String] = Option(getClass.getResource(s"/$resourcePath")).map { url =>
    url.getProtocol match {
      case "file" =>
        // Standard directory
        val file = new File(url.toURI)
        val finder: PathFinder = file * resourceNameFormat.r
        finder.get().map(file => getLanguage(file.getName))

      case "jar" =>
        // Jar (zip) file entry
        // Find the actual jar file, and open it as a zip
        val file = new File(url.openConnection().asInstanceOf[JarURLConnection].getJarFileURL.toURI)
        val zipFile = new ZipFile(file)
        try {
          import scala.collection.JavaConversions._
          // Search for entries
          zipFile.entries.flatMap { entry =>
            val entryName = entry.getName
            if (entryName.startsWith(resourcePath)) {
              val relativeName = entryName.substring(resourcePath.length)
              if ((relativeName.indexOf('/') != -1) || !relativeName.matches(resourceNameFormat)) None
              else Some(getLanguage(relativeName))
            } else None
          }.toSet
        } finally {
          zipFile.close()
        }

      case protocol =>
        warn(s"Unhandled resource protocol: $protocol")
        Set.empty[String]
    }
  }.getOrElse(Set.empty) + "en"

  /**
   * Locales that we handle.
   *
   * Java resource bundles and locales use a lenient form with underscore '_'
   * as separator for language/country/variant instead of hyphen as specified
   * in BCP 47 (e.g. en_US instead of en-US).
   * Split on the separator to get each part and build the corresponding locale.
   */
  val locales = languages.map { lang =>
    val split = lang.split("_", 3)
    val locale =
      if (split.length == 1) new Locale(split(0))
      else if (split.length == 2) new Locale(split(0), split(1))
      else new Locale(split(0), split(1), split(2))

    I18NLocale(locale.toString, locale.getDisplayName(locale).capitalize, locale)
  }.toList.sortBy(_.code)

  def loadLocale(): Unit = {
    val localeCode = localeCodePref()
    locales.find(_.code == localeCode).foreach { locale =>
      Locale.setDefault(locale.locale)
    }
  }

  /**
   * Sets locale.
   *
   * Note: JavaFX statically loads some resources (e.g. default dialog buttons
   * label, empty list text, etc.) which cannot be resetted. Changing locale
   * at runtime has no effect for those already loaded.
   */
  def setLocale(localeCode: String): Unit = {
    localeCodePref() = localeCode
    loadLocale()
  }

  // Note: if key is missing in bundle, it is searched in parent (if any).
  // Exception is thrown if it is missing in whole chain.
  def getResources: ResourceBundle =
    ResourceBundle.getBundle(baseName, Locale.getDefault, UTF8Control)

  case class I18NLocale(code: String, displayName: String, locale: Locale)

}
