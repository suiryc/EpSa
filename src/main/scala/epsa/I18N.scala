package epsa

import java.io.{File, InputStream, InputStreamReader, IOException}
import java.net.JarURLConnection
import java.security.{AccessController, PrivilegedActionException, PrivilegedExceptionAction}
import java.util.{Locale, PropertyResourceBundle, ResourceBundle}
import java.util.zip.ZipFile
import suiryc.scala.io.NameFilter._
import suiryc.scala.io.PathFinder
import suiryc.scala.io.PathFinder._
import suiryc.scala.settings.Preference

object I18N {

  import epsa.Main.prefs

  private val localeCodePref = Preference.forString("locale.code", "en")

  /** I18N resources (relative) path. */
  private val i18nPath = "i18n/"
  /** I18N EpSa resource bundle name format. */
  private val i18nEpsaFormat = "epsa_.*.properties"

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
  private val languages: Set[String] = Option(getClass.getResource(s"/$i18nPath")).map { url =>
    url.getProtocol match {
      case "file" =>
        // Standard directory
        val file = new File(url.toURI)
        val finder: PathFinder = file * i18nEpsaFormat.r
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
            if (entryName.startsWith(i18nPath)) {
              val relativeName = entryName.substring(i18nPath.length)
              if ((relativeName.indexOf('/') != -1) || !relativeName.matches(i18nEpsaFormat)) None
              else Some(getLanguage(relativeName))
            } else None
          }.toSet
        } finally {
          zipFile.close()
        }

      case protocol =>
        // XXX - log
        println(s"Unhandled resource protocol: $protocol")
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

  def setLocale(localeCode: String): Unit =
    localeCodePref() = localeCode

  def getResources: ResourceBundle =
    ResourceBundle.getBundle("i18n.epsa", Locale.getDefault, UTF8Control)

  case class I18NLocale(code: String, displayName: String, locale: Locale)

}

/**
 * UTF-8 resource bundle control.
 *
 * Resource bundle load files assuming ISO-8859-1 content.
 * To override this behaviour, one solution is to redefine the 'Control' class
 * used, for example by overriding the 'newBundle' function in order to use an
 * UTF-8 input stream reader.
 *
 * This is what this UTF8Control does, as seen in online answers. The code was
 * adapted with latest ResourceBundle.Control.newBundle code and translated to
 * scala.
 *
 * See: http://stackoverflow.com/a/4660195 (which refers to
 * http://jdevelopment.nl/internationalization-jsf-utf8-encoded-properties-files/)
 */
object UTF8Control extends ResourceBundle.Control {

  override def newBundle(baseName: String, locale: Locale, format: String, loader: ClassLoader, reload: Boolean): ResourceBundle = {
    // Note: we only have to change the behaviour for 'java.properties' format
    if (format == "java.properties") {
      val  bundleName = toBundleName(baseName, locale)
      // Changed: was toResourceName0
      Option(toResourceName(bundleName, "properties")).flatMap { resourceName =>
        val stream = try {
          AccessController.doPrivileged(new PrivilegedExceptionAction[InputStream]() {
            def run: InputStream = {
              if (reload) {
                Option(loader.getResource(resourceName)).flatMap { url =>
                  Option(url.openConnection).map { connection =>
                    // Disable caches to get fresh data for reloading.
                    connection.setUseCaches(false)
                    connection.getInputStream
                  }
                }.orNull
              } else {
                loader.getResourceAsStream(resourceName)
              }
            }
          })
        } catch {
          case e: PrivilegedActionException => throw e.getException.asInstanceOf[IOException]
        }
        Option(stream).map {stream =>
          try {
            // Changed: was new PropertyResourceBundle(stream)
            new PropertyResourceBundle(new InputStreamReader(stream, "UTF-8"))
          } finally {
            stream.close()
          }
        }
      }.orNull
    } else super.newBundle(baseName, locale, format, loader, reload)
  }

}
