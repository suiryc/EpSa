package epsa.tools

import epsa.model.Savings
import epsa.util.Http
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import org.htmlcleaner.{HtmlCleaner, TagNode}
import scala.collection.JavaConverters._
import scala.concurrent.Future

/**
 * Amundi investment fund file downloader.
 */
object AmundiInvestmentFundDownloader {

  import epsa.Main.Akka._

  /** Tag id where to find the 'download' form. */
  private val FORM_ID = "rub-66"
  /** Tag id for form date start. */
  private val FORM_DATE_START_ID = "datepicker_debut_vl"
  /** Tag id for form date end. */
  private val FORM_DATE_END_ID = "datepicker_fin_vl"
  /** Date formatter. */
  private val dateFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")

  def download(amfId: String, dateStart: Option[LocalDate] = None,
    dateEnd: Option[LocalDate] = None): Future[Savings.AssetValueHistory] =
  {
    val uri = getUri(amfId)

    val start = dateStart.getOrElse(LocalDate.of(1970, 1, 1))
    val end = dateEnd.getOrElse(LocalDate.now)
    getFormParameters(uri, start, end).map { formParameters =>
      val body = Http.executeBinaryRequest(Http.buildHttpPost(uri.toString, formParameters.toSeq))
      EsaliaInvestmentFundProber.probe(body).getOrElse {
        throw new Exception("Downloaded data could not be probed")
      }
    }
  }

  private def getUri(amfId: String): String =
    s"http://sggestion-ede.com/product/index.php?amf=$amfId&doc=fp"

  private def getFormParameters(uri: String, dateStart: LocalDate, dateEnd: LocalDate): Future[Map[String, String]] = {
    Future {
      val body = Http.executeTextRequest(Http.buildHttpGet(uri))
      val parameters = extractFormParameters((new HtmlCleaner).clean(body))

      parameters ++ Map(
        FORM_DATE_START_ID -> dateFormatter.format(dateStart),
        FORM_DATE_END_ID -> dateFormatter.format(dateEnd)
      )
    }
  }

  private def extractFormParameters(root: TagNode): Map[String, String] = {
    val form = root.findElementByAttValue("id", FORM_ID, true, false)
    if (Option(form).isEmpty) {
      throw new Exception(s"Site does not contain expected form inside $FORM_ID element")
    }

    val inputs = form.getElementListByName("input", true).asScala.filter { node =>
      node.hasAttribute("name") && node.hasAttribute("value")
    }.map { node =>
      node.getAttributeByName("name") -> node.getAttributeByName("value")
    }.toMap

    val selects = form.getElementListByName("select", true).asScala.filter { node =>
      node.hasAttribute("name")
    }.flatMap { node =>
      node.getElementListByName("option", false).asScala.find { option =>
        option.hasAttribute("value") && option.hasAttribute("SELECTED")
      }.map { option =>
        node.getAttributeByName("name") -> option.getAttributeByName("value")
      }
    }.toMap

    val parameters = inputs ++ selects
    def checkParameter(id: String): Unit = {
      if (parameters.get(id).isEmpty) {
        throw new Exception(s"Site form does not contain expected input $id element")
      }
    }

    checkParameter(FORM_DATE_START_ID)
    checkParameter(FORM_DATE_END_ID)

    parameters
  }

}
