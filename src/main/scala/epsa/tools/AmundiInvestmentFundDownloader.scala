package epsa.tools

import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import epsa.model.Savings
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

  implicit private val materializer = ActorMaterializer()

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
    getFormParameters(uri, start, end).flatMap { formParameters =>
      val post = HttpRequest(
        method = HttpMethods.POST,
        uri = uri,
        entity = FormData(formParameters).toEntity
      )

      Http().singleRequest(post).flatMap { response =>
        val status = response.status
        if (!status.isSuccess()) {
          throw new Exception(s"Request failed: $status")
        }
        response.entity.dataBytes.runFold(ByteString.empty)(_ ++ _).map { body =>
          EsaliaInvestmentFundProber.probe(body.toArray).getOrElse {
            throw new Exception("Downloaded data could not be probed")
          }
        }
      }
    }
  }

  private def getUri(amfId: String): String =
    s"http://sggestion-ede.com/product/index.php?amf=$amfId&doc=FP&partner=fme-esalia"

  private def getFormParameters(uri: Uri, dateStart: LocalDate, dateEnd: LocalDate): Future[Map[String, String]] = {
    Http().singleRequest(HttpRequest(uri = uri)).flatMap { response =>
      val status = response.status
      if (!status.isSuccess()) {
        throw new Exception(s"Request failed: $status")
      }
      val entity = response.entity
      val charset = entity.contentType.charsetOption.getOrElse(HttpCharsets.`UTF-8`)
      entity.dataBytes.runFold(ByteString.empty)(_ ++ _).map(_.decodeString(charset.nioCharset))
    }.map { body =>
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
