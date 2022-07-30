package epsa.tools

import epsa.model.Savings
import epsa.util.Http
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.concurrent.Future

/**
 * Amundi investment fund file downloader.
 */
object AmundiInvestmentFundDownloader {

  import epsa.Main.Akka._

  /** Date formatter. */
  private val dateFormatter = DateTimeFormatter.ofPattern("yyyyMMdd")

  def download(amfId: String, dateStart: Option[LocalDate] = None,
    dateEnd: Option[LocalDate] = None): Future[Savings.AssetValueHistory] =
  {
    val start = dateStart.getOrElse(LocalDate.of(1970, 1, 1))
    val end = dateEnd.getOrElse(LocalDate.now)
    val uri = getUri(amfId, start, end)
    Future {
      val body = Http.executeBinaryRequest(Http.buildHttpGet(uri))
      EsaliaInvestmentFundProber.probe(body).getOrElse {
        throw new Exception("Downloaded data could not be probed")
      }
    }
  }

  private def getUri(amfId: String, dateStart: LocalDate, dateEnd: LocalDate): String =
    s"https://www.societegeneralegestion.fr/psSGGestionEntr/ezaap/service/ExportDataProductSheet/Fwd/fr-FR/886/${
      amfId
    }/object/export/graphiquevl?bDate=${
      dateFormatter.format(dateStart)
    }&eDate=${
      dateFormatter.format(dateEnd)
    }&devise=3&duration=&perPage=0"

}
