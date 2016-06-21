package epsa.model

import epsa.Settings
import epsa.Settings.{Debug, DebugString, scaleAmount, splitAmount}
import java.time.LocalDate
import scala.collection.immutable.ListMap
import spray.json._
import suiryc.scala.math.Ordered._
import suiryc.scala.spray.json.JsonFormats

// Notes:
// (Tax) levy is used for naming the financial concept.
// (Tax/Levy) rate is used for the proportion applied on an amount by a levy.
// Amount levied is the actual amount due, e.g. when applying the levy rate on a gained amount.

case class LevyPeriod(
  rate: BigDecimal,
  start: LocalDate,
  end: Option[LocalDate]
) extends DebugString {
  override protected def debugString: String =
    s"Period($start->${end.getOrElse("")} @ $rate%)"
}

case class Levy(
  name: String,
  periods: List[LevyPeriod]
) extends DebugString {
  override protected def debugString: String =
    s"Levy($name,${periods.mkString(",")})"
}

case class Levies(
  name: String,
  date: LocalDate,
  levies: Map[String, Levy]
) extends DebugString {

  lazy val list = levies.values.toList

  lazy val normalized: Levies = {
    val startDate = levies.flatMap(_._2.periods.map(_.start)).min
    val normalizedLevies = levies.mapValues { levy =>
      // If a period has no end and is followed by another period, set its
      // end to the start of the next period.
      // If a period has an end and the next one does not start the next day,
      // introduce a '0 rate' fake period in-between. Also add a fake period
      // without end if the last period has an end, and a fake first period
      // for levies to all start on the same date.
      // As a trick, append periods in reverse order (so that it is easy to
      // get the previous period at each iteration), then reverse the
      // resulting list.
      val periods0 = levy.periods.foldLeft(List.empty[LevyPeriod]) { (acc, period) =>
        acc match {
          case head :: tail =>
            if (head.end.isEmpty) {
              // end previous period
              period :: head.copy(end = Some(period.start.minusDays(1))) :: tail
            } else if (head.end.get.plusDays(1) < period.start) {
              // Add fake period in-between
              period :: LevyPeriod(rate = 0, start = head.end.get.plusDays(1), end = Some(period.start.minusDays(1))) :: acc
            } else {
              period :: acc
            }
          case _ =>
            if (period.start > startDate) {
              // Add fake first period
              period :: LevyPeriod(rate = 0, start = startDate, end = Some(period.start.minusDays(1))) :: acc
            } else {
              period :: acc
            }
        }
      }
      // Add fake period after last ending
      val periods1 = periods0 match {
        case head :: tail if head.end.isDefined => LevyPeriod(rate = 0, start = head.end.get.plusDays(1), end = None) :: periods0
        case _                                  => periods0
      }
      val periods = periods1.reverse
      levy.copy(periods = periods)
    }.view.force
    copy(levies = normalizedLevies)
  }

  override protected def debugString: String =
    s"Levies($name,$date,${levies.mkString(",")})"

}

object Levies {

  val empty = Levies(name = null, date = LocalDate.now, levies = Map.empty)

  object JsonProtocol extends DefaultJsonProtocol with JsonFormats {

    private def getField(obj: JsObject, field: String): JsValue =
      obj.fields.get(field) match {
        case Some(value) => value
        case None        => deserializationError(s"Missing '$field'")
      }

    implicit val levyPeriodFormat = jsonFormat3(LevyPeriod)

    implicit object LevyJsonFormat extends JsonFormat[Levy] {

      val FIELD_PERIODS = "periods"
      val FIELD_LOSS_ATTRIBUTION_RULE = "lossAttributionRule"

      override def read(json: JsValue): Levy = {
        val obj = json.asJsObject
        // Sort periods by start date
        val periods = getField(obj, FIELD_PERIODS).asInstanceOf[JsArray].elements.toList.map { v =>
          v.convertTo[LevyPeriod]
        }.sortBy(_.start)
        // Make sure periods start/end don't overlap
        periods.zip(periods.drop(1)).foreach {
          case (previous, next) =>
            if ((next.start <= previous.start) || previous.end.exists(_ >= next.start))
              deserializationError(s"Levy periods cannot overlap: ${obj.compactPrint}")
        }
        Levy(name = null, periods = periods)
      }

      override def write(obj: Levy): JsValue =
        serializationError("Not implemented")

    }

    implicit object LeviesJsonFormat extends RootJsonFormat[Levies] {

      val FIELD_NAME = "name"
      val FIELD_DATE = "date"

      val knownFields = Set(FIELD_NAME, FIELD_DATE)

      override def read(json: JsValue): Levies = {
        val obj = json.asJsObject
        val name = getField(obj, FIELD_NAME).asInstanceOf[JsString].value
        val date = getField(obj, FIELD_DATE).convertTo[LocalDate]
        // Get levies, setting names from JsObject keys, and sort by name.
        val levies = obj.fields.keys.filterNot(knownFields.contains).toList.sorted.map { key =>
          key -> obj.fields(key).convertTo[Levy].copy(name = key)
        }.foldLeft(ListMap.empty[String, Levy])(_ + _)
        Levies(
          name = name,
          date = date,
          levies = levies
        )
      }

      override def write(obj: Levies): JsValue =
        serializationError("Not implemented")

    }

  }

}

case class LevyPeriodData(
  period: LevyPeriod,
  investedAmount: BigDecimal,
  grossGain: Option[BigDecimal]
) extends DebugString {
  def isComplete = grossGain.isDefined
  def getGrossGain: BigDecimal = {
    assert(isComplete)
    splitAmount(grossGain.getOrElse(0), 1)
  }
  def getGrossGain(grossAmount: BigDecimal, ratio: BigDecimal): BigDecimal = {
    assert(!isComplete)
    splitAmount(grossGain.getOrElse(grossAmount - investedAmount), ratio)
  }
  def amount: BigDecimal = {
    assert(isComplete)
    val v = scaleAmount(grossGain.getOrElse(BigDecimal(0)) * period.rate / 100)
    if (v >= 0) v
    else 0
  }
  def zero: LevyPeriodData =
    copy(grossGain = Some(0))
  def complete(grossGain: BigDecimal): LevyPeriodData =
    copy(grossGain = Some(grossGain))
  def proportioned(ratio: BigDecimal): (LevyPeriodData, LevyPeriodData) = {
    // We compute the requested ratio, and also return the remaining value.
    val invested = {
      val p1 = splitAmount(investedAmount, ratio)
      val p2 = investedAmount - p1
      (p1, p2)
    }
    val gain = grossGain.map { gain =>
      val p1 = splitAmount(gain, ratio)
      val p2 = gain - p1
      (p1, p2)
    }
    (copy(investedAmount = invested._1, grossGain = gain.map(_._1)),
      copy(investedAmount = invested._2, grossGain = gain.map(_._2)))
  }
  override protected def debugString: String =
    s"PeriodData($period,$investedAmount${grossGain.map(v => s",$v").getOrElse("")})"
}

case class LeviesPeriodsData(
  data: Map[String, List[LevyPeriodData]] = Map.empty,
  warnings: List[String] = Nil
) extends DebugString {
  def proportioned(ratio: BigDecimal): (LeviesPeriodsData, LeviesPeriodsData) =
    data.toList.foldLeft((LeviesPeriodsData(warnings = warnings), LeviesPeriodsData(warnings = warnings))) {
      case ((d1, d2), (levy, periodData)) =>
        val (p1, p2) = periodData.map(_.proportioned(ratio)).unzip
        (d1.copy(data = d1.data + (levy -> p1)), d2.copy(data = d2.data + (levy -> p2)))
    }
  def amount(levy: String): BigDecimal = data.getOrElse(levy, Nil).map(_.amount).sum
  def amount: BigDecimal = data.keys.toList.map(amount(_)).sum

  def addPeriodsData(levy: String, periodsData: List[LevyPeriodData]): LeviesPeriodsData =
    copy(data + (levy -> periodsData))
  def addWarning(warning: String): LeviesPeriodsData =
    if (warnings.contains(warning)) this
    else copy(warnings = warnings :+ warning)
  def addWarnings(warnings: List[String]): LeviesPeriodsData =
    warnings.foldLeft(this)(_.addWarning(_))
  override protected def debugString: String =
    s"PeriodsData(${data.mkString(",")})"
}
