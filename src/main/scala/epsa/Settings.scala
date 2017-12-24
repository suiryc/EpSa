package epsa

import java.math.{BigDecimal => jBigDecimal}
import java.text.{DecimalFormat, NumberFormat, ParsePosition}
import java.util.Locale
import java.util.prefs.Preferences
import scala.concurrent.duration._
import scala.runtime.ScalaRunTime
import suiryc.scala.concurrent.ThreadLocalEx
import suiryc.scala.settings.Preference
import suiryc.scala.settings.Preference._

/** Settings. */
object Settings {

  val prefs: Preferences = Preferences.userRoot.node("suiryc.epsa").node("epsa")

  var debugParams: Set[Debug.Value] = Set.empty
  def debug(v: Debug.Value): Boolean = debugParams.contains(v)
  def toString(product: Product, str: => String): String =
    if (debug(Debug.OriginalToString)) ScalaRunTime._toString(product)
    else str

  // Get decimal format
  // Note: as suggested by the API, wrap inside a ThreadLocal to handle
  // concurrent accesses.
  private val decimalFormat = ThreadLocalEx {
    val df = try {
      NumberFormat.getInstance(Locale.getDefault).asInstanceOf[DecimalFormat]
    } catch {
      case _: Exception => new DecimalFormat()
    }

    // We want to get BigDecimal upon parsing and we will manage rounding.
    // So a maximum of 6 fraction digits should be enough.
    df.setParseBigDecimal(true)
    df.setMaximumFractionDigits(6)
    df
  }


  // Decimal format without grouping (useful to fill editable fields).
  private val decimalCompactFormat = ThreadLocalEx {
    val df = decimalFormat.get.clone().asInstanceOf[DecimalFormat]
    df.setGroupingUsed(false)
    df
  }

  val preferredCurrencies = List("€", "$", "£", "￥", "฿")
  val defaultCurrency: String = preferredCurrencies.head

  val currency: Preference[String] = Preference.from(prefs, "currency", defaultCurrency)

  val amountScale: Preference[Int] = Preference.from(prefs, "amount.scale", 2)
  val amountRounding: Preference[BigDecimal.RoundingMode.Value] =
    Preference.from(prefs, "amount.rounding", BigDecimal.RoundingMode, BigDecimal.RoundingMode.HALF_EVEN)

  val unitsScale: Preference[Int] = Preference.from(prefs, "units.scale", 4)
  val unitsRounding: Preference[BigDecimal.RoundingMode.Value] =
    Preference.from(prefs, "units.rounding", BigDecimal.RoundingMode, BigDecimal.RoundingMode.HALF_EVEN)

  val vwapScale: Preference[Int] = Preference.from(prefs, "vwap.scale", 4)
  val vwapRounding: Preference[BigDecimal.RoundingMode.Value] =
    Preference.from(prefs, "vwap.rounding", BigDecimal.RoundingMode, BigDecimal.RoundingMode.HALF_EVEN)

  val httpClientTimeout: Preference[FiniteDuration] = Preference.from(prefs, "http.client.timeout", 10.seconds)

  // Note: not really useful to let user change percents scale/rounding
  val percentsScale = 2
  val percentsRounding: BigDecimal.RoundingMode.Value = BigDecimal.RoundingMode.HALF_EVEN

  def splitAmount(amount: BigDecimal, ratio: BigDecimal): BigDecimal =
    scaleAmount(amount * ratio)

  def scaleAmount(v: BigDecimal): BigDecimal =
    v.setScale(amountScale(), amountRounding())

  def scaleUnits(v: BigDecimal): BigDecimal =
    v.setScale(unitsScale(), unitsRounding())

  def scaleVWAP(v: BigDecimal): BigDecimal =
    v.setScale(vwapScale(), vwapRounding())

  def scalePercents(v: BigDecimal): BigDecimal = {
    // Adapt scale according to value.
    // e.g. a value below 10 have the requested scale, a value below 100 have
    // its scale down by 1, a value below 1000 down by 2, etc.
    val scale = scala.math.max(0, percentsScale - (v.abs.intValue.toString.length - 1))
    v.setScale(scale, percentsRounding)
  }

  private def formatNumber(df: DecimalFormat, v: BigDecimal): String = {
    // Change minimum fraction digits to match the BigDecimal scale so that
    // all formatted numbers look consistent.
    // Those not explicitly scaled are not affected, those we scaled (e.g.
    // units, amounts or percents) will all have the same number of decimals
    // displayed (even trailing 0s).
    df.setMinimumFractionDigits(v.scale)
    df.format(v)
  }

  def formatCompactNumber(v: BigDecimal): String = formatNumber(decimalCompactFormat.get, v)

  def formatNumber(v: BigDecimal): String = formatNumber(decimalFormat.get, v)

  def formatNumber(v: BigDecimal, suffix: String): String = {
    val f = formatNumber(v)
    Option(suffix).find(!_.isEmpty).map { s =>
      s"$f $s"
    }.getOrElse(f)
  }

  def parseNumber(str: String): BigDecimal = try {
    val pos = new ParsePosition(0)
    Option(str).map { s =>
      // First try to parse using locale format
      val parsed = decimalCompactFormat.get.parse(s, pos)
      // Upon failure, or if the whole string was not parsed, fallback to
      // standard BigDecimal parsing.
      if ((parsed != null) && (pos.getIndex == s.length)) BigDecimal(parsed.asInstanceOf[jBigDecimal])
      else BigDecimal(s)
    }.getOrElse(BigDecimal(0))
  } catch {
    case _: Exception => BigDecimal(0)
  }

  case object Debug extends Enumeration {
    val OriginalToString: Debug.Value = Value("original-tostring")
    val LeviesComputation: Debug.Value = Value("levies-computation")
    val LeviesHistory: Debug.Value = Value("levies-history")
  }

  trait DebugString { product: Product =>
    protected def debugString: String
    // Note: default scala 'toString' variant for 'Product' instances (e.g.
    // case classes) is available through 'ScalaRunTime._toString'
    // See: http://stackoverflow.com/a/27467406
    override def toString: String =
      if (debug(Debug.OriginalToString)) ScalaRunTime._toString(product)
      else debugString
  }

  case class DebugInfo(info: List[String] = Nil, source: List[DebugInfo] = Nil) extends DebugString {
    override protected def debugString: String = {
      @scala.annotation.tailrec
      def loop(acc: String, remaining: List[(Int, DebugInfo)]): String = {
        remaining match {
          case head :: tail =>
            val (level, debug) = head
            val prefix = "  " * level
            val acc2 = if (debug.info.isEmpty) acc
              else {
                (if (acc.isEmpty) "" else s"$acc\n") +
                  debug.info.mkString(s"$prefix-> ", s"\n$prefix + ", "")
              }
            loop(acc2, debug.source.map((level + 1) -> _) ::: tail)

          case Nil =>
            acc
        }
      }
      loop("", List(0 -> this))
    }
  }

}
