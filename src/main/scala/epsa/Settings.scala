package epsa

import java.util.prefs.Preferences
import scala.runtime.ScalaRunTime
import suiryc.scala.settings.Preference
import suiryc.scala.settings.Preference._

/** Settings. */
object Settings {

  implicit val prefs = Preferences.userRoot.node("suiryc.epsa").node("epsa")

  var debugParams: Set[Debug.Value] = Set.empty
  def debug(v: Debug.Value) = debugParams.contains(v)
  def toString(product: Product, str: => String): String =
    if (debug(Debug.OriginalToString)) ScalaRunTime._toString(product)
    else str

  val preferredCurrencies = List("€", "$", "£", "￥", "฿")
  val defaultCurrency = preferredCurrencies.head

  val currency = Preference.from("currency", defaultCurrency)

  implicit private val roundingMode = BigDecimal.RoundingMode

  val amountScale = Preference.from("amount.scale", 2)
  val amountRounding = Preference.from("amount.rounding", BigDecimal.RoundingMode.HALF_EVEN)

  val unitsScale = Preference.from("units.scale", 4)
  val unitsRounding = Preference.from("units.rounding", BigDecimal.RoundingMode.HALF_EVEN)

  val vwapScale = Preference.from("vwap.scale", 4)
  val vwapRounding = Preference.from("vwap.rounding", BigDecimal.RoundingMode.HALF_EVEN)

  // Note: not really useful to let user change percents scale/rounding
  val percentsScale = 2
  val percentsRounding = BigDecimal.RoundingMode.HALF_EVEN

  def getBigDecimal(str: String): BigDecimal = try {
    Option(str).map(BigDecimal(_)).getOrElse(BigDecimal(0))
  } catch {
    case ex: Exception => BigDecimal(0)
  }

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

  case object Debug extends Enumeration {
    val OriginalToString = Value("original-tostring")
    val LeviesComputation = Value("levies-computation")
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

}
