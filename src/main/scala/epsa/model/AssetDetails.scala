package epsa.model

import com.sun.javafx.collections.SourceAdapterChange
import epsa.I18N.Strings
import epsa.Settings._
import epsa.controllers.Form
import java.time.LocalDate
import java.util
import java.util.Comparator
import javafx.beans.property.{ObjectProperty, SimpleObjectProperty}
import javafx.collections.ListChangeListener.Change
import javafx.collections.{FXCollections, ObservableList}
import javafx.collections.transformation.{SortedList, TransformationList}
import scala.collection.JavaConverters._
import suiryc.scala.javafx.collections.RichObservableList._
import suiryc.scala.math.Ordering.localDateOrdering

/** The kind of asset details. */
object AssetDetailsKind extends Enumeration {
  /** Standard (i.e. real) asset details. */
  val Standard = Value
  /** Partial total details. */
  val TotalPartial = Value
  /** Partial (per fund) total details. */
  val TotalPerFund = Value
  /** Partial (per availability) total details. */
  val TotalPerAvailability = Value
  /** Total details. */
  val Total = Value
}

/** Asset details. */
trait AssetDetails {
  /** Savings. */
  val savings: Savings
  /** Concerned asset. */
  val asset: Savings.Asset
  /** VWAP to use (if not the asset one). */
  val actualVWAP: Option[BigDecimal] = None
  /** Concerned scheme. */
  val scheme: Savings.Scheme
  /** Concerned fund. */
  val fund: Savings.Fund
  /** NAV date. */
  val date: Option[LocalDate]
  /** NAV. */
  val nav: Option[BigDecimal]
  /** Gross amount warning. */
  val grossAmountWarning: List[String] = Nil

  /** Date on which availability is based on. */
  val availabilityBase: Option[LocalDate]
  /** Kind of details. */
  val kind: AssetDetailsKind.Value
  /** Whether this is the first (used for styling in partial totals sections). */
  var first: Boolean = false

  private val currency = epsa.Settings.currency()

  // The following functions/values give access to detailed values and formats
  def availability: Option[LocalDate] = asset.availability
  def units: BigDecimal = asset.units
  def vwap: BigDecimal = actualVWAP.getOrElse(asset.vwap)
  def investedAmount: BigDecimal = asset.amount(vwap)
  lazy val grossAmount: Option[BigDecimal] = nav.map { value =>
    asset.amount(value)
  }
  lazy val grossGain: Option[BigDecimal] = grossAmount.map { amount =>
    amount - investedAmount
  }
  lazy val grossGainPct: Option[BigDecimal] =
    if (investedAmount == 0) Some(BigDecimal(0))
    else grossGain.map(v => scalePercents((v * 100) / investedAmount))
  // When showing VWAP per asset, don't display levies (and net amount/gain)
  // as those are based on assetId VWAP.
  lazy val leviesPeriodsData: Option[LeviesPeriodsData] =
    if ((kind != AssetDetailsKind.Standard) || actualVWAP.isEmpty) None
    else nav.map { nav =>
      val totalUnits = savings.assets.units(asset.id)
      val leviesPeriodsData = savings.computeLevies(asset.id, availabilityBase.getOrElse(LocalDate.now), nav)
      val (refundLevies, _) = leviesPeriodsData.proportioned(units / totalUnits)
      refundLevies
    }
  def leviesWarning: List[String] = leviesPeriodsData.map(_.warnings).getOrElse(Nil)
  def leviesAmount: Option[BigDecimal] = leviesPeriodsData.map(_.amount)
  lazy val netAmount: Option[BigDecimal] =
    for {
      grossAmount <- grossAmount
      leviesAmount <- leviesAmount
    } yield {
      grossAmount - leviesAmount
    }
  lazy val netGain: Option[BigDecimal] = netAmount.map { amount =>
    amount - investedAmount
  }
  lazy val netGainPct: Option[BigDecimal] =
    if (investedAmount == 0) Some(BigDecimal(0))
    else netGain.map(v => scalePercents((v * 100) / investedAmount))

  lazy val formatAvailability: String =
    if ((kind != AssetDetailsKind.Standard) && (kind != AssetDetailsKind.TotalPerAvailability)) null
    else Form.formatAvailability(asset.availability, date = availabilityBase)
  lazy val formatUnits: String =
    if ((kind != AssetDetailsKind.Standard) && (kind != AssetDetailsKind.TotalPerFund)) null
    else formatNumber(scaleUnits(units))
  lazy val formatVWAP: String =
    if ((kind != AssetDetailsKind.Standard) && (kind != AssetDetailsKind.TotalPerFund)) null
    else formatNumber(vwap, currency)
  lazy val formatDate: String =
    if ((kind != AssetDetailsKind.Standard) && (kind != AssetDetailsKind.TotalPerFund)) null
    else date.map(_.toString).getOrElse(Strings.na)
  lazy val formatNAV: String =
    if ((kind != AssetDetailsKind.Standard) && (kind != AssetDetailsKind.TotalPerFund)) null
    else nav.map(formatNumber(_, currency)).getOrElse(Strings.na)
  lazy val formatInvestedAmount: String = formatNumber(investedAmount, currency)
  lazy val formatGrossAmount: String = grossAmount.map(formatNumber(_, currency)).getOrElse(Strings.na)
  lazy val formatLeviesAmount: String = leviesAmount.map(formatNumber(_, currency)).getOrElse(Strings.na)
  lazy val formatNetAmount: String = netAmount.map(formatNumber(_, currency)).getOrElse(Strings.na)
  lazy val formatGrossGain: String = grossGain.map(formatNumber(_, currency)).getOrElse(Strings.na)
  lazy val formatGrossGainPct: String = grossGainPct.map(formatNumber(_, "%")).getOrElse(Strings.na)
  lazy val formatNetGain: String = netGain.map(formatNumber(_, currency)).getOrElse(Strings.na)
  lazy val formatNetGainPct: String = netGainPct.map(formatNumber(_, "%")).getOrElse(Strings.na)
}

/** Standard details. */
case class StandardAssetDetails(
  savings: Savings,
  asset: Savings.Asset,
  scheme: Savings.Scheme,
  fund: Savings.Fund,
  date: Option[LocalDate],
  nav: Option[BigDecimal],
  override val actualVWAP: Option[BigDecimal],
  availabilityBase: Option[LocalDate]
) extends AssetDetails {
  val kind = AssetDetailsKind.Standard
}

/** Total (maybe partial) details. */
case class TotalAssetDetails(
  savings: Savings,
  asset: Savings.Asset,
  scheme: Savings.Scheme,
  fund: Savings.Fund,
  date: Option[LocalDate],
  nav: Option[BigDecimal],
  availabilityBase: Option[LocalDate],
  kind: AssetDetailsKind.Value,
  override val investedAmount: BigDecimal,
  override val grossAmountWarning: List[String],
  override val leviesWarning: List[String],
  override val leviesAmount: Option[BigDecimal]
) extends AssetDetails

/**
 * Special items list that automatically adds a 'sum' of all wrapped assets.
 *
 * Ensures the dynamically generated rows remain at the end of the table.
 * See: http://stackoverflow.com/a/30509417
 */
class AssetDetailsWithTotal(
  savings: Savings,
  source0: ObservableList[AssetDetails],
  vwapPerAsset: Boolean,
  showTotalsPerScheme: Boolean,
  showTotalsPerFund: Boolean,
  showTotalsPerAvailability: Boolean,
  availabilityBase: Option[LocalDate]
) extends TransformationList[AssetDetails, AssetDetails](source0)
{

  // Notes:
  // We wish to append partial totals (per scheme, per fund, and per
  // availability date) and the grand total at the end of the table.
  // We would also like for partial totals to be sortable (only the entries
  // inside each total group, not between groups).
  // To do so, we create SortedList for each partial total group, and bind its
  // comparator to ours (which is bound to the table one), then we listen for
  // changes in each group and propagate them (with appropriate adaptation to
  // fir the transformed list given as table items).

  private def orZero(v: Option[BigDecimal]): BigDecimal = v.getOrElse(0)

  private def computeTotal(savings: Savings, assets: List[AssetDetails], kind: AssetDetailsKind.Value,
                           scheme: Option[Savings.Scheme], fund: Option[Savings.Fund],
                           availability: Option[LocalDate]): AssetDetails =
  {
    val total0 = TotalAssetDetails(
      savings = savings,
      asset = Savings.Asset(scheme.map(_.id).orNull, fund.map(_.id).orNull, availability, units = 0, vwap = 0),
      scheme = scheme.getOrElse(Savings.Scheme(null, null, None, Nil)),
      fund = fund.getOrElse(Savings.Fund(null, null, None, None)),
      date = None,
      nav = Some(0),
      availabilityBase = availabilityBase,
      kind = kind,
      investedAmount = 0,
      grossAmountWarning = Nil,
      leviesWarning = Nil,
      leviesAmount = None
    )
    assets.foldLeft(total0) { (acc, details) =>
      // For total per fund, units, NAV and VWAP are displayed, so we use the
      // real values. For other totals, cheat with units and NAV so that gross
      // amount and derived values are computed for us.
      val units =
        if (kind != AssetDetailsKind.TotalPerFund) BigDecimal(1)
        else acc.units + details.units
      val nav =
        if (kind != AssetDetailsKind.TotalPerFund) Some(orZero(acc.nav) + orZero(details.grossAmount))
        else details.nav
      val investedAmount = acc.investedAmount + details.investedAmount
      val vwap = scaleVWAP(investedAmount / units)
      val grossAmountWarning =
        if ((kind == AssetDetailsKind.TotalPerFund) || details.grossAmount.nonEmpty) acc.grossAmountWarning
        else {
          // $1=fund $2=date
          val warning = Strings.accountHistoryIssuesNAV.format(details.fund.name, availabilityBase.getOrElse(LocalDate.now))
          if (acc.grossAmountWarning.contains(warning)) acc.grossAmountWarning
          else acc.grossAmountWarning :+ warning
        }
      // When showing VWAP per asset, don't display levies (and net amount/gain)
      // as those are based on assetId VWAP.
      val leviesWarning =
        if (vwapPerAsset) Nil
        else (acc.leviesWarning ::: details.leviesWarning).distinct
      val leviesAmount =
        if (vwapPerAsset) None
        else Some(orZero(acc.leviesAmount) + orZero(details.leviesAmount))
      acc.copy(
        asset = acc.asset.copy(units = units, vwap = vwap),
        date = details.date,
        nav = nav,
        investedAmount = investedAmount,
        grossAmountWarning = grossAmountWarning,
        leviesWarning = leviesWarning,
        leviesAmount = leviesAmount
      )
    }
  }

  private def toSorted(list: List[AssetDetails]): SortedList[AssetDetails] =
    new SortedList[AssetDetails](FXCollections.observableList(list.asJava))

  // This adapts and fires a change triggered from a partial total group.
  // The adapted change properly sources the full items list (not only its own
  // group) and has its indices offsetted to get the correct ones in this list.
  private def adaptChange[A <: AssetDetails](c: Change[A], offset: Int): Unit = {
    // We need to update our list first before firing any change
    updateTotals()
    val list = FXCollections.observableList(totals.asJava).asInstanceOf[ObservableList[A]]
    // Since the original list is not the complete one, wrap the change to
    // offset the returned indices.
    val adapted0 = new Change[A](list) {
      override def next(): Boolean = c.next()
      override def reset(): Unit = c.reset()
      override def getRemoved: util.List[A] = c.getRemoved
      override def getFrom: Int = c.getFrom + offset
      override def getTo: Int = c.getTo + offset
      override def getPermutation: Array[Int] = Array()
      override def getPermutation(i: Int): Int = c.getPermutation(i) + offset
    }
    // Note: we need to implement getPermutation which returns an array of
    // indices (permutations). The original one is not accessible so we would
    // have to create one and fill it; fortunately SourceAdapterChange can do
    // it for us, so use it to wrap ours.
    val adapted = new SourceAdapterChange[A](list, adapted0)
    fireChange(adapted)
  }

  val comparatorProperty: ObjectProperty[Comparator[AssetDetails]] =
    new SimpleObjectProperty[Comparator[AssetDetails]]()

  private val assets0 = source0.asScala.toList

  // The grand total
  private val total = computeTotal(savings, assets0, kind = AssetDetailsKind.Total, scheme = None, fund = None, availability = None)
  // Partial totals, with proper initial sorting and bound comparators.
  // Listen for changes in each group in order to propagate them (so that
  // the table rows are updated).
  private val totalPerScheme =
    if (!showTotalsPerScheme) toSorted(Nil)
    else {
      val totals = toSorted(assets0.groupBy(_.scheme).map { case (scheme, assets) =>
        computeTotal(savings, assets, kind = AssetDetailsKind.TotalPartial, scheme = Some(scheme), fund = None, availability = None)
      }.toList.sortBy(_.scheme.name))
      totals.comparatorProperty.bind(comparatorProperty)
      totals.listen { change =>
        adaptChange(change, getSource.size)
      }
      totals
    }
  private val totalPerFund =
    if (!showTotalsPerFund) toSorted(Nil)
    else {
      val totals = toSorted(assets0.groupBy(v => (v.scheme, v.fund)).map { case ((scheme, fund), assets) =>
        computeTotal(savings, assets, kind = AssetDetailsKind.TotalPerFund, scheme = Some(scheme), fund = Some(fund), availability = None)
      }.toList.sortBy(v => (v.scheme.name, v.fund.name)))
      totals.comparatorProperty.bind(comparatorProperty)
      totals.listen { change =>
        adaptChange(change, getSource.size + totalPerScheme.size)
      }
      totals
    }
  private val totalPerAvailability =
    if (!showTotalsPerAvailability) toSorted(Nil)
    else {
      // Resolve availabilities for given date, otherwise we may have multiple
      // 'available' totals if 'up to date assets' is disabled.
      val totals = toSorted(assets0.groupBy { details =>
        Savings.resolveAvailability(details.asset.availability, availabilityBase)
      }.map { case (availability, assets) =>
        computeTotal(savings, assets, kind = AssetDetailsKind.TotalPerAvailability, scheme = None, fund = None, availability = availability)
      }.toList.sortBy(_.asset.availability))
      totals.comparatorProperty.bind(comparatorProperty)
      totals.listen { change =>
        adaptChange(change, getSource.size + totalPerScheme.size + totalPerFund.size)
      }
      totals
    }

  private var totals: List[AssetDetails] = Nil

  private def updateTotals(): Unit = {
    def tagFirst(assets: List[AssetDetails]): List[AssetDetails] = {
      @scala.annotation.tailrec
      def loop(assets: List[AssetDetails], first: Boolean): Unit =
        assets match {
          case head :: tail =>
            head.first = first
            loop(tail, first = false)

          case Nil =>
        }

      loop(assets, first = true)
      assets
    }

    // Rebuild all totals, tagging the first row of each group
    totals = (tagFirst(totalPerScheme.asScala.toList)
      ::: tagFirst(totalPerFund.asScala.toList)
      ::: tagFirst(totalPerAvailability.asScala.toList)
      ) :+ total
  }

  updateTotals()

  override def sourceChanged(c: Change[_ <: AssetDetails]): Unit = {
    fireChange(c)
  }

  override def getSourceIndex(index: Int): Int =
    if (index < getSource.size) index else -1

  override def get(index: Int): AssetDetails =
    if (index < getSource.size) getSource.get(index)
    else if (index < getSource.size + totals.size) totals(index - getSource.size)
    else throw new ArrayIndexOutOfBoundsException(index)

  override def size(): Int = getSource.size + totals.size

}
