package epsa.model

import epsa.Settings.scaleVWAP
import epsa.charts.ChartSeriesData
import epsa.util.Awaits
import grizzled.slf4j.Logging
import java.time.{LocalDate, Month}
import java.util.UUID
import javafx.stage.Window
import spray.json._
import suiryc.scala.math.Ordered._

/** Savings helpers. */
object Savings {

  import epsa.Settings._

  /**
   * Resolves availability date against a target date.
   *
   * Availability date remains if given date predates it. Otherwise availability
   * is considered immediate.
   *
   * @param availability availability date to resolve
   * @param date0 target date to resolve availability against; use None for current date
   * @return resolved availability, which is either given one, or None (for immediate availability)
   */
  def resolveAvailability(availability: Option[LocalDate], date0: Option[LocalDate]): Option[LocalDate] = {
    val date = date0.getOrElse(LocalDate.now)
    availability.filter(_ > date)
  }

  /**
   * Sorts events.
   *
   * Regroups asset events by date when necessary.
   *
   * @param events events to sort
   * @return sorted events
   */
  def sortEvents(events: Seq[Event]): Seq[Event] = {
    case class DateRange(min: LocalDate, max: LocalDate)
    case class SortingEvents(prefix: Seq[Event], assetEvents: Seq[AssetEvent]) {
      lazy val dateRange: Option[DateRange] = {
        val eventsDates = assetEvents.map(_.date)
        if (eventsDates.isEmpty) None
        else Some(DateRange(eventsDates.min, eventsDates.max))
      }
    }

    // Splits events into consecutive groups of pure Events and AssetEvents
    @scala.annotation.tailrec
    def split(events: Seq[Event], sortings: Seq[SortingEvents]): Seq[SortingEvents] = {
      if (events.isEmpty) sortings
      else {
        val (prefix, suffix0) = events.span(!_.isInstanceOf[AssetEvent])
        val (assetEvents0, suffix) = suffix0.span(_.isInstanceOf[AssetEvent])
        val assetEvents = assetEvents0.asInstanceOf[Seq[AssetEvent]]
        split(suffix, sortings :+ SortingEvents(prefix, assetEvents))
      }
    }

    // Rejoins groups of consecutive Events and AssetEvents
    def join(sortings: Seq[SortingEvents]): Seq[Event] =
      sortings.flatMap { v =>
        v.prefix ++ v.assetEvents
      }

    // Move AssetEvents from one group to another if necessary, that is if the
    // source group contains dates predating maximal date of destination group.
    def extract(to: SortingEvents, from: SortingEvents): (SortingEvents, SortingEvents) =
      to.dateRange match {
        case Some(dateRange) if from.dateRange.exists(_.min < dateRange.max) =>
          val (extracted, remaining) = from.assetEvents.partition(_.date < dateRange.max)
          (to.copy(assetEvents = to.assetEvents ++ extracted), from.copy(assetEvents = remaining))

        case _ =>
          (to, from)
      }

    // Sorts events: loop over each group, and integrate AssetEvents of
    // remaining groups if necessary.
    // As the final step in each group, sort AssetEvents by date.
    @scala.annotation.tailrec
    def sort(sortings: Seq[SortingEvents], sorted: Seq[SortingEvents]): Seq[SortingEvents] =
      sortings.headOption match {
        case Some(sorting) =>
          val (sorting2, sortings2) = sortings.tail.foldLeft(sorting, Seq.empty[SortingEvents]) {
            case ((accSorting, accSortings), from) =>
              val (accSorting2, from2) = extract(accSorting, from)
              (accSorting2, accSortings :+ from2)
          }
          val sorting3 = sorting2.copy(assetEvents = sorting2.assetEvents.sortBy(_.date))
          sort(sortings2, sorted :+ sorting3)

        case None =>
          sorted
      }

    val sortings = split(events, Nil)
    val sorted = sort(sortings, Nil)
    join(sorted)
  }

  case class Scheme(id: UUID, name: String, comment: Option[String], funds: List[UUID]) {
    // See: http://stackoverflow.com/a/19348339
    import scala.math.Ordered.orderingToOrdered
    def compareParams(other: Scheme): Int =
      (name, comment) compare (other.name, other.comment)
  }

  case class Fund(id: UUID, name: String, comment: Option[String]) {
    // See: http://stackoverflow.com/a/19348339
    import scala.math.Ordered.orderingToOrdered
    def compareParams(other: Fund): Int =
      (name, comment) compare (other.name, other.comment)
  }

  case class AssetId(schemeId: UUID, fundId: UUID)

  case class Assets(list: List[Asset] = Nil, vwaps: Map[AssetId, BigDecimal] = Map.empty) {
    lazy val byId = list.groupBy(_.id)
    def units(id: AssetId) = byId.getOrElse(id, Nil).map(_.units).sum
    def investedAmount(id: AssetId) = scaleAmount(units(id) * vwaps.getOrElse(id, 0))
    def addAsset(asset: Asset) = copy(list = list :+ asset)
    def updateAsset(date: LocalDate, asset: Asset) =
      copy(list = list.map { currentAsset =>
        if (currentAsset.matches(date, asset)) asset
        else currentAsset
      })
    def removeAsset(date: LocalDate, asset: AssetEntry) = {
      // Filter out asset
      val assets0 = copy(list = list.filterNot(_.matches(date, asset)))
      // Then remove VWAP if no more asset
      if (assets0.byId.contains(asset.id)) assets0
      else assets0.copy(vwaps = vwaps - asset.id)
    }
  }

  trait AssetEntry {
    val schemeId: UUID
    val fundId: UUID
    val availability: Option[LocalDate]
    // Note: we could use a lazy val to cache the value, but it prevents using
    // jsonFormat on subclasses without having to explicitly list field names
    // to take ino account
    def id = AssetId(schemeId, fundId)
    def matches(date: LocalDate, asset: AssetEntry): Boolean = {
      lazy val checkDate =
        if (asset.availability.nonEmpty) availability == asset.availability
        else resolveAvailability(availability, Some(date)).isEmpty
      (id == asset.id) &&
        checkDate
    }
  }

  case class Asset(schemeId: UUID, fundId: UUID, availability: Option[LocalDate], units: BigDecimal, vwap: BigDecimal)
    extends AssetEntry
  {
    def amount(value: BigDecimal): BigDecimal = scaleAmount(units * value)
    lazy val investedAmount: BigDecimal = scaleAmount(units * vwap)
  }

  /** Asset value: holds asset value at a given date. */
  case class AssetValue(date: LocalDate, value: BigDecimal)
    extends ChartSeriesData

  case class AssetValueHistory(name: Option[String], values: List[Savings.AssetValue] = Nil)

  case class AssetPart(schemeId: UUID, fundId: UUID, availability: Option[LocalDate], units: BigDecimal, value: BigDecimal)
    extends AssetEntry
  {
    def amount(value: BigDecimal) = scaleAmount(units * value)
  }

  case class UnavailabilityPeriod(id: String, years: Int, month: Option[Month])

  sealed trait Event

  sealed trait AssetEvent extends Event {
    val date: LocalDate
    val comment: Option[String]
  }

  case class CreateScheme(schemeId: UUID, name: String, comment: Option[String])
    extends Event

  case class UpdateScheme(schemeId: UUID, name: String, comment: Option[String])
    extends Event

  case class DeleteScheme(schemeId: UUID)
    extends Event

  case class CreateFund(fundId: UUID, name: String, comment: Option[String])
    extends Event

  case class UpdateFund(fundId: UUID, name: String, comment: Option[String])
    extends Event

  case class DeleteFund(fundId: UUID)
    extends Event

  case class AssociateFund(schemeId: UUID, fundId: UUID)
    extends Event

  case class DissociateFund(schemeId: UUID, fundId: UUID)
    extends Event

  case class MakePayment(date: LocalDate, part: AssetPart, comment: Option[String])
    extends AssetEvent

  case class MakeTransfer(date: LocalDate, partSrc: AssetPart, partDst: AssetPart, comment: Option[String])
    extends AssetEvent

  case class MakeRefund(date: LocalDate, part: AssetPart, comment: Option[String])
    extends AssetEvent

  object JsonProtocol extends DefaultJsonProtocol {

    // UUID format is not present in spray-json
    // See: https://github.com/spray/spray-json/issues/24
    implicit object UUIDFormat extends JsonFormat[UUID] {

      def write(uuid: UUID): JsValue =
        JsString(uuid.toString)

      def read(value: JsValue): UUID = value match {
        case JsString(uuid) =>
          try {
            UUID.fromString(uuid)
          } catch {
            case ex: Throwable => deserializationError(s"Invalid UUID format: $uuid", ex)
          }

        case _ => deserializationError(s"Expected UUID as JsString. Got $value")
      }
    }

    implicit object LocalDateFormat extends JsonFormat[LocalDate] {

      def write(date: LocalDate): JsValue =
        JsString(date.toString)

      def read(value: JsValue): LocalDate = value match {
        case JsString(dateS) =>
          try {
            LocalDate.parse(dateS)
          } catch {
            case ex: Throwable => deserializationError(s"Invalid LocalDate format: $dateS", ex)
          }

        case _ => deserializationError(s"Expected LocalDate as JsString. Got $value")
      }
    }

    implicit val createSchemeFormat = jsonFormat3(CreateScheme)
    implicit val updateSchemeFormat = jsonFormat3(UpdateScheme)
    implicit val deleteSchemeFormat = jsonFormat1(DeleteScheme)
    implicit val createFundFormat = jsonFormat3(CreateFund)
    implicit val updateFundFormat = jsonFormat3(UpdateFund)
    implicit val deleteFundFormat = jsonFormat1(DeleteFund)
    implicit val associateFundFormat = jsonFormat2(AssociateFund)
    implicit val dissociateFundFormat = jsonFormat2(DissociateFund)
    implicit val assetPartFormat = jsonFormat5(AssetPart)
    implicit val makePaymentFormat = jsonFormat3(MakePayment)
    implicit val makeTransferFormat = jsonFormat4(MakeTransfer)
    implicit val makeRefundFormat = jsonFormat3(MakeRefund)

    implicit object EventJsonFormat extends RootJsonFormat[Event] with BasicFormats {

      val FIELD_KIND = "kind"
      val FIELD_CONTENT = "content"

      def write(event: Event): JsObject = event match {
        case event: CreateScheme   => JsObject(FIELD_CONTENT -> event.toJson, FIELD_KIND -> JsString("CreateScheme"))
        case event: UpdateScheme   => JsObject(FIELD_CONTENT -> event.toJson, FIELD_KIND -> JsString("UpdateScheme"))
        case event: DeleteScheme   => JsObject(FIELD_CONTENT -> event.toJson, FIELD_KIND -> JsString("DeleteScheme"))
        case event: CreateFund     => JsObject(FIELD_CONTENT -> event.toJson, FIELD_KIND -> JsString("CreateFund"))
        case event: UpdateFund     => JsObject(FIELD_CONTENT -> event.toJson, FIELD_KIND -> JsString("UpdateFund"))
        case event: DeleteFund     => JsObject(FIELD_CONTENT -> event.toJson, FIELD_KIND -> JsString("DeleteFund"))
        case event: AssociateFund  => JsObject(FIELD_CONTENT -> event.toJson, FIELD_KIND -> JsString("AssociateFund"))
        case event: DissociateFund => JsObject(FIELD_CONTENT -> event.toJson, FIELD_KIND -> JsString("DissociateFund"))
        case event: MakePayment    => JsObject(FIELD_CONTENT -> event.toJson, FIELD_KIND -> JsString("MakePayment"))
        case event: MakeTransfer   => JsObject(FIELD_CONTENT -> event.toJson, FIELD_KIND -> JsString("MakeTransfer"))
        case event: MakeRefund     => JsObject(FIELD_CONTENT -> event.toJson, FIELD_KIND -> JsString("MakeRefund"))

        case _ => serializationError(s"Unknown event kind: ${event.getClass.getName}")
      }

      def read(value: JsValue): Event = value.asJsObject.getFields(FIELD_CONTENT, FIELD_KIND) match {
        case Seq(content: JsObject, JsString(kind)) => kind match {
          case "CreateScheme"   => content.convertTo[CreateScheme]
          case "UpdateScheme"   => content.convertTo[UpdateScheme]
          case "DeleteScheme"   => content.convertTo[DeleteScheme]
          case "CreateFund"     => content.convertTo[CreateFund]
          case "UpdateFund"     => content.convertTo[UpdateFund]
          case "DeleteFund"     => content.convertTo[DeleteFund]
          case "AssociateFund"  => content.convertTo[AssociateFund]
          case "DissociateFund" => content.convertTo[DissociateFund]
          case "MakePayment"    => content.convertTo[MakePayment]
          case "MakeTransfer"   => content.convertTo[MakeTransfer]
          case "MakeRefund"     => content.convertTo[MakeRefund]

          case _ => deserializationError(s"Unknown (JSON) event kind: $kind")
        }

        case fields => deserializationError(s"Event (JSON format) expected. Got $fields from $value")
      }

    }

  }

}

case class Savings(
  schemes: List[Savings.Scheme] = Nil,
  funds: List[Savings.Fund] = Nil,
  assets: Savings.Assets = Savings.Assets(),
  latestAssetAction: Option[LocalDate] = None
) extends Logging {

  import Savings._

  // Notes on VWAP:
  // VWAP = invested amount / units
  //  => invested amount = units * VWAP
  //
  // U: number of units
  // V: value of a unit
  // n: iteration
  // Xn: value of X for asset operation n
  // X(n): asset value of X after operation n
  //
  // U(n) = U(n-1) + Un
  // VWAP(n) = (U(n-1) * VWAP(n-1) + Un * Vn) / U(n)
  //
  // Refunds don't change VWAP: Un * (Vn - VWAP(n-1)) = gain/loss compared to invested amount
  //
  // Upon transferring, (invested) amount for operation is the same on source and destination:
  // invested (operation) = srcU * srcVWAP
  // amount (operation) = dstUn * dstVn = srcU * srcV
  //  => dstUn = (srcU * srcV) / dstVn
  // dstVWAP(n) = (dstU(n-1) * dstVWAP(n-1) + srcU * srcVWAP) / dstU(n)

  def processActions(actions: (Savings => Savings.Event)*): Savings =
    actions.foldLeft(this) { (savings, action) =>
      val event = action(savings)
      savings.processEvent(event)
    }

  def processEvents(events: Seq[Savings.Event]): Savings =
    events.foldLeft(this) { (savings, event) =>
      savings.processEvent(event)
    }

  def processEvents(events: Savings.Event*)(implicit d: DummyImplicit): Savings =
    processEvents(events)

  def processEvent(event: Event): Savings = event match {
    case CreateScheme(id, name, comment)               => createScheme(id, name, comment)
    case UpdateScheme(id, name, comment)               => updateScheme(id, name, comment)
    case DeleteScheme(id)                              => deleteScheme(id)
    case CreateFund(id, name, comment)                 => createFund(id, name, comment)
    case UpdateFund(id, name, comment)                 => updateFund(id, name, comment)
    case DeleteFund(id)                                => deleteFund(id)
    case AssociateFund(schemeId, fundId)               => associateFund(schemeId, fundId)
    case DissociateFund(schemeId, fundId)              => dissociateFund(schemeId, fundId)
    case MakePayment(date, part, comment)              => computeAssets(date).makePayment(date, part, comment)
    case MakeTransfer(date, partSrc, partDst, comment) => computeAssets(date).makeTransfer(date, partSrc, partDst, comment)
    case MakeRefund(date, part, comment)               => computeAssets(date).makeRefund(date, part, comment)
  }

  protected def createScheme(id: UUID, name: String, comment: Option[String]): Savings = {
    copy(schemes = schemes :+ Scheme(id, name, comment, Nil))
  }

  protected def updateScheme(id: UUID, name: String, comment: Option[String]): Savings = {
    val updated = schemes.map { scheme =>
      if (scheme.id != id) scheme
      else scheme.copy(name = name, comment = comment)
    }
    copy(schemes = updated)
  }

  protected def deleteScheme(id: UUID): Savings = {
    copy(schemes = schemes.filterNot(_.id == id))
  }

  protected def createFund(id: UUID, name: String, comment: Option[String]): Savings = {
    copy(funds = funds :+ Fund(id, name, comment))
  }

  protected def updateFund(id: UUID, name: String, comment: Option[String]): Savings = {
    val updated = funds.map { fund =>
      if (fund.id != id) fund
      else fund.copy(name = name, comment = comment)
    }
    copy(funds = updated)
  }

  protected def deleteFund(id: UUID): Savings = {
    copy(funds = funds.filterNot(_.id == id))
  }

  protected def associateFund(schemeId: UUID, fundId: UUID): Savings = {
    val updated = schemes.map { scheme =>
      if (scheme.id != schemeId) scheme
      else scheme.copy(funds = scheme.funds :+ fundId)
    }
    copy(schemes = updated)
  }

  protected def dissociateFund(schemeId: UUID, fundId: UUID): Savings = {
    val updated = schemes.map { scheme =>
      if (scheme.id != schemeId) scheme
      else scheme.copy(funds = scheme.funds.filterNot(_ == fundId))
    }
    copy(schemes = updated)
  }

  protected def makePayment(date: LocalDate, part: AssetPart, comment: Option[String],
    srcInvestedAmountFine: Option[BigDecimal] = None, srcInvestedAmount: Option[BigDecimal] = None): Savings =
  {
    // Note: VWAP = invested amount / units
    val savings0 = findAsset(date, part) match {
      case Some(currentAsset) =>
        val units = currentAsset.units + part.units
        val vwap = scaleVWAP((currentAsset.investedAmount + srcInvestedAmountFine.getOrElse(part.amount(part.value))) / units)
        // Note: keeping the given (and not existing) asset availability date
        // has the nice side effect of resetting it if the existing asset is
        // actually available for the given date.
        val asset = Asset(part.schemeId, part.fundId, part.availability, units, vwap)
        updateAsset(date, asset)

      case None =>
        val vwap = scaleVWAP(srcInvestedAmountFine.map(_ / part.units).getOrElse(part.value))
        val asset = Asset(part.schemeId, part.fundId, part.availability, part.units, vwap)
        copy(assets = assets.addAsset(asset))
    }

    // Notes:
    // Current Savings is used to get the previous invested amount
    // savings0 already has the total number of units after operation
    val assets0 = savings0.assets
    val units = assets0.units(part.id)
    val vwap = scaleVWAP((assets.investedAmount(part.id) + srcInvestedAmount.getOrElse(part.amount(part.value))) / units)
    val savings = savings0.copy(assets = assets0.copy(vwaps = assets0.vwaps + (part.id -> vwap)))

    savings.copy(latestAssetAction = Some(date))
  }

  protected def makeTransfer(date: LocalDate, partSrc: AssetPart, partDst: AssetPart, comment: Option[String]): Savings = {
    val srcAsset = findAsset(date, partSrc).get
    // Note: invested amount = units * VWAP
    val srcInvestedAmountFine = partSrc.amount(srcAsset.vwap)
    val srcInvestedAmount = partSrc.amount(assets.vwaps(partSrc.id))
    makeRefund(date, partSrc, comment, Some(srcAsset)).makePayment(date, partDst, comment, Some(srcInvestedAmountFine), Some(srcInvestedAmount))
  }

  protected def makeRefund(date: LocalDate, part: AssetPart, comment: Option[String], srcAsset: Option[Savings.Asset] = None): Savings = {
    val currentAsset = srcAsset.orElse(findAsset(date, part)).get
    val units = currentAsset.units - part.units
    val vwap = currentAsset.vwap

    // Note: keep existing asset availability date if any, instead of using
    // given one (which may be empty if asset is actually available for the
    // given date).
    val savings =
      if (units <= 0) removeAsset(date, part)
      else updateAsset(date, Asset(part.schemeId, part.fundId, currentAsset.availability, units, vwap))
    savings.copy(latestAssetAction = Some(date))
  }

  def findAsset(date: LocalDate, asset: AssetEntry): Option[Savings.Asset] =
    assets.byId.getOrElse(asset.id, Nil).find(_.matches(date, asset))

  protected def updateAsset(date: LocalDate, asset: Asset): Savings =
    copy(assets = assets.updateAsset(date, asset))

  protected def removeAsset(date: LocalDate, asset: AssetEntry): Savings =
    copy(assets = assets.removeAsset(date, asset))

  def findScheme(schemeId: UUID): Option[Scheme] =
    schemes.find(_.id == schemeId)

  def getScheme(schemeId: UUID): Scheme =
    findScheme(schemeId).get

  def findFund(fundId: UUID): Option[Fund] =
    funds.find(_.id == fundId)

  def getFund(fundId: UUID): Fund =
    findFund(fundId).get

  def hasAsset(schemeId: UUID, fundId: UUID): Boolean =
    assets.byId.contains(AssetId(schemeId, fundId))

  def createSchemeEvent(name: String, comment: Option[String] = None): CreateScheme = {
    val id = newId(schemes.map(_.id))
    CreateScheme(id, name, comment)
  }

  def createFundEvent(name: String, comment: Option[String] = None): CreateFund = {
    val id = newId(funds.map(_.id))
    CreateFund(id, name, comment)
  }

  protected def newId(existing: List[UUID]): UUID = {
    @scala.annotation.tailrec
    def loop(): UUID = {
      val id = UUID.randomUUID()
      if (!existing.contains(id)) id
      else loop()
    }

    loop()
  }

  /**
   * Flattens events.
   *
   * Applies events and determines actual diff with computed savings.
   * Effectively filters unnecessary events, e.g. when creating then deleting a
   * scheme or fund.
   *
   * WARNING: Does not take care of assets modifications.
   */
  def flattenEvents(events: List[Savings.Event]): List[Savings.Event] = {
    val oldSchemes = schemes.map(_.id).toSet
    val oldFunds = funds.map(_.id).toSet
    val newSavings = processEvents(events)
    val newSchemes = newSavings.schemes.map(_.id).toSet
    val newFunds = newSavings.funds.map(_.id).toSet

    def orderSchemes(uuids: Set[UUID]): List[Savings.Scheme] =
      newSavings.schemes.filter { scheme =>
        uuids.contains(scheme.id)
      }

    def orderFunds(uuids: Set[UUID]): List[Savings.Fund] =
      newSavings.funds.filter { fund =>
        uuids.contains(fund.id)
      }

    val schemesCreated = newSchemes -- oldSchemes
    val schemesCreatedOrdered = orderSchemes(schemesCreated)
    val schemesDeleted = oldSchemes -- newSchemes
    val schemesRemaining = newSchemes.intersect(oldSchemes)
    val schemesRemainingOrdered = orderSchemes(schemesRemaining)
    val fundsCreated = newFunds -- oldFunds
    val fundsCreatedOrdered = orderFunds(fundsCreated)
    val fundsDeleted = oldFunds -- newFunds
    val fundsRemaining = newFunds.intersect(oldFunds)
    val fundsRemainingOrdered = orderFunds(fundsRemaining)

    // In order, we have:
    // 1. deleted schemes (with funds dissociation)
    // 2. deleted funds (with remaining dissociations)
    // 3. created schemes
    // 4. created funds
    // 5. funds associated to created schemes
    // 6. created funds remaining associations
    // 7. remaining schemes updates
    // 8. remaining funds updates
    val flattenedEvents = schemesDeleted.toList.flatMap { schemeId =>
      getScheme(schemeId).funds.map { fundId =>
        Savings.DissociateFund(schemeId, fundId)
      } :+ Savings.DeleteScheme(schemeId)
    } ::: fundsDeleted.toList.flatMap { fundId =>
      schemes.filter { scheme =>
        scheme.funds.contains(fundId) && !schemesDeleted.contains(scheme.id)
      }.map { scheme =>
        Savings.DissociateFund(scheme.id, fundId)
      } :+ Savings.DeleteFund(fundId)
    } ::: schemesCreatedOrdered.map { scheme =>
      Savings.CreateScheme(scheme.id, scheme.name, scheme.comment)
    } ::: fundsCreatedOrdered.map { fund =>
      Savings.CreateFund(fund.id, fund.name, fund.comment)
    } ::: schemesCreatedOrdered.flatMap { scheme =>
      scheme.funds.map { fundId =>
        Savings.AssociateFund(scheme.id, fundId)
      }
    } ::: fundsCreatedOrdered.flatMap { fund =>
      newSavings.schemes.filter { scheme =>
        scheme.funds.contains(fund.id) && !schemesCreated.contains(scheme.id)
      }.map { scheme =>
        Savings.AssociateFund(scheme.id, fund.id)
      }
    } ::: schemesRemainingOrdered.flatMap { newScheme =>
      val oldScheme = getScheme(newScheme.id)
      val event1 =
        if (newScheme.compareParams(oldScheme) == 0) None
        else Some(Savings.UpdateScheme(newScheme.id, newScheme.name, newScheme.comment))

      // Note: don't forget to ignore created/deleted funds (already taken care of)
      val oldFunds = oldScheme.funds.toSet
      val newFunds = newScheme.funds.toSet
      val fundsAssociated = (newFunds -- oldFunds) -- fundsCreated
      val fundsAssociatedOrdered = newScheme.funds.filter(fundsAssociated.contains)
      val fundsDissociated = (oldFunds -- newFunds) -- fundsDeleted

      event1.toList ++ fundsDissociated.toList.map { fundId =>
        Savings.DissociateFund(oldScheme.id, fundId)
      } ++ fundsAssociatedOrdered.map { fundId =>
        Savings.AssociateFund(oldScheme.id, fundId)
      }
    } ::: fundsRemainingOrdered.flatMap { newFund =>
      val oldFund = getFund(newFund.id)
      if (newFund.compareParams(oldFund) == 0) None
      else Some(Savings.UpdateFund(newFund.id, newFund.name, newFund.comment))
      // Note: association/dissociation to old, new or remaining schemes have
      // been taken care of already.
    }

    // Ensure flattening is OK
    val flattenedSavings = processEvents(flattenedEvents)
    if (flattenedSavings != newSavings) {
      warn(s"Events flattening failed: savings[$this] events[$events] flattened[$flattenedEvents]")
      events
    }
    else flattenedEvents
  }

  /**
   * Computes assets at given date.
   *
   * Merges assets for the same scheme, fund, and availability (compared to
   * given date).
   */
  def computeAssets(date: LocalDate): Savings = {
    case class AssetKey(schemeId: UUID, fundId: UUID, availability: Option[LocalDate])
    case class AssetComputation(computed: List[Savings.Asset] = Nil, keys: Set[AssetKey] = Set.empty)

    def getKey(asset: Savings.Asset): AssetKey = {
      val availability = resolveAvailability(asset.availability, Some(date))
      AssetKey(asset.schemeId, asset.fundId, availability)
    }

    def process(computation: AssetComputation, asset: Savings.Asset): AssetComputation = {
      val key = getKey(asset)
      val (previous, filtered) = computation.computed.partition(getKey(_) == key)
      val asset0 = Savings.Asset(
        schemeId = key.schemeId,
        fundId = key.fundId,
        availability = key.availability,
        units = BigDecimal(0),
        vwap = BigDecimal(0)
      )
      val computedAsset = (previous :+ asset).foldLeft(asset0) { (computed, asset) =>
        val units = computed.units + asset.units
        val vwap = scaleVWAP((computed.investedAmount + asset.investedAmount) / units)
        computed.copy(units = units, vwap = vwap)
      }
      computation.copy(computed = filtered :+ computedAsset)
    }

    val computedAssets = assets.list.foldLeft(AssetComputation())(process).computed
    copy(assets = assets.copy(list = computedAssets))
  }

  /**
   * Gets NAVs for given date.
   *
   * @param owner parent window if any (to display error dialog if necessary)
   * @param date date to get NAVs on
   * @param exactDate whether we want NAVs for the exact date, or if nearest predating date is OK too
   * @return NAVs
   */
  def getNAVs(owner: Option[Window], date: LocalDate, exactDate: Boolean = false): Map[UUID, Savings.AssetValue] =
    assets.list.map(_.fundId).distinct.flatMap { fundId =>
      val nav = Awaits.readDataStoreNAV(owner, fundId, date, exactDate).getOrElse(None)
      nav.map(fundId -> _)
    }.toMap

}
