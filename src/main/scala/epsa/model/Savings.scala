package epsa.model

import com.typesafe.scalalogging.StrictLogging
import epsa.Main
import epsa.I18N.Strings
import epsa.Settings
import epsa.Settings.{Debug, DebugInfo}
import epsa.charts.ChartSeriesData
import epsa.util.Awaits
import java.time.{LocalDate, Month}
import java.util.UUID
import javafx.stage.Window
import scala.collection.mutable
import spray.json._
import suiryc.scala.math.Ordered._
import suiryc.scala.spray.json.JsonFormats

/** Savings helpers. */
object Savings extends StrictLogging {

  import Settings._
  import Main.settings.scaleAmount

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
   * Normalize events.
   *
   * Regroups asset events by date when necessary, and flatten non-asset actions
   * when possible.
   *
   * @param events events to normalize
   * @return normalized events
   */
  def normalizeEvents(events: List[Event]): (List[Event], Boolean) = {
    // Notes:
    // The idea is simple:
    // 1. Events are split in groups of consecutive pure events + asset actions
    // 2. Events are moved when applicable (based on actions date, taking care
    // of associated scheme/fund creation if necessary)
    // 3. Groups are joined back, and for each group
    // 3.1. Pure events are flattened
    // 3.2. Asset actions are sorted by date
    //
    // We thus take care of the most important needs: re-group actions per date,
    // and move associated scheme/fund creation/association when applicable.
    // But we don't care about lone scheme/fund updating, nor do we try to
    // insert creations/associations at the right 'date'.
    // e.g. if there is a group of actions spanning over a year, followed by a
    // fund creation&association and an action for that fund which predates the
    // previous group of actions date range end, we move the action into that
    // group and the fund creation&association too: it thus appears the fund
    // was created *before* all those actions in the events history (that is
    // one year before it may actually have). The right thing would be to split
    // the group of actions in 2 based on the moved action date, but it's not
    // really worth the hassle (the code is already complex enough as it is).

    case class DateRange(min: LocalDate, max: LocalDate)
    case class SortingEvents(prefix: List[Event], assetEvents: List[AssetEvent]) {
      def isEmpty: Boolean = prefix.isEmpty && assetEvents.isEmpty
      lazy val dateRange: Option[DateRange] = {
        val eventsDates = assetEvents.map(_.date)
        if (eventsDates.isEmpty) None
        else Some(DateRange(eventsDates.min, eventsDates.max))
      }
      def addEvents(events: List[Event]): SortingEvents = copy(prefix = prefix ::: events)
      def addAssetEvents(events: List[AssetEvent]): SortingEvents = copy(assetEvents = assetEvents ::: events)
    }

    // Splits events into consecutive groups of pure Events and AssetEvents
    @scala.annotation.tailrec
    def split(events: List[Event], sortings: List[SortingEvents]): List[SortingEvents] = {
      if (events.isEmpty) sortings
      else {
        val (prefix, suffix0) = events.span(!_.isInstanceOf[AssetEvent])
        val (assetEvents0, suffix) = suffix0.span(_.isInstanceOf[AssetEvent])
        val assetEvents = assetEvents0.asInstanceOf[List[AssetEvent]]
        split(suffix, sortings :+ SortingEvents(prefix, assetEvents))
      }
    }

    // Rejoins groups of consecutive Events and AssetEvents
    def join(sortings: List[SortingEvents]): List[Event] = {
      @scala.annotation.tailrec
      def loop(joined: List[Event], savings: Savings, remaining: List[SortingEvents]): List[Event] =
        remaining match {
          case head :: tail =>
            if (head.assetEvents.isEmpty && tail.nonEmpty) {
              // Merge head with next group as it only contains pure events
              val head2 = tail.head
              val tail2 = tail.tail
              loop(joined, savings, head2.copy(prefix = head.prefix ::: head2.prefix) :: tail2)
            } else {
              // Flatten pure events
              val prefix = savings.flattenEvents(head.prefix)
              // Finally sort asset events by date, as they may be out of order due
              // to original order in the same group or reordering groups.
              val joined2 = joined ::: prefix ::: head.assetEvents.sortBy(_.date)
              val savings2 = savings.processEvents(prefix)
              loop(joined2, savings2, tail)
            }

          case Nil =>
            joined
        }

      loop(Nil, Savings(), sortings)
    }

    // Gets asset ids for which there were actions
    def getIds(event: AssetEvent): Set[AssetId] =
      event match {
        case e: MakePayment  => Set(e.part.id)
        case e: MakeTransfer => Set(e.partSrc.id, e.partDst.id)
        case e: MakeRefund   => Set(e.part.id)
      }

    // Gets scheme ids for which we will need to move create/update events.
    // We need it when there was a creation.
    def getNeededSchemes(sortings: Seq[SortingEvents], ids: Set[AssetId]): Set[UUID] =
      ids.map(_.schemeId).filter { schemeId =>
        sortings.flatMap(_.prefix).exists {
          case e: CreateScheme => schemeId == e.schemeId
          case _               => false
        }
      }

    // Gets fund ids for which we will need to move create/update events.
    // We need it when there was a creation.
    def getNeededFunds(sortings: List[SortingEvents], ids: Set[AssetId]): Set[UUID] =
      ids.map(_.fundId).filter { fundId =>
        sortings.flatMap(_.prefix).exists {
          case e: CreateFund    => fundId == e.fundId
          case _                => false
        }
      }

    // Gets whether we need to move an asset event
    def extractNeeded(event: Event, ids: Set[AssetId], schemeIds: Set[UUID], fundIds: Set[UUID]): Boolean =
      event match {
        case e: CreateScheme  => schemeIds.contains(e.schemeId)
        case e: UpdateScheme  => schemeIds.contains(e.schemeId)
        case e: CreateFund    => fundIds.contains(e.fundId)
        case e: UpdateFund    => fundIds.contains(e.fundId)
        case e: AssociateFund => ids.contains(e.id)
        case _                => false
      }

    // Move Events from one group to another if necessary, that is if the
    // source groups contain asset events that match our criteria.
    def extractCreations(to: SortingEvents, from: List[SortingEvents], movedIds: Set[AssetId]): (SortingEvents, List[SortingEvents]) = {
      val schemeIds = getNeededSchemes(from, movedIds)
      val fundIds = getNeededFunds(from, movedIds)
      from.foldLeft((to, List.empty[SortingEvents])) {
        case ((accTo, accSortings), sorting) =>
          val (extracted, remaining) = sorting.prefix.partition(extractNeeded(_, movedIds, schemeIds, fundIds))
          val accTo2 = accTo.addEvents(extracted)
          val accSorting2 = (accSortings :+ sorting.copy(prefix = remaining)).filterNot(_.isEmpty)
          (accTo2, accSorting2)
      }
    }

    // Move AssetEvents from one group to another if necessary, that is if the
    // source groups contain dates predating maximal date of destination group.
    def extractActions(to: SortingEvents, from: List[SortingEvents]): (SortingEvents, List[SortingEvents]) =
      to.dateRange match {
        case Some(dateRange) =>
          from.foldLeft((to, List.empty[SortingEvents])) {
            case ((accTo, accSortings), sorting) =>
              if (sorting.dateRange.exists(_.min < dateRange.max)) {
                // Extract actions and move them to head group
                val (extracted, remaining) = sorting.assetEvents.partition(_.date < dateRange.max)
                val accTo2 = accTo.addAssetEvents(extracted)
                // Drop empty groups in remaining ones
                val accSortings2 = (accSortings :+ sorting.copy(assetEvents = remaining)).filterNot(_.isEmpty)
                // Then move associated events (scheme/fund creation/update) when needed
                val movedIds = accTo2.assetEvents.filter(sorting.assetEvents.contains).flatMap(getIds).toSet
                extractCreations(accTo2, accSortings2, movedIds)
              } else {
                (accTo, accSortings :+ sorting)
              }
          }

        case _ =>
          (to, from)
      }

    // Sorts events: loop over each group, and integrate AssetEvents of
    // remaining groups if necessary.
    // As the final step in each group, sort AssetEvents by date.
    @scala.annotation.tailrec
    def sort(sortings: List[SortingEvents], sorted: List[SortingEvents]): List[SortingEvents] =
      sortings match {
        case head :: tail =>
          val (head2, tail2) = extractActions(head, tail)
          sort(tail2, sorted :+ head2)

        case Nil =>
          sorted
      }

    val sortings = split(events, Nil)
    val sortedGroups = sort(sortings, Nil)
    val sorted = join(sortedGroups)
    (sorted, sorted != events)
  }

  /**
   * Converts events to JSON in a human readable form.
   *
   * For events that reference scheme or fund by id, add a field including the
   * actual name of the scheme/fund.
   *
   * @param events to process
   * @return JSON form of events, enriched with necessary information
   */
  def humanReadableJson(events: List[Event]): List[JsValue] = {
    import JsonProtocol._

    @scala.annotation.tailrec
    def enrich(events: List[Savings.Event], savings: Savings, jsons: List[JsValue]): List[JsValue] = {
      if (events.isEmpty) jsons
      else {
        // The next event to process
        val event = events.head
        // Compute savings (to track schemes/funds names)
        val savings2 = savings.processEvent(event)
        // The nominal JSON form of the event
        val js0 = event.toJson.asJsObject
        // Adds fields to given JSON object key.
        def update0(obj: JsObject, key: List[String], fields: Map[String, JsValue]): JsObject = key match {
          case head :: tail => obj.copy(fields = obj.fields + (head -> update0(obj.fields(head).asJsObject, tail, fields)))
          case _ => obj.copy(fields = obj.fields ++ fields)
        }
        def update(obj: JsObject, key: Option[String], fields: Map[String, JsValue]): JsObject =
          update0(obj, "content" :: key.toList, fields)
        def updateScheme(obj: JsObject, schemeId: UUID): JsObject =
          update(obj, None, Map("schemeName" -> JsString(savings.getScheme(schemeId).name)))
        def updateFund(obj: JsObject, fundId: UUID): JsObject =
          update(obj, None, Map("fundName" -> JsString(savings.getFund(fundId).name)))
        def updatePart(obj: JsObject, key: String, part: Savings.AssetPart): JsObject = update(obj, Some(key), Map(
          "schemeName" -> JsString(savings.getScheme(part.schemeId).name),
          "fundName" -> JsString(savings.getFund(part.fundId).name)
        ))
        // Add scheme or fund name where necessary.
        val eventJson = event match {
          case e: Savings.DeleteScheme =>
            updateScheme(js0, e.schemeId)

          case e: Savings.DeleteFund =>
            updateFund(js0, e.fundId)

          case e: Savings.AssociateFund =>
            val js1 = updateScheme(js0, e.schemeId)
            updateFund(js1, e.fundId)

          case e: Savings.DissociateFund =>
            val js1 = updateScheme(js0, e.schemeId)
            updateFund(js1, e.fundId)

          case e: Savings.MakePayment =>
            updatePart(js0, "part", e.part)

          case e: Savings.MakeRefund =>
            updatePart(js0, "part", e.part)

          case e: Savings.MakeTransfer =>
            val js1 = updatePart(js0, "partSrc", e.partSrc)
            updatePart(js1, "partDst", e.partDst)

          case _ =>
            js0
        }
        enrich(events.tail, savings2, jsons :+ eventJson)
      }
    }

    enrich(events, Savings(), Nil)
  }

  /**
   * Gets NAVs out of events.
   *
   * This can complete data store NAVs, especially for old (now deleted) funds.
   *
   * @param events events to get NAVs out of
   * @return NAVs
   */
  def getEventsNAVs(events: Seq[Event]): Map[UUID, Seq[AssetValue]] = {
    val assetEvents = events.filter(_.isInstanceOf[Savings.AssetEvent]).asInstanceOf[Seq[Savings.AssetEvent]]
    assetEvents.flatMap {
      case e: Savings.MakePayment =>
        List(e.part.fundId -> Savings.AssetValue(e.date, e.part.value))

      case e: Savings.MakeTransfer =>
        List(e.partSrc.fundId -> Savings.AssetValue(e.date, e.partSrc.value),
          e.partDst.fundId -> Savings.AssetValue(e.dstDate.getOrElse(e.date), e.partDst.value))

      case e: Savings.MakeRefund =>
        List(e.part.fundId -> Savings.AssetValue(e.date, e.part.value))
    }.groupBy(_._1).view.mapValues { v =>
      v.map(_._2).sortBy(_.date)
    }.toMap
  }

  /**
   * Gets a fund NAV at given date, from history events and data store NAV history.
   *
   * Nearest predating NAV is returned if there is none for requested date.
   *
   * @param owner parent window
   * @param fundId fund to get NAV for
   * @param date date to get NAV on
   * @param eventsNAVs NAVs deduced from history events
   */
  def getNAV(owner: Option[Window], fundId: UUID, date: LocalDate,
    eventsNAVs: Map[UUID, Seq[AssetValue]] = Map.empty): Option[Savings.AssetValue] =
    (eventsNAVs.getOrElse(fundId, Nil) ++ Awaits.readDataStoreNAV(owner, fundId, date).getOrElse(None)).filter { nav =>
      nav.date <= date
    }.sortBy(date.toEpochDay - _.date.toEpochDay).headOption

  /**
   * Gets a cached fund NAV at given date from data store NAV history.
   *
   * Nearest predating NAV is returned if there is none for requested date.
   *
   * @param owner parent window
   * @param fund fund to get NAV for
   * @param date date to get NAV on
   * @param cache cached NAVs (updated with new value when applicable)
   */
  private def getNAV(owner: Option[Window], fund: Fund, date: LocalDate,
    cache: mutable.Map[LocalDate, AssetValue]): Option[Savings.AssetValue] =
  {
    cache.get(date).orElse {
      val nav = getNAV(owner, fund.id, date)
      if (Settings.debug(Debug.LeviesComputation))
        logger.info(s"action=<getNAV> id=<${fund.name}> date=<$date> nav=<$nav>")
      nav.foreach { v =>
        cache(date) = v
      }
      nav
    }
  }

  case class Scheme(id: UUID, name: String, comment: Option[String], funds: List[UUID],
    used: Boolean = false, active: Boolean = false, disabled: Boolean = false)
  {
    // See: http://stackoverflow.com/a/19348339
    import scala.math.Ordered.orderingToOrdered
    def compareParams(other: Scheme): Int =
      (name, comment, disabled).compare((other.name, other.comment, other.disabled))
  }

  case class Fund(id: UUID, name: String, amfId: Option[String], comment: Option[String],
    used: Boolean = false, active: Boolean = false, disabled: Boolean = false)
  {
    // See: http://stackoverflow.com/a/19348339
    import scala.math.Ordered.orderingToOrdered
    def compareParams(other: Fund): Int =
      (name, amfId, comment, disabled).compare((other.name, other.amfId, other.comment, other.disabled))
  }

  case class AssetId(schemeId: UUID, fundId: UUID) {
    def toString(savings: Savings): String =
      Settings.toString(this, s"${savings.getFund(fundId).name} / ${savings.getScheme(schemeId).name}")
  }

  case class Assets(list: List[Asset] = Nil, vwaps: Map[AssetId, BigDecimal] = Map.empty) {
    lazy val byId: Map[AssetId, List[Asset]] = list.groupBy(_.id)
    def units(id: AssetId): BigDecimal = byId.getOrElse(id, Nil).map(_.units).sum
    def amount(id: AssetId, value: BigDecimal): BigDecimal = scaleAmount(units(id) * value)
    def investedAmount(id: AssetId): BigDecimal = amount(id, vwaps.getOrElse(id, 0))
    def addAsset(asset: Asset): Assets = copy(list = list :+ asset)
    def updateAsset(date: LocalDate, asset: Asset): Assets =
      copy(list = list.map { currentAsset =>
        if (currentAsset.matches(date, asset)) asset
        else currentAsset
      })
    def removeAsset(date: LocalDate, asset: AssetEntry): Assets = {
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
    lazy val investedAmount: BigDecimal = amount(vwap)
    def toString(savings: Savings): String =
      Settings.toString(this, s"Asset(${id.toString(savings)},$units,$vwap,${availability.getOrElse("")})")
  }

  /** Asset value: holds asset value at a given date. */
  case class AssetValue(date: LocalDate, value: BigDecimal)
    extends ChartSeriesData with DebugString
  {
    override protected def debugString: String = s"$value@$date"
  }

  case class AssetValueHistory(name: Option[String], values: List[Savings.AssetValue] = Nil)

  case class AssetPart(schemeId: UUID, fundId: UUID, availability: Option[LocalDate], units: BigDecimal, value: BigDecimal)
    extends AssetEntry
  {
    def amount(value: BigDecimal): BigDecimal = scaleAmount(units * value)
    def toString(savings: Savings): String =
      Settings.toString(this, s"Asset(${id.toString(savings)},$units,$value,${availability.getOrElse("")})")
  }

  case class UnavailabilityPeriod(id: String, years: Int, month: Option[Month])

  sealed trait Event

  sealed trait AssetEvent extends Event {
    val date: LocalDate
    val comment: Option[String]
  }

  case class CreateScheme(schemeId: UUID, name: String, comment: Option[String])
    extends Event

  case class UpdateScheme(schemeId: UUID, name: String, comment: Option[String], disabled: Boolean)
    extends Event

  object UpdateScheme {
    def apply(scheme: Scheme): UpdateScheme = UpdateScheme(scheme.id, scheme.name, scheme.comment, scheme.disabled)
  }

  case class DeleteScheme(schemeId: UUID)
    extends Event

  case class CreateFund(fundId: UUID, name: String, amfId: Option[String], comment: Option[String])
    extends Event

  case class UpdateFund(fundId: UUID, name: String, amfId: Option[String], comment: Option[String], disabled: Boolean)
    extends Event

  object UpdateFund {
    def apply(fund: Fund): UpdateFund = UpdateFund(fund.id, fund.name, fund.amfId, fund.comment, fund.disabled)
  }

  case class DeleteFund(fundId: UUID)
    extends Event

  case class AssociateFund(schemeId: UUID, fundId: UUID)
    extends Event
  {
    def id = AssetId(schemeId, fundId)
  }

  case class DissociateFund(schemeId: UUID, fundId: UUID)
    extends Event
  {
    def id = AssetId(schemeId, fundId)
  }

  case class MakePayment(date: LocalDate, part: AssetPart, comment: Option[String])
    extends AssetEvent

  case class MakeTransfer(date: LocalDate, partSrc: AssetPart, dstDate: Option[LocalDate], partDst: AssetPart, comment: Option[String])
    extends AssetEvent

  case class MakeRefund(date: LocalDate, part: AssetPart, comment: Option[String])
    extends AssetEvent

  object JsonProtocol extends DefaultJsonProtocol with JsonFormats {

    implicit val createSchemeFormat: RootJsonFormat[CreateScheme] = jsonFormat3(CreateScheme)
    implicit val updateSchemeFormat: RootJsonFormat[UpdateScheme] = jsonFormat4(UpdateScheme.apply)
    implicit val deleteSchemeFormat: RootJsonFormat[DeleteScheme] = jsonFormat1(DeleteScheme)
    implicit val createFundFormat: RootJsonFormat[CreateFund] = jsonFormat4(CreateFund)
    implicit val updateFundFormat: RootJsonFormat[UpdateFund] = jsonFormat5(UpdateFund.apply)
    implicit val deleteFundFormat: RootJsonFormat[DeleteFund] = jsonFormat1(DeleteFund)
    implicit val associateFundFormat: RootJsonFormat[AssociateFund] = jsonFormat2(AssociateFund)
    implicit val dissociateFundFormat: RootJsonFormat[DissociateFund] = jsonFormat2(DissociateFund)
    implicit val assetPartFormat: RootJsonFormat[AssetPart] = jsonFormat5(AssetPart)
    implicit val makePaymentFormat: RootJsonFormat[MakePayment] = jsonFormat3(MakePayment)
    implicit val makeTransferFormat: RootJsonFormat[MakeTransfer] = jsonFormat5(MakeTransfer)
    implicit val makeRefundFormat: RootJsonFormat[MakeRefund] = jsonFormat3(MakeRefund)

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
  latestAssetAction: Option[LocalDate] = None,
  levies: Levies = Levies.empty,
  leviesData: Map[Savings.AssetId, LeviesPeriodsData] = Map.empty
) extends StrictLogging {

  import Main.settings.{scaleAmount, scaleVWAP}
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

  // Notes on levies:
  // We need to know for each fund and each levy period:
  //   - the invested amount: it is computed from the NAV on the day before the
  //     start of the period, and any payment/transfer done during the period.
  //   - the gross amount at the end of the period (computed from the NAV)
  // For a fund, during a levy period: gain/loss = grossAmount - investedAmount
  // Upon loss:
  //   - the invested amount of the period is kept to compute the gain/loss of
  //     the next period (meaning the loss is pushed forward on the next period)
  //   - the period is zeroed (as the loss is pushed to the next period)
  //   - upon computing an actual amount levied (refund), loss on the last
  //     period is pushed back on any previous period recursively until either
  //     * there is no more loss: the levy rate of the reached period is applied
  //       on the gain remaining for that period
  //    or
  //     * there is no more period: it is considered a global loss and the levy
  //       is not due
  //
  // If there is a transfer or refund after a levy period has ended, the
  // associated gain/loss is computed proportionally to the total fund units.
  // Upon transfer the proportioned gain/loss of each past period, and the
  // current period proportioned invested amount, are applied on the destination
  // fund.
  // In both cases the gain/loss on the source fund is updated (remaining value
  // after removing the proportioned gain/loss).
  //
  // For easier computing, missing periods are replaced by fake periods with 0%
  // levy rate.
  // This e.g. takes care of actual loss after a levy ended, that is taking into
  // account the actual fund gross amount compared to when the levy was active:
  //   - upon global loss, no past levy is due
  //   - upon lowered gain (current gain is lower than what if was when the levy
  //     ended), the amount due is lowered accordingly
  // (How it works: the fake period will be seen as a loss and thus pushed back
  // on previous periods as explained above.)
  // Since there may be multiple levies not starting at the same date, taking
  // into account there should not be levies amount to pay for a particular levy
  // if it started after the others but there was a loss since then, a fake
  // period is also placed first when necessary, so that all levies start at the
  // same date.
  // (Note: should this actually extend before any levies to take into account
  // the real initial invested amount instead of the 'perceived' invested amount
  // at the start of all levies ?)
  //
  // TODO: should we try to detect missing NAV values (that should have been
  //       nearer than found ones) ?

  def hasLevies: Boolean = levies.levies.nonEmpty

  protected def addLeviesPeriodsData(assetId: AssetId, leviesPeriodsData: LeviesPeriodsData): Savings =
    copy(leviesData = leviesData + (assetId -> leviesPeriodsData))

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
    processEvents(events.toSeq)

  def processEvent(event: Event): Savings = event match {
    case CreateScheme(id, name, comment)                  => createScheme(id, name, comment)
    case UpdateScheme(id, name, comment, disabled)        => updateScheme(id, name, comment, disabled)
    case DeleteScheme(id)                                 => deleteScheme(id)
    case CreateFund(id, name, amfId, comment)             => createFund(id, name, amfId, comment)
    case UpdateFund(id, name, amfId, comment, disabled)   => updateFund(id, name, amfId, comment, disabled)
    case DeleteFund(id)                                   => deleteFund(id)
    case AssociateFund(schemeId, fundId)                  => associateFund(schemeId, fundId)
    case DissociateFund(schemeId, fundId)                 => dissociateFund(schemeId, fundId)
    case MakePayment(date, part, _)                       => computeAssets(date).makePayment(date, part)
    case MakeTransfer(date, partSrc, dstDate, partDst, _) => computeAssets(date).makeTransfer(date, partSrc, dstDate, partDst)
    case MakeRefund(date, part,    _)                     => computeAssets(date).makeRefund(date, part)._1
  }

  protected def createScheme(id: UUID, name: String, comment: Option[String]): Savings = {
    copy(schemes = schemes :+ Scheme(id, name, comment, Nil))
  }

  protected def updateScheme(id: UUID, name: String, comment: Option[String], disabled: Boolean): Savings = {
    val updated = schemes.map { scheme =>
      if (scheme.id != id) scheme
      else scheme.copy(name = name, comment = comment, disabled = disabled && !scheme.active)
    }
    copy(schemes = updated)
  }

  protected def deleteScheme(id: UUID): Savings = {
    copy(schemes = schemes.filterNot(_.id == id))
  }

  protected def createFund(id: UUID, name: String, amfId: Option[String], comment: Option[String]): Savings = {
    copy(funds = funds :+ Fund(id, name, amfId, comment))
  }

  protected def updateFund(id: UUID, name: String, amfId: Option[String], comment: Option[String], disabled: Boolean): Savings = {
    val updated = funds.map { fund =>
      if (fund.id != id) fund
      else fund.copy(name = name, amfId = amfId, comment = comment, disabled = disabled && !fund.active)
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

  protected def triggerActiveScheme(schemeId: UUID, set: Boolean): Savings = {
    val updated = schemes.map { scheme =>
      if (scheme.id != schemeId) scheme
      else scheme.copy(used = true, active = set)
    }
    copy(schemes = updated)
  }

  protected def triggerActiveFund(fundId: UUID, set: Boolean): Savings = {
    val updated = funds.map { fund =>
      if (fund.id != fundId) fund
      else fund.copy(used = true, active = set)
    }
    copy(funds = updated)
  }

  protected def triggerActiveAsset(part: AssetPart): Savings = {
    triggerActiveScheme(part.schemeId, set = true).triggerActiveFund(part.fundId, set = true)
  }

  protected def checkActiveAsset(part: AssetPart): Savings = {
    triggerActiveScheme(part.schemeId, assets.byId.keys.exists(_.schemeId == part.schemeId))
      .triggerActiveFund(part.fundId, assets.byId.keys.exists(_.fundId == part.fundId))
  }

  protected def updateLatestAssetAction(date: LocalDate): Savings = {
    val newDate = latestAssetAction match {
      case Some(latestDate) => if (date > latestDate) date else latestDate
      case None             => date
    }
    copy(latestAssetAction = Some(newDate))
  }

  protected def makePayment(date: LocalDate, part: AssetPart,
    partSrc: Option[AssetPart] = None, savingsSrc: Option[Savings] = None,
    srcLeviesPeriodsData: Option[LeviesPeriodsData] = None): Savings =
  {
    // If transferring, don't set invested amount as we will properly do it
    // while merging.
    val savings0 =
      if (partSrc.isDefined) checkLeviesPeriod(part.id, date, 0)
      else checkLeviesPeriod(part.id, date, part.amount(part.value))
    val (savings1, extraInvestedAmount, extraInvestedAmountFine) = partSrc match {
      case Some(src) =>
        val savings = savingsSrc.get
        val srcAsset = savings.findAsset(date, src).get
        val leviesPeriodsData = srcLeviesPeriodsData.get
        if (hasLevies && Settings.debug(Debug.LeviesComputation))
          logger.info(s"action=<makePayment> date=<$date> id=<${part.id.toString(savings)}> srcId=<${src.id.toString(savings)}> srcLevies=<$leviesPeriodsData>")
        val savings1 = savings0.mergeLeviesPeriods(part.id, leviesPeriodsData)
        // Note: invested amount = units * VWAP
        val extraInvestedAmount = src.amount(savings.assets.vwaps(src.id))
        val extraInvestedAmountFine = src.amount(srcAsset.vwap)
        (savings1, extraInvestedAmount, extraInvestedAmountFine)

      case None =>
        val extraInvestedAmount = part.amount(part.value)
        val extraInvestedAmountFine = extraInvestedAmount
        (savings0, extraInvestedAmount, extraInvestedAmountFine)
    }

    // Note: VWAP = invested amount / units
    val savings2 = savings1.findAsset(date, part) match {
      case Some(currentAsset) =>
        val units = currentAsset.units + part.units
        val vwap = scaleVWAP((currentAsset.investedAmount + extraInvestedAmountFine) / units)
        // Note: keeping the given (and not existing) asset availability date
        // has the nice side effect of resetting it if the existing asset is
        // actually available for the given date.
        val asset = Asset(part.schemeId, part.fundId, part.availability, units, vwap)
        savings1.updateAsset(date, asset)

      case None =>
        val vwap = scaleVWAP(if (partSrc.isDefined) extraInvestedAmountFine / part.units else part.value)
        val asset = Asset(part.schemeId, part.fundId, part.availability, part.units, vwap)
        savings1.copy(assets = assets.addAsset(asset))
    }

    // Notes:
    // Current Savings is used to get the previous invested amount
    // savings2 already has the total number of units after operation
    val assets0 = savings2.assets
    val units = assets0.units(part.id)
    val vwap = scaleVWAP((assets.investedAmount(part.id) + extraInvestedAmount) / units)
    val savings = savings2.copy(assets = assets0.copy(vwaps = assets0.vwaps + (part.id -> vwap)))

    savings.updateLatestAssetAction(date).triggerActiveAsset(part)
  }

  protected def makeTransfer(date: LocalDate, partSrc: AssetPart, dstDate: Option[LocalDate], partDst: AssetPart): Savings = {
    val (savings, leviesPeriodsData) = makeRefund(date, partSrc)
    savings.makePayment(dstDate.getOrElse(date), partDst, Some(partSrc), Some(this), Some(leviesPeriodsData))
  }

  protected def makeRefund(date: LocalDate, part: AssetPart): (Savings, LeviesPeriodsData) = {
    val currentAsset = findAsset(date, part).get
    val totalUnits = assets.units(part.id)
    val units = currentAsset.units - part.units
    val emptied = (units == 0) && (totalUnits == part.units)
    val vwap = currentAsset.vwap

    val leviesPeriodsData =
      if (!hasLevies) LeviesPeriodsData()
      else {
        val debug =
          if (!Settings.debug(Debug.LeviesHistory)) None
          else Some(s"action date=<$date>: refunding units=<${part.units}> from totalUnits=<$totalUnits>")
        checkLeviesPeriod(part.id, date, 0).leviesData(part.id).addDebugs(debug.toList)
      }
    val (outgoingLevies, remainingLevies) = leviesPeriodsData.proportioned(part.units / totalUnits)
    if (hasLevies && Settings.debug(Debug.LeviesComputation))
      logger.info(s"action=<makeRefund> date=<$date> id=<${part.id.toString(this)}> totalUnits=<$totalUnits> units=<${part.units}> emptied=<$emptied> outgoingLevies=<$outgoingLevies>, remainingLevies=<$remainingLevies>")

    // Note: keep existing asset availability date if any, instead of using
    // given one (which may be empty if asset is actually available for the
    // given date).
    val savings0 =
      if (!hasLevies) this
      else if (emptied) copy(leviesData = leviesData - part.id)
      else addLeviesPeriodsData(part.id, remainingLevies)
    val savings =
      if (units <= 0) savings0.removeAsset(date, part).checkActiveAsset(part)
      else savings0.updateAsset(date, Asset(part.schemeId, part.fundId, currentAsset.availability, units, vwap))
    (savings.updateLatestAssetAction(date), outgoingLevies)
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

  def getSchemes(associated: Option[UUID]): List[Scheme] = {
    val r = associated match {
      case Some(fundId) => schemes.filter(_.funds.contains(fundId))
      case None         => schemes
    }
    r.sortBy(_.name)
  }

  def findFund(fundId: UUID): Option[Fund] =
    funds.find(_.id == fundId)

  def getFund(fundId: UUID): Fund =
    findFund(fundId).get

  def getFunds(associated: Boolean): List[Fund] = {
    val r =
      if (!associated) funds
      else schemes.flatMap(_.funds).distinct.map(getFund)
    r.sortBy(_.name)
  }

  def hasAsset(schemeId: UUID, fundId: UUID): Boolean =
    assets.byId.contains(AssetId(schemeId, fundId))

  def createSchemeEvent(name: String, comment: Option[String] = None): CreateScheme = {
    val id = newId(schemes.map(_.id))
    CreateScheme(id, name, comment)
  }

  def getSchemeEvent(schemeId: UUID): CreateScheme = {
    val scheme = getScheme(schemeId)
    CreateScheme(scheme.id, scheme.name, scheme.comment)
  }

  def createFundEvent(name: String, amfId: Option[String] = None, comment: Option[String] = None): CreateFund = {
    val id = newId(funds.map(_.id))
    CreateFund(id, name, amfId, comment)
  }

  def getFundEvent(fundId: UUID): CreateFund = {
    val fund = getFund(fundId)
    CreateFund(fund.id, fund.name, fund.amfId, fund.comment)
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
    // 3. remaining schemes updates
    // 4. remaining funds updates
    // 5. created schemes (possibly disabled)
    // 6. created funds (possibly disabled)
    // 7. funds associated to created schemes
    // 8. created funds remaining associations
    // Note: for consistency, it is better to update funds before creating new
    // ones (as created ones may re-use a renamed existing one).
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
    } ::: schemesRemainingOrdered.flatMap { newScheme =>
      val oldScheme = getScheme(newScheme.id)
      val event1 =
        if (newScheme.compareParams(oldScheme) == 0) None
        else Some(Savings.UpdateScheme(newScheme.id, newScheme.name, newScheme.comment, newScheme.disabled))

      // Note: don't forget to ignore created/deleted funds (already taken care of)
      val oldFunds = oldScheme.funds.toSet
      val newFunds = newScheme.funds.toSet
      val fundsAssociated = (newFunds -- oldFunds) -- fundsCreated
      val fundsAssociatedOrdered = newScheme.funds.filter(fundsAssociated.contains)
      val fundsDissociated = (oldFunds -- newFunds) -- fundsDeleted

      event1.toList ::: fundsDissociated.toList.map { fundId =>
        Savings.DissociateFund(oldScheme.id, fundId)
      } ::: fundsAssociatedOrdered.map { fundId =>
        Savings.AssociateFund(oldScheme.id, fundId)
      }
    } ::: fundsRemainingOrdered.flatMap { newFund =>
      val oldFund = getFund(newFund.id)
      if (newFund.compareParams(oldFund) == 0) None
      else Some(Savings.UpdateFund(newFund.id, newFund.name, newFund.amfId, newFund.comment, newFund.disabled))
      // Note: association/dissociation to old, new or remaining schemes have
      // been taken care of already.
    } ::: schemesCreatedOrdered.map { scheme =>
      Savings.CreateScheme(scheme.id, scheme.name, scheme.comment)
    } ::: schemesCreatedOrdered.filter(_.disabled).map { scheme =>
      Savings.UpdateScheme(scheme).copy(disabled = scheme.disabled)
    } ::: fundsCreatedOrdered.map { fund =>
      Savings.CreateFund(fund.id, fund.name, fund.amfId, fund.comment)
    } ::: fundsCreatedOrdered.filter(_.disabled).map { fund =>
      Savings.UpdateFund(fund).copy(disabled = fund.disabled)
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
    }

    // Ensure flattening is OK
    val flattenedSavings = processEvents(flattenedEvents)
    if (flattenedSavings != newSavings) {
      logger.warn(s"Events flattening failed: savings[$this] events[$events] flattened[$flattenedEvents]")
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
   * Updates asset levy periods data up to given date if necessary.
   *
   * If asset levy is not up to date (i.e. known for the period the given
   * date belongs to) it is computed from first applicable period up to
   * the current matching period.
   */
  protected def updateLevyPeriods(id: AssetId, levy: String, date: LocalDate, investedAmount: BigDecimal, cachedNAVs: mutable.Map[LocalDate, AssetValue]): Savings = {
    // We expect that:
    //   - levies periods are properly sorted
    //   - there is no gap between periods, and last period is open-ended (must have been normalized with fake '0 rate' ones)
    //   - we are called with increasing dates
    // Meaning we only get to move forward for everything.
    val totalUnits = assets.units(id)
    val assetData = leviesData.getOrElse(id, LeviesPeriodsData())
    val levyData0 = assetData.data.getOrElse(levy, Nil)
    val levyPeriods = levies.levies(levy).periods

    // Check whether we have to initiate a first period data, and what are the
    // periods to go through (excepting the first/current one).
    val (initiateData, debug, warning, remainingPeriods) = levyData0.headOption match {
      case Some(current) =>
        // We need to go from the current period up to the last matching one
        (None, None, None, levyPeriods.dropWhile(_ != current.period).tail.takeWhile(_.start <= date))

      case None =>
        // There is no levy period data yet.
        // Either all previous actions preceded the first levy period, or this is
        // the first action for this asset.
        if (totalUnits == 0) {
          // This is the first asset action.
          // If a period matches the date, create a new period data with the
          // invested amount on action date (given by caller).
          val r = levyPeriods.find { period =>
            (period.start <= date) && period.end.forall(_ >= date)
          }.map { period =>
            // Note: invested amount will be added to this period right below
            val periodData = LevyPeriodData(
              period = period,
              investedAmount = 0,
              grossGain = None
            )
            val debug =
              if (!Settings.debug(Debug.LeviesHistory)) None
              else Some(s"action date=<$date> levy=<$levy>: found first matching period=<$period> with invested amount=<$investedAmount>")
            (periodData, debug)
          }
          // In any case, there is no other period to go to.
          (r.map(_._1), r.flatMap(_._2), None, Nil)
        } else {
          // There were previous actions, but all preceding the first levy period.
          // If the date does not precede the first period, create a new period
          // data with the NAV on start (of period - actually the day before) date.
          val (initiateData, debug, warning) = levyPeriods.headOption.find(_.start <= date).map { period =>
            val navDate = period.start.minusDays(1)
            getNAV(None, getFund(id.fundId), navDate, cachedNAVs) match {
              case Some(v) =>
                val periodData = LevyPeriodData(
                  period = period,
                  investedAmount = scaleAmount(v.value * totalUnits),
                  grossGain = None
                )
                val debug =
                  if (!Settings.debug(Debug.LeviesHistory)) None
                  else Some(s"action date=<$date> levy=<$levy>: found first matching period=<$period> with starting date=<$navDate> nav=<$v> totalUnits=<$totalUnits> and invested amount=<${periodData.investedAmount}>")
                (Some(periodData), debug, None)

              case None =>
                val warning = Strings.accountHistoryIssuesNAV.format(getFund(id.fundId).name, navDate)
                (None, None, Some(warning))
            }
          }.getOrElse((None, None, None))
          // If the first period matched, we will need to go up to the last period
          // which still matches.
          val remainingPeriods =
            if (initiateData.isEmpty) Nil
            else levyPeriods.tail.takeWhile(_.start <= date)
          (initiateData, debug, warning, remainingPeriods)
        }
    }

    @scala.annotation.tailrec
    def loop(data: List[LevyPeriodData], debugs: List[String], warnings: List[String],
      periods: List[LevyPeriod]): (List[LevyPeriodData], List[String], List[String]) =
    {
      periods match {
        case head :: tail =>
          val currentPeriodData = data.head
          val currentPeriod = currentPeriodData.period
          // Get end of period NAV and computed fund gross amount; fallback to
          // actual invested amount - that is consider no gain/loss for the
          // period - if no NAV is found.
          // Note: period end being 1 day before the next period start, it is
          // the correct date to get the NAV for.
          val navDate = currentPeriod.end.get
          val (grossAmount, debug0, warning) = getNAV(None, getFund(id.fundId), navDate, cachedNAVs).map { v =>
            val grossAmount = scaleAmount(v.value * totalUnits)
            val debug =
              if (!Settings.debug(Debug.LeviesHistory)) None
              else Some(s"action date=<$date> levy=<$levy>: closing period=<$currentPeriod> with ending date=<$navDate> nav=<$v> totalUnits=<$totalUnits> and gross amount=<$grossAmount>")
            (grossAmount, debug, Option.empty[String])
          }.getOrElse {
            val warning = Strings.accountHistoryIssuesNAV.format(getFund(id.fundId).name, navDate)
            (currentPeriodData.investedAmount, None, Some(warning))
          }
          // Compute gross gain, and end period.
          val grossGain = currentPeriodData.getGrossGain(grossAmount, 1)
          val (completedPeriod, debug1, investedAmount) = if (grossGain < 0) {
            // This is a loss, zero this period, and keep previously invested amount for next period.
            val debug =
              if (!Settings.debug(Debug.LeviesHistory)) None
              else Some(s"action date=<$date> levy=<$levy>: resetting closed period=<$currentPeriod> gross loss=<$grossGain> and keeping invested amount=<${currentPeriodData.investedAmount}> for next period")
            (currentPeriodData.zero, debug.toList ::: debug0.toList, currentPeriodData.investedAmount)
          } else {
            // Complete the current period.
            val debug =
              if (!Settings.debug(Debug.LeviesHistory)) None
              else Some(s"action date=<$date> levy=<$levy>: completing closed period=<$currentPeriod> with gross gain=<$grossGain> and using gross amount as invested amount=<$grossAmount> for next period")
            (currentPeriodData.complete(grossGain), debug.toList ::: debug0.toList, grossAmount)
          }

          // Now open the next period.
          val nextPeriodData = LevyPeriodData(
            period = head,
            investedAmount = investedAmount,
            grossGain = None
          )
          val debug =
            if (!Settings.debug(Debug.LeviesHistory)) None
            else Some(s"action date=<$date> levy=<$levy>: opening period=<$head> with invested amount=<$investedAmount>")
          if (Settings.debug(Debug.LeviesComputation))
            logger.info(s"action=<updateLevyPeriods> date=<$date> id=<${id.toString(this)}> levy=<$levy> period=<$currentPeriodData> totalUnits=<$totalUnits> grossAmount=<$grossAmount> grossGain=<$grossGain> completed=<$completedPeriod> next=<$nextPeriodData>")
          loop(nextPeriodData :: completedPeriod :: data.tail, debug.toList ::: debug1 ::: debugs, warnings ::: warning.toList, tail)

        case _ =>
          (data, debugs, warnings)
      }
    }

    val levyData1 = initiateData.toList ::: levyData0
    val (levyData2, debugs0, warnings) = loop(levyData1, debug.toList, warning.toList, remainingPeriods)
    // Now that we have updated periods, add invested amount (if any) to the
    // current period. Caller must give '0' for actions other than payment.
    val (levyData, debugs) = if ((investedAmount > 0) && levyData2.nonEmpty) {
      val head = levyData2.head
      val head2 = head.copy(investedAmount = head.investedAmount + investedAmount)
      // Note: if there was no asset yet, we already have the debug info
      // stating the initially invested amount
      val debug =
        if (!Settings.debug(Debug.LeviesHistory) || (totalUnits == 0)) None
        else Some(s"action date=<$date> levy=<$levy>: updating current period=<$head> with invested amount=<$investedAmount->${head2.investedAmount}>")
      (head2 :: levyData2.tail, debug.toList ::: debugs0)
    } else (levyData2, debugs0)
    if (Settings.debug(Debug.LeviesComputation))
      logger.info(s"action=<updateLevyPeriods> date=<$date> id=<${id.toString(this)}> totalUnits=<$totalUnits> investedAmount=<$investedAmount> levy=<$levy> levyData=<$levyData0> initiateData=<$initiateData> remainingPeriods<$remainingPeriods> updated=<$levyData> warnings=<$warnings>")
    if (levyData.isEmpty) this
    else addLeviesPeriodsData(id, assetData.addPeriodsData(levy, levyData).addDebugs(debugs).addWarnings(warnings))
  }

  /** Updates asset levies periods data up to given date if necessary. */
  protected def checkLeviesPeriod(id: AssetId, date: LocalDate, investedAmount: BigDecimal): Savings = {
    lazy val cachedNAVs = mutable.Map.empty[LocalDate, AssetValue]
    levies.list.foldLeft(this) { (savings, levy) =>
      savings.updateLevyPeriods(id, levy.name, date, investedAmount, cachedNAVs)
    }
  }

  /**
   * Computes actual asset levies for given date and NAV.
   *
   * Completes levies up to current period and compute final gain/loss.
   */
  def computeLevies(id: AssetId, date: LocalDate, nav: BigDecimal): LeviesPeriodsData = {
    if (hasLevies) {
      // Note: ensure periods are up to date.
      val savings = checkLeviesPeriod(id, date, 0)
      val leviesPeriodsData = savings.leviesData.getOrElse(id, LeviesPeriodsData())
      val totalUnits = assets.units(id)
      val grossAmount = scaleAmount(totalUnits * nav)
      val data = levies.list.flatMap { levy =>
        val r = leviesPeriodsData.data.get(levy.name).map { levyData =>
          @scala.annotation.tailrec
          def loop(updated: List[LevyPeriodData], debugs: List[String],
            periods: List[LevyPeriodData], pushedAmount: BigDecimal): (List[LevyPeriodData], List[String]) =
          {
            val head = periods.head
            val tail = periods.tail
            val grossGain = head.getGrossGain + pushedAmount
            // Keep on pushing until we remain with a gain or there is no other period to push back the loss to.
            if ((grossGain >= 0) || tail.isEmpty) {
              val debug =
                if (!Settings.debug(Debug.LeviesHistory)) None
                else Some(s"action date=<$date> levy=<${levy.name}>: completed period=<${head.period}> with gross gain/loss=<$grossGain>")
              (head.complete(grossGain) :: updated ::: tail, debug.toList ::: debugs)
            } else {
              val debug =
                if (!Settings.debug(Debug.LeviesHistory)) None
                else Some(s"action date=<$date> levy=<${levy.name}>: pushing back period=<${head.period}> gross loss=<$grossGain>")
              loop(head.zero :: updated, debug.toList ::: debugs, tail, grossGain)
            }
          }

          // We want to complete the current period, and push back the loss (if
          // any) on previous periods until either we remain with a gain on a
          // period or there is no more period to push back to.
          // We can achieve it by getting the gross gain of the current period,
          // zeroing the period and applying our rule starting from the current
          // period (as if its gain/loss was pushed back): if it was really a
          // gain it completes the period, otherwise the loss is pushed backward.
          val currentPeriodData = levyData.head
          val grossGain = currentPeriodData.getGrossGain(grossAmount, 1)
          // Since updated periods may not be in order, re-sort them (from newest to oldest)
          val (computedData, debugs) = loop(Nil, Nil, currentPeriodData.zero :: levyData.tail, grossGain)
          (computedData.sortBy(_.period.start).reverse, debugs)
        }
        r.map(levy.name -> _)
      }.toMap
      val debug =
        if (!Settings.debug(Debug.LeviesHistory)) None
        else Some(s"action date=<$date>: computing levies for nav=<$nav> totalUnits=<$totalUnits> and gross amount=<$grossAmount>")
      val computed = LeviesPeriodsData(
        data = data.view.mapValues(_._1).toMap,
        debugInfo = leviesPeriodsData.debugInfo,
        warnings = leviesPeriodsData.warnings
      ).addDebugs(data.values.flatMap(_._2).toList ::: debug.toList)
      if (Settings.debug(Debug.LeviesComputation))
        logger.info(s"action=<computeLevies> id=<${id.toString(this)}> nav=<$nav> computed=<$computed>")
      computed
    } else LeviesPeriodsData()
  }

  /**
   * Update asset levies periods data with given ones.
   *
   * For each period, merges gain/loss (invested amount for current period).
   */
  protected def mergeLeviesPeriods(id: AssetId, leviesPeriodsData: LeviesPeriodsData): Savings = {
    if (hasLevies) {
      // Note: since we expect caller to have updated levies periods, we should
      // only have gains (maybe 0) for completed (i.e. past) periods, and
      // invested amount for current period.
      val assetData = leviesData.getOrElse(id, LeviesPeriodsData())

      @scala.annotation.tailrec
      def loopData(levy: String, merged: List[LevyPeriodData], debugs: List[String], remaining: List[List[LevyPeriodData]]): (List[LevyPeriodData], List[String]) =
        remaining match {
          case periodData :: tail =>
            val isComplete = periodData.head.isComplete
            if (isComplete) {
              // This is a past period: merge gains, and for consistency also
              // merge invested amounts.
              val mergedPeriodData = periodData.reduceLeft { (period1, period2) =>
                val grossGain1 = period1.getGrossGain
                val grossGain2 = period2.getGrossGain
                assert(grossGain1 >= 0)
                assert(grossGain2 >= 0)
                val grossGain = grossGain1 + grossGain2
                val investedAmount = period1.investedAmount + period2.investedAmount
                period1.copy(investedAmount = investedAmount).complete(grossGain)
              }
              val debug =
                if (!Settings.debug(Debug.LeviesHistory)) None
                else Some(s"levy=<$levy>: merged completed period=<${mergedPeriodData.period}> data with invested amount=<${mergedPeriodData.investedAmount}> and gross gain=<${mergedPeriodData.getGrossGain}>")
              loopData(levy, mergedPeriodData :: merged, debug.toList ::: debugs, tail)
            } else {
              // This is the current period: merge invested amount
              val investedAmount = periodData.map(_.investedAmount).sum
              val mergedPeriodData = periodData.head.copy(investedAmount = investedAmount)
              val debug =
                if (!Settings.debug(Debug.LeviesHistory)) None
                else Some(s"levy=<$levy>: merged current period=<${mergedPeriodData.period}> data with invested amount=<${mergedPeriodData.investedAmount}>")
              loopData(levy, mergedPeriodData :: merged, debug.toList ::: debugs, tail)
            }

          case Nil =>
            (merged, debugs)
        }

      @scala.annotation.tailrec
      def loopLevies(merged: LeviesPeriodsData, debugs: List[String], remaining: List[String]): (LeviesPeriodsData, List[String]) =
        remaining match {
          case levy :: tail =>
            val srcLeviesData = leviesPeriodsData.data(levy)
            val dstLeviesData = merged.data.getOrElse(levy, Nil)
            if (Settings.debug(Debug.LeviesComputation))
              logger.info(s"action=<mergeLeviesPeriods> id=<${id.toString(this)}> levy=<$levy> src=<$srcLeviesData> dst=<$dstLeviesData>")
            // Merge all levies data, grouped by period and sorted from oldest to newest.
            // Then compute merged data for each period.
            val (mergedPeriodsData, debugs0) = loopData(levy, Nil, Nil,
              (dstLeviesData ::: srcLeviesData).groupBy(_.period).toList.sortBy(_._1.start).map(_._2))
            // Since periods may not be in order, re-sort them (from newest to oldest)
            val merged1 = merged.addPeriodsData(levy, mergedPeriodsData.sortBy(_.period.start).reverse).addWarnings(leviesPeriodsData.warnings)
            loopLevies(merged1, debugs0 ::: debugs, tail)

          case Nil =>
            (merged, debugs)
        }

      val (updatedAssetData0, debugs) = loopLevies(assetData, Nil, leviesPeriodsData.data.keys.toList)
      val debugInfo =
        if (!Settings.debug(Debug.LeviesHistory)) updatedAssetData0.debugInfo
        else DebugInfo(
          info = debugs,
          source = List(updatedAssetData0.debugInfo, leviesPeriodsData.debugInfo)
        )
      val updatedAssetData = updatedAssetData0.copy(debugInfo = debugInfo)

      if (Settings.debug(Debug.LeviesComputation))
        logger.info(s"action=<mergeLeviesPeriods> id=<${id.toString(this)}> src=<$leviesPeriodsData> dst=<$assetData> merged=<$updatedAssetData>")
      addLeviesPeriodsData(id, updatedAssetData)
    } else this
  }

  /**
   * Gets NAVs for given date.
   *
   * For each asset fund, read NAV from data store.
   * Nearest predating NAV is returned if there is none for requested date.
   *
   * @param owner parent window if any (to display error dialog if necessary)
   * @param date date to get NAVs on
   * @param eventsNAVs NAVs deduced from history events
   * @return NAVs
   */
  def getNAVs(owner: Option[Window], date: LocalDate,
    eventsNAVs: Map[UUID, Seq[AssetValue]] = Map.empty): Map[UUID, Savings.AssetValue] =
    assets.list.map(_.fundId).distinct.flatMap { fundId =>
      getNAV(owner, fundId, date, eventsNAVs).map(fundId -> _)
    }.toMap

}
