package epsa.model

import grizzled.slf4j.Logging
import java.time.LocalDate
import java.util.UUID
import spray.json._

object Savings {

  case class Scheme(id: UUID, name: String, funds: List[UUID])

  case class Fund(id: UUID, name: String)

  // TODO - fundId ok even if dealing with 'frozen current account' ?
  case class Asset(schemeId: UUID, fundId: UUID, availability: Option[LocalDate], amount: BigDecimal, units: BigDecimal)

  sealed trait Event

  case class CreateScheme(schemeId: UUID, name: String)
    extends Event

  case class UpdateScheme(schemeId: UUID, name: String)
    extends Event

  case class DeleteScheme(schemeId: UUID)
    extends Event

  case class CreateFund(fundId: UUID, name: String)
    extends Event

  case class UpdateFund(fundId: UUID, name: String)
    extends Event

  case class DeleteFund(fundId: UUID)
    extends Event

  case class AssociateFund(schemeId: UUID, fundId: UUID)
    extends Event

  case class DissociateFund(schemeId: UUID, fundId: UUID)
    extends Event

  case class MakePayment(date: LocalDate, asset: Asset)
    extends Event

  case class MakeTransfer(date: LocalDate, assetSrc: Asset, assetDst: Asset)
    extends Event

  case class MakeRefund(date: LocalDate, asset: Asset)
    extends Event

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
            case ex: Throwable => deserializationError(s"Invalid LocalData format: $dateS", ex)
          }

        case _ => deserializationError(s"Expected LocalData as JsString. Got $value")
      }
    }

    implicit val createSchemeFormat = jsonFormat2(CreateScheme)
    implicit val updateSchemeFormat = jsonFormat2(UpdateScheme)
    implicit val deleteSchemeFormat = jsonFormat1(DeleteScheme)
    implicit val createFundFormat = jsonFormat2(CreateFund)
    implicit val updateFundFormat = jsonFormat2(UpdateFund)
    implicit val deleteFundFormat = jsonFormat1(DeleteFund)
    implicit val associateFundFormat = jsonFormat2(AssociateFund)
    implicit val dissociateFundFormat = jsonFormat2(DissociateFund)
    implicit val assetFormat = jsonFormat5(Asset)
    implicit val makePaymentFormat = jsonFormat2(MakePayment)
    implicit val makeTransferFormat = jsonFormat3(MakeTransfer)
    implicit val makeRefundFormat = jsonFormat2(MakeRefund)

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

case class Savings(schemes: List[Savings.Scheme] = Nil, funds: List[Savings.Fund] = Nil, assets: List[Savings.Asset] = Nil) extends Logging {

  import Savings._

  def processActions(actions: (Savings => Savings.Event)*): Savings =
    actions.foldLeft(this) { (savings, action) =>
      val event = action(savings)
      savings.processEvent(event)
    }

  def processEvents(events: Savings.Event*): Savings =
    events.foldLeft(this) { (savings, event) =>
      savings.processEvent(event)
    }

  def processEvents(events: List[Savings.Event]): Savings =
    processEvents(events:_*)

  def processEvent(event: Event): Savings = event match {
    case CreateScheme(id, name)              => createScheme(id, name)
    case UpdateScheme(id, name)              => updateScheme(id, name)
    case DeleteScheme(id)                    => deleteScheme(id)
    case CreateFund(id, name)                => createFund(id, name)
    case UpdateFund(id, name)                => updateFund(id, name)
    case DeleteFund(id)                      => deleteFund(id)
    case AssociateFund(schemeId, fundId)     => associateFund(schemeId, fundId)
    case DissociateFund(schemeId, fundId)    => dissociateFund(schemeId, fundId)
    case MakePayment(_, asset)               => makePayment(asset)
    case MakeTransfer(_, assetSrc, assetDst) => makeTransfer(assetSrc, assetDst)
    case MakeRefund(_, asset)                => makeRefund(asset)
  }

  protected def createScheme(id: UUID, name: String): Savings = {
    copy(schemes = schemes :+ Scheme(id, name, Nil))
  }

  protected def updateScheme(id: UUID, name: String): Savings = {
    val updated = schemes.map { scheme =>
      if (scheme.id != id) scheme
      else scheme.copy(name = name)
    }
    copy(schemes = updated)
  }

  protected def deleteScheme(id: UUID): Savings = {
    copy(schemes = schemes.filterNot(_.id == id))
  }

  protected def createFund(id: UUID, name: String): Savings = {
    copy(funds = funds :+ Fund(id, name))
  }

  protected def updateFund(id: UUID, name: String): Savings = {
    val updated = funds.map { fund =>
      if (fund.id != id) fund
      else fund.copy(name = name)
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

  protected def makePayment(asset: Asset): Savings =
    findAsset(asset) match {
      case Some(currentAsset) =>
        val amount = currentAsset.amount + asset.amount
        val units = currentAsset.units + asset.units
        updateAsset(Asset(asset.schemeId, asset.fundId, asset.availability, amount, units))

      case None =>
        copy(assets = assets :+ asset)
    }

  protected def makeTransfer(assetSrc: Asset, assetDst: Asset): Savings =
    makeRefund(assetSrc).makePayment(assetDst)

  protected def makeRefund(asset: Asset): Savings = {
    val currentAsset = findAsset(asset).get
    val amount = currentAsset.amount - asset.amount
    val units = currentAsset.units - asset.units

    if (units <= 0) removeAsset(asset)
    else updateAsset(Asset(asset.schemeId, asset.fundId, asset.availability, amount, units))
  }

  protected def testAsset(currentAsset: Asset, asset: Asset): Boolean =
    (currentAsset.schemeId == asset.schemeId) &&
      (currentAsset.fundId == asset.fundId) &&
      (currentAsset.availability == asset.availability)

  protected def findAsset(asset: Asset): Option[Savings.Asset] =
    assets.find(testAsset(_, asset))

  protected def updateAsset(asset: Asset): Savings =
    copy(assets = assets.map { currentAsset =>
      if (testAsset(currentAsset, asset)) asset
      else currentAsset
    })

  protected def removeAsset(asset: Asset): Savings =
    copy(assets = assets.filterNot(testAsset(_, asset)))

  def findScheme(schemeId: UUID): Option[Scheme] =
    schemes.find(_.id == schemeId)

  def getScheme(schemeId: UUID): Scheme =
    findScheme(schemeId).get

  def findFund(fundId: UUID): Option[Fund] =
    funds.find(_.id == fundId)

  def getFund(fundId: UUID): Fund =
    findFund(fundId).get

  def hasAsset(schemeId: UUID, fundId: UUID): Boolean =
    assets.exists { asset =>
      (asset.schemeId == schemeId) &&
        (asset.fundId == fundId)
    }

  def createSchemeEvent(name: String): CreateScheme = {
    val id = newId(schemes.map(_.id))
    CreateScheme(id, name)
  }

  def createFundEvent(name: String): CreateFund = {
    val id = newId(funds.map(_.id))
    CreateFund(id, name)
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
      Savings.CreateScheme(scheme.id, scheme.name)
    } ::: fundsCreatedOrdered.map { fund =>
      Savings.CreateFund(fund.id, fund.name)
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
        if (newScheme.name == oldScheme.name) None
        else Some(Savings.UpdateScheme(newScheme.id, newScheme.name))

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
      if (newFund.name == oldFund.name) None
      else Some(Savings.UpdateFund(newFund.id, newFund.name))
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

}
