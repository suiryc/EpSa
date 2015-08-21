package epsa.model

// XXX - use joda time
import java.util.Date
import java.util.UUID
import spray.json._

object Savings {

  def processActions(savings: Savings, actions: (Savings => Savings.Event)*): Savings =
    actions.foldLeft(savings) { (savings, action) =>
      val event = action(savings)
      savings.processEvent(event)
    }

  def processEvents(savings: Savings, events: Savings.Event*): Savings =
    events.foldLeft(savings) { (savings, event) =>
      savings.processEvent(event)
    }

  def processEvents(savings: Savings, events: List[Savings.Event]): Savings =
    processEvents(savings, events:_*)

  case class Scheme(id: UUID, name: String, funds: List[UUID])

  case class Fund(id: UUID, name: String)

  sealed trait Event

  // XXX - explicit fractional value instead of Double (possible precision issues) ?
  // XXX - fundId ok even if dealing with 'frozen current account' ?
  case class AssetQuantity(schemeId: UUID, fundId: UUID, amount: Double, units: Double, unitValue: Double)

  case class CreateScheme(schemeId: UUID, name: String)
    extends Event

  case class UpdateScheme(schemeId: UUID, name: String)
    extends Event

  case class CreateFund(fundId: UUID, name: String)
    extends Event

  case class UpdateFund(fundId: UUID, name: String)
    extends Event

  case class AssociateFund(schemeId: UUID, fundId: UUID)
    extends Event

  case class DissociateFund(schemeId: UUID, fundId: UUID)
    extends Event

  case class MakePayment(date: Date, assetQuantity: AssetQuantity, availability: Option[Date])
    extends Event

  case class MakeTransfer(date: Date, assetQuantitySrc: AssetQuantity, assetQuantityDst: AssetQuantity, availability: Option[Date])
    extends Event

  case class MakeRefund(date: Date, assetQuantity: AssetQuantity)
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

    implicit val createSchemeFormat = jsonFormat2(CreateScheme)
    implicit val updateSchemeFormat = jsonFormat2(UpdateScheme)
    implicit val createFundFormat = jsonFormat2(CreateFund)
    implicit val updateFundFormat = jsonFormat2(UpdateFund)
    implicit val associateFundFormat = jsonFormat2(AssociateFund)
    implicit val dissociateFundFormat = jsonFormat2(DissociateFund)

    implicit object EventJsonFormat extends RootJsonFormat[Event] with BasicFormats {

      val FIELD_KIND = "kind"
      val FIELD_CONTENT = "content"

      def write(event: Event): JsObject = event match {
        case event: CreateScheme   => JsObject(FIELD_CONTENT -> event.toJson, FIELD_KIND -> JsString("CreateScheme"))
        case event: UpdateScheme   => JsObject(FIELD_CONTENT -> event.toJson, FIELD_KIND -> JsString("UpdateScheme"))
        case event: CreateFund     => JsObject(FIELD_CONTENT -> event.toJson, FIELD_KIND -> JsString("CreateFund"))
        case event: UpdateFund     => JsObject(FIELD_CONTENT -> event.toJson, FIELD_KIND -> JsString("UpdateFund"))
        case event: AssociateFund  => JsObject(FIELD_CONTENT -> event.toJson, FIELD_KIND -> JsString("AssociateFund"))
        case event: DissociateFund => JsObject(FIELD_CONTENT -> event.toJson, FIELD_KIND -> JsString("DissociateFund"))

        case _ => serializationError(s"Unknown event kind: ${event.getClass.getName}")
      }

      def read(value: JsValue): Event = value.asJsObject.getFields(FIELD_CONTENT, FIELD_KIND) match {
        case Seq(content: JsObject, JsString(kind)) => kind match {
          case "CreateScheme"   => content.convertTo[CreateScheme]
          case "UpdateScheme"   => content.convertTo[UpdateScheme]
          case "CreateFund"     => content.convertTo[CreateFund]
          case "UpdateFund"     => content.convertTo[UpdateFund]
          case "AssociateFund"  => content.convertTo[AssociateFund]
          case "DissociateFund" => content.convertTo[DissociateFund]

          case _ => deserializationError(s"Unknown (JSON) event kind: $kind")
        }

        case fields => deserializationError(s"Event (JSON format) expected. Got $fields from $value")
      }

    }

  }

}

case class Savings(schemes: List[Savings.Scheme] = Nil, funds: List[Savings.Fund] = Nil) {

  import Savings._

  def processEvent(event: Event): Savings = event match {
    case CreateScheme(id, name)           => createScheme(id, name)
    case UpdateScheme(id, name)           => updateScheme(id, name)
    case CreateFund(id, name)             => createFund(id, name)
    case UpdateFund(id, name)             => updateFund(id, name)
    case AssociateFund(schemeId, fundId)  => associateFund(schemeId, fundId)
    case DissociateFund(schemeId, fundId) => dissociateFund(schemeId, fundId)

    case MakePayment(date, assetQuantity, availability) =>
      ???

    case MakeTransfer(date, assetQuantitySrc, assetQuantityDst, availability) =>
      ???

    case MakeRefund(date, assetQuantity) =>
      ???
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

  def getFund(fundId: UUID): Option[Fund] =
    funds.find(_.id == fundId)

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

}
