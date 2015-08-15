package epsa.model

import java.util.Date
import java.util.UUID

object Savings {

  def processActions(savings: Savings, action: (Savings => Savings.Event)*): Savings =
    action.foldLeft(savings) { (savings, action) =>
      val event = action(savings)
      savings.processEvent(event)
    }

  def processEvents(savings: Savings, event: Savings.Event*): Savings =
    event.foldLeft(savings) { (savings, event) =>
      savings.processEvent(event)
    }

  case class Scheme(id: UUID, name: String, funds: List[UUID])

  case class Fund(id: UUID, name: String)

  sealed trait Event

  // XXX - save list of schemes
  // XXX - save list of funds

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
}

case class Savings(schemes: List[Savings.Scheme] = Nil, funds: List[Savings.Fund] = Nil) {

  import Savings._

  def processEvent(event: Event): Savings = event match {
    case CreateScheme(id, name) =>
      copy(schemes = schemes :+ Scheme(id, name, Nil))

    case UpdateScheme(id, name) =>
      val updated = schemes.map { scheme =>
        if (scheme.id != id) scheme
        else scheme.copy(name = name)
      }
      copy(schemes = updated)

    case CreateFund(id, name) =>
      copy(funds = funds :+ Fund(id, name))

    case UpdateFund(id, name) =>
      val updated = funds.map { fund =>
        if (fund.id != id) fund
        else fund.copy(name = name)
      }
      copy(funds = updated)

    case AssociateFund(schemeId, fundId) =>
      val updated = schemes.map { scheme =>
        if (scheme.id != schemeId) scheme
        else scheme.copy(funds = scheme.funds :+ fundId)
      }
      copy(schemes = updated)

    case DissociateFund(schemeId, fundId) =>
      val updated = schemes.map { scheme =>
        if (scheme.id != schemeId) scheme
        else scheme.copy(funds = scheme.funds.filterNot(_ == fundId))
      }
      copy(schemes = updated)

    case MakePayment(date, assetQuantity, availability) =>
      ???

    case MakeTransfer(date, assetQuantitySrc, assetQuantityDst, availability) =>
      ???

    case MakeRefund(date, assetQuantity) =>
      ???
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

  @scala.annotation.tailrec
  private def newId(existing: List[UUID]): UUID = {
    val id = UUID.randomUUID()
    if (!existing.contains(id)) id
    else newId(existing)
  }

}
