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
