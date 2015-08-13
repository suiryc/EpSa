package epsa

import java.util.Date

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

  case class Scheme(id: String, name: String, funds: List[String])

  case class Fund(id: String, name: String)

  sealed trait Event

  // XXX - save list of schemes
  // XXX - save list of funds

  // XXX - explicit fractional value instead of Double (possible precision issues) ?
  // XXX - fundId ok even if dealing with 'frozen current account' ?
  case class AssetQuantity(schemeId: String, fundId: String, amount: Double, units: Double, unitValue: Double)

  case class CreateScheme(schemeId: String, name: String)
    extends Event

  case class CreateFund(fundId: String, name: String)
    extends Event

  case class AssociateFund(schemeId: String, fundId: String)
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

  //protected val schemes: List[Scheme] = Nil

  //protected val funds: List[Fund] = Nil

  def processEvent(event: Event): Savings = event match {
    case CreateScheme(id, name) =>
      copy(schemes = schemes :+ Scheme(id, name, Nil))

    case CreateFund(id, name) =>
      copy(funds = funds :+ Fund(id, name))

    case AssociateFund(schemeId, fundId) =>
      val updated = schemes.map { scheme =>
        if (scheme.id == schemeId) {
          scheme.copy(funds = scheme.funds :+ fundId)
        } else {
          scheme
        }
      }
      copy(schemes = updated)

    case MakePayment(date, assetQuantity, availability) =>
      ???

    case MakeTransfer(date, assetQuantitySrc, assetQuantityDst, availability) =>
      ???

    case MakeRefund(date, assetQuantity) =>
      ???
  }

  def getFund(fundId: String): Option[Fund] =
    funds.find(_.id == fundId)

  def createScheme(name: String): CreateScheme = {
    // XXX - not good if we can remove entries
    val id = f"${schemes.length + 1}%08x"
    CreateScheme(id, name)
  }

  def createFund(name: String): CreateFund = {
    // XXX - not good if we can remove entries
    val id = f"${funds.length + 1}%08x"
    CreateFund(id, name)
  }

  def associateFund(schemeId: String, fundId: String): AssociateFund =
    AssociateFund(schemeId, fundId)

}
