package epsa.model

import java.time.LocalDate
import org.scalatest.{Matchers, WordSpec}

class SavingsSpec extends WordSpec with Matchers {

  private val savings0 = Savings()

  private val savings1 = savings0.processActions(
    _.createSchemeEvent("scheme 1"),
    _.createFundEvent("fund 1")
  )

  private val savings2 = savings1.processActions(
    _.createFundEvent("fund 2"),
    { s => Savings.AssociateFund(s.schemes.head.id, s.funds.head.id) },
    { s => Savings.AssociateFund(s.schemes.head.id, s.funds(1).id) }
  )

  "Savings" should {
    "have an empty ctor" in {
      savings0.schemes shouldBe empty
      savings0.funds shouldBe empty
      savings0.assets shouldBe empty
      savings0.latestAssetAction shouldBe empty
    }

    "handle adding a scheme" in {
      val schemeName = "scheme 1"
      val event = savings0.createSchemeEvent(schemeName)
      event.name shouldBe schemeName

      val savings = savings0.processEvent(event)
      savings.schemes.size shouldBe 1
      savings.funds shouldBe empty
      savings.assets shouldBe empty
      savings.latestAssetAction shouldBe empty

      val scheme = savings.schemes.head
      scheme.name shouldBe schemeName
      scheme.funds shouldBe empty
    }

    "handle adding a scheme with comment" in {
      val schemeName = "scheme 1"
      val schemeComment = Some("some comment")
      val event = savings0.createSchemeEvent(schemeName, schemeComment)
      event.name shouldBe schemeName
      event.comment shouldBe schemeComment

      val savings = savings0.processEvent(event)
      val scheme = savings.schemes.head
      scheme.name shouldBe schemeName
      scheme.comment shouldBe schemeComment
      scheme.funds shouldBe empty
    }

    "handle updating a scheme name" in {
      val scheme0 = savings1.schemes.head
      val schemeName = scheme0.name + "-new"
      val savings = savings1.processEvent(Savings.UpdateScheme(scheme0.id, schemeName, scheme0.comment))
      val scheme = savings.schemes.head
      scheme shouldBe scheme0.copy(name = schemeName)
    }

    "handle updating a scheme comment" in {
      val scheme0 = savings1.schemes.head
      val schemeComment = Some("New comment")
      val savings = savings1.processEvent(Savings.UpdateScheme(scheme0.id, scheme0.name, schemeComment))
      val scheme = savings.schemes.head
      scheme shouldBe scheme0.copy(comment = schemeComment)
    }

    "handle deleting a scheme" in {
      val scheme = savings1.schemes.head
      val savings = savings1.processEvent(Savings.DeleteScheme(scheme.id))
      savings.schemes shouldBe empty
      savings.funds.size shouldBe 1
    }

    "handle adding a fund" in {
      val fundName = "fund 1"
      val event = savings0.createFundEvent(fundName)
      event.name shouldBe fundName

      val savings = savings0.processEvent(event)
      savings.schemes shouldBe empty
      savings.funds.size shouldBe 1
      savings.assets shouldBe empty
      savings.latestAssetAction shouldBe empty

      val fund = savings.funds.head
      fund.name shouldBe fundName
    }

    "handle adding a fund with comment" in {
      val fundName = "fund 1"
      val fundComment = Some("some comment")
      val event = savings0.createFundEvent(fundName, fundComment)
      event.name shouldBe fundName
      event.comment shouldBe fundComment

      val savings = savings0.processEvent(event)
      val fund = savings.funds.head
      fund.name shouldBe fundName
      fund.comment shouldBe fundComment
    }

    "handle updating a fund name" in {
      val fund0 = savings1.funds.head
      val fundName = fund0.name + "-new"
      val savings = savings1.processEvent(Savings.UpdateFund(fund0.id, fundName, fund0.comment))
      val fund = savings.funds.head
      fund shouldBe fund0.copy(name = fundName)
    }

    "handle updating a fund comment" in {
      val fund0 = savings1.funds.head
      val fundComment = Some("New comment")
      val savings = savings1.processEvent(Savings.UpdateFund(fund0.id, fund0.name, fundComment))
      val fund = savings.funds.head
      fund shouldBe fund0.copy(comment = fundComment)
    }

    "handle deleting a fund" in {
      val fund = savings1.funds.head
      val savings = savings1.processEvent(Savings.DeleteFund(fund.id))
      savings.funds shouldBe empty
      savings.schemes.size shouldBe 1
    }

    "handle associating a fund to a scheme" in {
      val scheme0 = savings1.schemes.head
      val fund = savings1.funds.head
      val savings = savings1.processEvent(Savings.AssociateFund(scheme0.id, fund.id))
      savings.schemes.size shouldBe 1
      savings.funds.size shouldBe 1
      savings.schemes.head shouldBe scheme0.copy(funds = List(fund.id))
    }


    "handle dissociating a fund from a scheme" in {
      val scheme0 = savings1.schemes.head
      val fund = savings1.funds.head
      val savings1_2 = savings1.processEvent(Savings.AssociateFund(scheme0.id, fund.id))
      savings1_2.schemes.head shouldBe scheme0.copy(funds = List(fund.id))
      val savings1_3 = savings1_2.processEvent(Savings.DissociateFund(scheme0.id, fund.id))
      savings1_3 shouldBe savings1
    }

    "handle making payments" in {
      val scheme = savings2.schemes.head
      val fund = savings2.funds.head
      val date = LocalDate.now.minusDays(10)
      val savings = savings2.processEvents(
        Savings.MakePayment(date.plusDays(1), Savings.AssetPart(scheme.id, fund.id, None, BigDecimal(1), BigDecimal(1))),
        Savings.MakePayment(date.plusDays(2), Savings.AssetPart(scheme.id, fund.id, Some(date.plusDays(10)), BigDecimal(2), BigDecimal(2))),
        Savings.MakePayment(date.plusDays(3), Savings.AssetPart(scheme.id, fund.id, Some(date.plusDays(10)), BigDecimal(3), BigDecimal(3))),
        Savings.MakePayment(date.plusDays(4), Savings.AssetPart(scheme.id, fund.id, Some(date.plusDays(11)), BigDecimal(4), BigDecimal(4))),
        Savings.MakePayment(date.plusDays(5), Savings.AssetPart(scheme.id, fund.id, None, BigDecimal(5), BigDecimal(5)))
      )
      savings.latestAssetAction shouldBe Some(date.plusDays(5))
      checkSavings(date, savings,
        Savings.Asset(scheme.id, fund.id, None, BigDecimal(6), 0),
        Savings.Asset(scheme.id, fund.id, Some(date.plusDays(10)), BigDecimal(5), 0),
        Savings.Asset(scheme.id, fund.id, Some(date.plusDays(11)), BigDecimal(4), 0)
      )
    }

    "handle making refunds" in {
      val scheme = savings2.schemes.head
      val fund = savings2.funds.head
      val date = LocalDate.now.minusDays(10)
      val savings = savings2.processEvents(
        Savings.MakePayment(date.plusDays(1), Savings.AssetPart(scheme.id, fund.id, None, BigDecimal(10), BigDecimal(10))),
        Savings.MakePayment(date.plusDays(2), Savings.AssetPart(scheme.id, fund.id, Some(date.plusDays(10)), BigDecimal(10), BigDecimal(10))),
        Savings.MakePayment(date.plusDays(3), Savings.AssetPart(scheme.id, fund.id, Some(date.plusDays(11)), BigDecimal(10), BigDecimal(10))),
        Savings.MakeRefund(date.plusDays(4), Savings.AssetPart(scheme.id, fund.id, None, BigDecimal(1), BigDecimal(1))),
        Savings.MakeRefund(date.plusDays(5), Savings.AssetPart(scheme.id, fund.id, Some(date.plusDays(10)), BigDecimal(10), BigDecimal(10))),
        Savings.MakeRefund(date.plusDays(6), Savings.AssetPart(scheme.id, fund.id, Some(date.plusDays(11)), BigDecimal(2), BigDecimal(2)))
      )
      savings.latestAssetAction shouldBe Some(date.plusDays(6))
      checkSavings(date, savings,
        Savings.Asset(scheme.id, fund.id, None, BigDecimal(9), 0),
        Savings.Asset(scheme.id, fund.id, Some(date.plusDays(11)), BigDecimal(8), 0)
      )
    }

    "handle making transfers" in {
      val scheme = savings2.schemes.head
      val fund1 = savings2.funds.head
      val fund2 = savings2.funds(1)
      val date = LocalDate.now.minusDays(10)
      val savings = savings2.processEvents(
        Savings.MakePayment(date.plusDays(1), Savings.AssetPart(scheme.id, fund1.id, None, BigDecimal(10), BigDecimal(10))),
        Savings.MakePayment(date.plusDays(2), Savings.AssetPart(scheme.id, fund1.id, Some(date.plusDays(10)), BigDecimal(10), BigDecimal(10))),
        Savings.MakeTransfer(date.plusDays(3), Savings.AssetPart(scheme.id, fund1.id, None, BigDecimal(1), BigDecimal(10)),
          Savings.AssetPart(scheme.id, fund2.id, None, BigDecimal(2), BigDecimal(5))),
        Savings.MakeTransfer(date.plusDays(4), Savings.AssetPart(scheme.id, fund1.id, None, BigDecimal(2), BigDecimal(10)),
          Savings.AssetPart(scheme.id, fund2.id, None, BigDecimal(10), BigDecimal(2))),
        Savings.MakeTransfer(date.plusDays(5), Savings.AssetPart(scheme.id, fund1.id, Some(date.plusDays(10)), BigDecimal(3), BigDecimal(10)),
          Savings.AssetPart(scheme.id, fund2.id, Some(date.plusDays(10)), BigDecimal(10), BigDecimal(3)))
      )
      savings.latestAssetAction shouldBe Some(date.plusDays(5))
      checkSavings(date, savings,
        Savings.Asset(scheme.id, fund1.id, None, BigDecimal(7), 0),
        Savings.Asset(scheme.id, fund1.id, Some(date.plusDays(10)), BigDecimal(7), 0),
        Savings.Asset(scheme.id, fund2.id, None, BigDecimal(12), 0),
        Savings.Asset(scheme.id, fund2.id, Some(date.plusDays(10)), BigDecimal(10), 0)
      )
    }

    "handle resolving assets availability by date" in {
      val scheme = savings2.schemes.head
      val fund = savings2.funds.head
      val date = LocalDate.now.minusDays(10)
      val savings = savings2.processEvents(
        Savings.MakePayment(date.plusDays(1), Savings.AssetPart(scheme.id, fund.id, None, BigDecimal(1), BigDecimal(1))),
        Savings.MakePayment(date.plusDays(2), Savings.AssetPart(scheme.id, fund.id, Some(date.plusDays(10)), BigDecimal(2), BigDecimal(2))),
        Savings.MakePayment(date.plusDays(3), Savings.AssetPart(scheme.id, fund.id, Some(date.plusDays(11)), BigDecimal(3), BigDecimal(3)))
      )
      checkSavings(date, savings,
        Savings.Asset(scheme.id, fund.id, None, BigDecimal(1), 0),
        Savings.Asset(scheme.id, fund.id, Some(date.plusDays(10)), BigDecimal(2), 0),
        Savings.Asset(scheme.id, fund.id, Some(date.plusDays(11)), BigDecimal(3), 0)
      )
      checkSavings(date.plusDays(9), savings,
        Savings.Asset(scheme.id, fund.id, None, BigDecimal(1), 0),
        Savings.Asset(scheme.id, fund.id, Some(date.plusDays(10)), BigDecimal(2), 0),
        Savings.Asset(scheme.id, fund.id, Some(date.plusDays(11)), BigDecimal(3), 0)
      )
      checkSavings(date.plusDays(10), savings,
        Savings.Asset(scheme.id, fund.id, None, BigDecimal(3), 0),
        Savings.Asset(scheme.id, fund.id, Some(date.plusDays(11)), BigDecimal(3), 0)
      )
      checkSavings(date.plusDays(11), savings,
        Savings.Asset(scheme.id, fund.id, None, BigDecimal(6), 0)
      )
    }

    "handle flattening events" in {
      val scheme = savings2.schemes.head
      val fund1 = savings2.funds.head
      val fund2 = savings2.funds(1)
      val createFund3 = savings2.createFundEvent("fund 3")
      val createScheme2 = savings2.createSchemeEvent("scheme 2")

      // Simple events should remain
      checkFlattening(savings2, createScheme2)
      checkFlattening(savings2, Savings.UpdateScheme(scheme.id, scheme.name + "-new", scheme.comment))
      checkFlattening(savings2, Savings.UpdateScheme(scheme.id, scheme.name, Some("New comment")))
      checkFlattening(savings2, createFund3)
      checkFlattening(savings2, Savings.UpdateFund(fund1.id, fund1.name + "-new", fund1.comment))
      checkFlattening(savings2, Savings.UpdateFund(fund1.id, fund1.name, Some("New comment")))
      // Note: this case actually triggers a warning because flattening first
      // dissociates fund (which is right) and thus resulting scheme differs
      // because *we* did not do it. In this case flattening keep the provided
      // events as-is.
      checkFlattening(savings2, Savings.DeleteFund(fund1.id))
      checkFlattening(savings2, Savings.AssociateFund(scheme.id, createFund3.fundId))
      checkFlattening(savings2, Savings.DissociateFund(scheme.id, fund1.id))

      // Scheme deletion should trigger funds dissociation (because *we* did
      // not do it).
      checkFlattening(savings2,
        List(Savings.DeleteScheme(scheme.id)),
        List(Savings.DissociateFund(scheme.id, fund1.id), Savings.DissociateFund(scheme.id, fund2.id), Savings.DeleteScheme(scheme.id))
      )

      // Events cancelling each other should disappear
      checkFlattening(savings2,
        List(createScheme2, Savings.DeleteScheme(createScheme2.schemeId)),
        Nil
      )
      checkFlattening(savings2,
        List(Savings.UpdateScheme(scheme.id, scheme.name + "-new", scheme.comment), Savings.UpdateScheme(scheme.id, scheme.name, scheme.comment)),
        Nil
      )
      checkFlattening(savings2,
        List(Savings.UpdateScheme(scheme.id, scheme.name, Some("New comment")), Savings.UpdateScheme(scheme.id, scheme.name, scheme.comment)),
        Nil
      )
      checkFlattening(savings2,
        List(createFund3, Savings.DeleteFund(createFund3.fundId)),
        Nil
      )
      checkFlattening(savings2,
        List(Savings.UpdateFund(fund1.id, fund1.name + "-new", scheme.comment), Savings.UpdateFund(fund1.id, fund1.name, scheme.comment)),
        Nil
      )
      checkFlattening(savings2,
        List(Savings.UpdateFund(fund1.id, fund1.name, Some("New comment")), Savings.UpdateFund(fund1.id, fund1.name, scheme.comment)),
        Nil
      )
      checkFlattening(savings2,
        List(Savings.AssociateFund(scheme.id, createFund3.fundId), Savings.DissociateFund(scheme.id, createFund3.fundId)),
        Nil
      )
    }
  }

  "Savings (de)serialization" should {
    "handle CreateScheme" in {
      checkEventSerialization(savings0.createSchemeEvent("scheme name"))
    }

    "handle UpdateScheme" in {
      checkEventSerialization(Savings.UpdateScheme(savings1.schemes.head.id, "scheme new name", None))
      checkEventSerialization(Savings.UpdateScheme(savings1.schemes.head.id, "scheme new name", Some("scheme new comment")))
    }

    "handle DeleteScheme" in {
      checkEventSerialization(Savings.DeleteScheme(savings1.schemes.head.id))
    }

    "handle CreateFund" in {
      checkEventSerialization(savings0.createFundEvent("fund name"))
    }

    "handle UpdateFund" in {
      checkEventSerialization(Savings.UpdateFund(savings1.funds.head.id, "fund new name", None))
      checkEventSerialization(Savings.UpdateFund(savings1.funds.head.id, "fund new name", Some("fund new comment")))
    }

    "handle DeleteFund" in {
      checkEventSerialization(Savings.DeleteFund(savings1.funds.head.id))
    }

    "handle AssociateFund" in {
      checkEventSerialization(Savings.AssociateFund(savings1.schemes.head.id, savings1.funds.head.id))
    }

    "handle DissociateFund" in {
      checkEventSerialization(Savings.DissociateFund(savings1.schemes.head.id, savings1.funds.head.id))
    }

    "handle MakePayment" in {
      checkEventSerialization(Savings.MakePayment(LocalDate.now,
        Savings.AssetPart(savings1.schemes.head.id, savings1.funds.head.id, Some(LocalDate.now), BigDecimal(10), BigDecimal(10))))
    }

    "handle MakeRefund" in {
      checkEventSerialization(Savings.MakeRefund(LocalDate.now,
        Savings.AssetPart(savings1.schemes.head.id, savings1.funds.head.id, Some(LocalDate.now), BigDecimal(10), BigDecimal(10))))
    }

    "handle MakeTransfer" in {
      val scheme = savings1.schemes.head
      val fund = savings1.funds.head
      checkEventSerialization(Savings.MakeTransfer(LocalDate.now,
        Savings.AssetPart(scheme.id, fund.id, Some(LocalDate.now), BigDecimal(10), BigDecimal(10)),
        Savings.AssetPart(fund.id, scheme.id, None, BigDecimal(1), BigDecimal(1))))
    }
  }

  "Savings asset" should {
    "handle VWAP" in {
      val scheme = savings2.schemes.head
      val fund1 = savings2.funds.head
      val fund2 = savings2.funds(1)
      val date = LocalDate.now.minusDays(10)

      // Invest 1 unit at price 6; VWAP = 1*6 / 1 = 6
      val savings2_1 = savings2.processEvent(
        Savings.MakePayment(date.plusDays(1), Savings.AssetPart(scheme.id, fund1.id, None, BigDecimal(1), BigDecimal(6)))
      )
      checkSavings(date, savings2_1,
        Savings.Asset(scheme.id, fund1.id, None, BigDecimal(1), BigDecimal(6))
      )

      // Invest 2 units at price 3; VWAP = (1*6 + 2*3) / (1 + 2) = 4
      val savings2_2 = savings2_1.processEvent(
        Savings.MakePayment(date.plusDays(1), Savings.AssetPart(scheme.id, fund1.id, None, BigDecimal(2), BigDecimal(3)))
      )
      checkSavings(date, savings2_2,
        Savings.Asset(scheme.id, fund1.id, None, BigDecimal(3), BigDecimal(4))
      )

      // VWAP remains unchanged upon refund
      val savings2_3 = savings2_2.processEvent(
        Savings.MakeRefund(date.plusDays(1), Savings.AssetPart(scheme.id, fund1.id, None, BigDecimal(1), BigDecimal(100)))
      )
      checkSavings(date, savings2_3,
        Savings.Asset(scheme.id, fund1.id, None, BigDecimal(2), BigDecimal(4))
      )

      // Invest 2 units at price 2; (once all available) VWAP = (2*4 + 2*2) / (2 + 2) = 3
      val savings2_4 = savings2_3.processEvent(
        Savings.MakePayment(date.plusDays(1), Savings.AssetPart(scheme.id, fund1.id, Some(date.plusDays(10)), BigDecimal(2), BigDecimal(2)))
      )
      checkSavings(date, savings2_4,
        Savings.Asset(scheme.id, fund1.id, None, BigDecimal(2), BigDecimal(4)),
        Savings.Asset(scheme.id, fund1.id, Some(date.plusDays(10)), BigDecimal(2), BigDecimal(2))
      )
      checkSavings(date.plusDays(11), savings2_4,
        Savings.Asset(scheme.id, fund1.id, None, BigDecimal(4), BigDecimal(3))
      )

      // Transfer 2 units at price 2 (with VWAP = 4) to 4 units at price 1; dstVWAP = (2 * 4) / 4 = 2
      val savings2_5 = savings2_4.processEvent(
        Savings.MakeTransfer(date.plusDays(1), Savings.AssetPart(scheme.id, fund1.id, None, BigDecimal(2), BigDecimal(2)),
          Savings.AssetPart(scheme.id, fund2.id, None, BigDecimal(4), BigDecimal(1)))
      )
      checkSavings(date, savings2_5,
        Savings.Asset(scheme.id, fund1.id, Some(date.plusDays(10)), BigDecimal(2), BigDecimal(2)),
        Savings.Asset(scheme.id, fund2.id, None, BigDecimal(4), BigDecimal(2))
      )

      // Transfer 2 units at price 4 (with VWAP = 2) to 8 units at price 1; dstVWAP = (4 * 2 + 2 * 2) / (4 + 8) = 1
      val savings2_6 = savings2_5.processEvent(
        Savings.MakeTransfer(date.plusDays(1), Savings.AssetPart(scheme.id, fund1.id, Some(date.plusDays(10)), BigDecimal(2), BigDecimal(4)),
          Savings.AssetPart(scheme.id, fund2.id, None, BigDecimal(8), BigDecimal(1)))
      )
      checkSavings(date, savings2_6,
        Savings.Asset(scheme.id, fund2.id, None, BigDecimal(12), BigDecimal(1))
      )
    }
  }

  private def checkFlattening(savings: Savings, event: Savings.Event): Unit =
    savings.flattenEvents(List(event)) shouldBe List(event)

  private def checkFlattening(savings: Savings, process: List[Savings.Event], expected: List[Savings.Event]): Unit =
    savings.flattenEvents(process) shouldBe expected

  private def checkAsset(date: LocalDate, savings: Savings, expectedAsset: Savings.Asset): Unit = {
    val foundAssetOpt = savings.findAsset(date, expectedAsset)
    foundAssetOpt should not be empty
    val foundAsset0 = foundAssetOpt.get
    val foundAsset =
      if (expectedAsset.vwap != 0) foundAsset0
      else foundAsset0.copy(vwap = 0)
    foundAsset shouldBe expectedAsset
  }

  private def checkSavings(date: LocalDate, savings0: Savings, expectedAssets: Savings.Asset*): Unit = {
    val savings = savings0.computeAssets(date)
    withClue(s"Expecting ${expectedAssets.toList} for $savings:\n") {
      savings.assets.size shouldBe expectedAssets.size
      expectedAssets.foreach { expectedAsset =>
        checkAsset(date, savings, expectedAsset)
      }
    }
  }

  private def checkEventSerialization(event: Savings.Event): Unit = {
    val format = Savings.JsonProtocol.EventJsonFormat
    format.read(format.write(event)) shouldBe event
  }

}
