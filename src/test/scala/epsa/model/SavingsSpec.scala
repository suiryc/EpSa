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
      savings0.assets.list shouldBe empty
      savings0.assets.byId shouldBe empty
      savings0.assets.vwaps shouldBe empty
      savings0.latestAssetAction shouldBe empty
    }

    "handle adding a scheme" in {
      val schemeName = "scheme 1"
      val event = savings0.createSchemeEvent(schemeName)
      event.name shouldBe schemeName

      val savings = savings0.processEvent(event)
      savings.schemes.size shouldBe 1
      savings.funds shouldBe empty
      savings.assets.list shouldBe empty
      savings.assets.byId shouldBe empty
      savings.assets.vwaps shouldBe empty
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
      val scheme = savings1.schemes.head
      val schemeName = scheme.name + "-new"
      val savings = savings1.processEvent(Savings.UpdateScheme(scheme).copy(name = schemeName))
      savings.schemes.head shouldBe scheme.copy(name = schemeName)
    }

    "handle updating a scheme comment" in {
      val scheme = savings1.schemes.head
      val schemeComment = Some("New comment")
      val savings = savings1.processEvent(Savings.UpdateScheme(scheme).copy(comment = schemeComment))
      savings.schemes.head shouldBe scheme.copy(comment = schemeComment)
    }

    "handle disabling a scheme" in {
      val scheme = savings1.schemes.head
      val savings = savings1.processEvent(Savings.UpdateScheme(scheme).copy(disabled = true))
      savings.schemes.head shouldBe scheme.copy(disabled = true)
    }

    "handle enabling a scheme" in {
      val scheme = savings1.schemes.head
      val savings = savings1.processEvents(
        Savings.UpdateScheme(scheme).copy(disabled = true),
        Savings.UpdateScheme(scheme).copy(disabled = false)
      )
      savings.schemes.head shouldBe scheme
    }

    "prevent disabling an active scheme" in {
      val scheme = savings2.schemes.head
      val fund = savings2.funds.head
      val savings = savings2.processEvents(
        Savings.MakePayment(LocalDate.now.minusDays(10), Savings.AssetPart(scheme.id, fund.id, None, BigDecimal(1), BigDecimal(1)), None),
        Savings.UpdateScheme(scheme).copy(disabled = true)
      )
      savings.schemes.head shouldBe scheme.copy(used = true, active = true)
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
      savings.assets.list shouldBe empty
      savings.assets.byId shouldBe empty
      savings.assets.vwaps shouldBe empty
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
      val fund = savings1.funds.head
      val fundName = fund.name + "-new"
      val savings = savings1.processEvent(Savings.UpdateFund(fund).copy(name = fundName))
      savings.funds.head shouldBe fund.copy(name = fundName)
    }

    "handle updating a fund comment" in {
      val fund = savings1.funds.head
      val fundComment = Some("New comment")
      val savings = savings1.processEvent(Savings.UpdateFund(fund).copy(comment = fundComment))
      savings.funds.head shouldBe fund.copy(comment = fundComment)
    }

    "handle disabling a fund" in {
      val fund = savings1.funds.head
      val savings = savings1.processEvent(Savings.UpdateFund(fund).copy(disabled = true))
      savings.funds.head shouldBe fund.copy(disabled = true)
    }

    "handle enabling a fund" in {
      val fund = savings1.funds.head
      val savings = savings1.processEvents(
        Savings.UpdateFund(fund).copy(disabled = true),
        Savings.UpdateFund(fund).copy(disabled = false)
      )
      savings.funds.head shouldBe fund
    }

    "prevent disabling an active fund" in {
      val scheme = savings2.schemes.head
      val fund = savings2.funds.head
      val savings = savings2.processEvents(
        Savings.MakePayment(LocalDate.now.minusDays(10), Savings.AssetPart(scheme.id, fund.id, None, BigDecimal(1), BigDecimal(1)), None),
        Savings.UpdateFund(fund).copy(disabled = true)
      )
      savings.funds.head shouldBe fund.copy(used = true, active = true)
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
        Savings.MakePayment(date.plusDays(1), Savings.AssetPart(scheme.id, fund.id, None, BigDecimal(1), BigDecimal(1)), None),
        Savings.MakePayment(date.plusDays(2), Savings.AssetPart(scheme.id, fund.id, Some(date.plusDays(10)), BigDecimal(2), BigDecimal(2)), None),
        Savings.MakePayment(date.plusDays(3), Savings.AssetPart(scheme.id, fund.id, Some(date.plusDays(10)), BigDecimal(3), BigDecimal(3)), None),
        Savings.MakePayment(date.plusDays(4), Savings.AssetPart(scheme.id, fund.id, Some(date.plusDays(11)), BigDecimal(4), BigDecimal(4)), None),
        Savings.MakePayment(date.plusDays(5), Savings.AssetPart(scheme.id, fund.id, None, BigDecimal(5), BigDecimal(5)), None)
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
        Savings.MakePayment(date.plusDays(1), Savings.AssetPart(scheme.id, fund.id, None, BigDecimal(10), BigDecimal(10)), None),
        Savings.MakePayment(date.plusDays(2), Savings.AssetPart(scheme.id, fund.id, Some(date.plusDays(10)), BigDecimal(10), BigDecimal(10)), None),
        Savings.MakePayment(date.plusDays(3), Savings.AssetPart(scheme.id, fund.id, Some(date.plusDays(11)), BigDecimal(10), BigDecimal(10)), None),
        Savings.MakeRefund(date.plusDays(4), Savings.AssetPart(scheme.id, fund.id, None, BigDecimal(1), BigDecimal(1)), None),
        Savings.MakeRefund(date.plusDays(5), Savings.AssetPart(scheme.id, fund.id, Some(date.plusDays(10)), BigDecimal(10), BigDecimal(10)), None),
        Savings.MakeRefund(date.plusDays(6), Savings.AssetPart(scheme.id, fund.id, Some(date.plusDays(11)), BigDecimal(2), BigDecimal(2)), None)
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
        Savings.MakePayment(date.plusDays(1), Savings.AssetPart(scheme.id, fund1.id, None, BigDecimal(10), BigDecimal(10)), None),
        Savings.MakePayment(date.plusDays(2), Savings.AssetPart(scheme.id, fund1.id, Some(date.plusDays(10)), BigDecimal(10), BigDecimal(10)), None),
        Savings.MakeTransfer(date.plusDays(3), Savings.AssetPart(scheme.id, fund1.id, None, BigDecimal(1), BigDecimal(10)),
          Savings.AssetPart(scheme.id, fund2.id, None, BigDecimal(2), BigDecimal(5)), None),
        Savings.MakeTransfer(date.plusDays(4), Savings.AssetPart(scheme.id, fund1.id, None, BigDecimal(2), BigDecimal(10)),
          Savings.AssetPart(scheme.id, fund2.id, None, BigDecimal(10), BigDecimal(2)), None),
        Savings.MakeTransfer(date.plusDays(5), Savings.AssetPart(scheme.id, fund1.id, Some(date.plusDays(10)), BigDecimal(3), BigDecimal(10)),
          Savings.AssetPart(scheme.id, fund2.id, Some(date.plusDays(10)), BigDecimal(10), BigDecimal(3)), None)
      )
      savings.latestAssetAction shouldBe Some(date.plusDays(5))
      checkSavings(date, savings,
        Savings.Asset(scheme.id, fund1.id, None, BigDecimal(7), 0),
        Savings.Asset(scheme.id, fund1.id, Some(date.plusDays(10)), BigDecimal(7), 0),
        Savings.Asset(scheme.id, fund2.id, None, BigDecimal(12), 0),
        Savings.Asset(scheme.id, fund2.id, Some(date.plusDays(10)), BigDecimal(10), 0)
      )
    }

    "handle assets list and vwaps populating" in {
      val scheme = savings2.schemes.head
      val fund1 = savings2.funds.head
      val fund2 = savings2.funds(1)
      val date = LocalDate.now.minusDays(10)

      // Create 2 assets entries
      val savings2_1 = savings2.processEvents(
        Savings.MakePayment(date.plusDays(1), Savings.AssetPart(scheme.id, fund1.id, None, BigDecimal(1), BigDecimal(1)), None),
        Savings.MakePayment(date.plusDays(2), Savings.AssetPart(scheme.id, fund1.id, Some(date.plusDays(10)), BigDecimal(1), BigDecimal(1)), None),
        Savings.MakePayment(date.plusDays(2), Savings.AssetPart(scheme.id, fund1.id, None, BigDecimal(1), BigDecimal(1)), None)
      )
      savings2_1.assets.list.size shouldBe 2
      savings2_1.assets.byId.size shouldBe 1
      savings2_1.assets.vwaps.size shouldBe 1

      // Create a 3rd entry by transferring part of an existing one
      val savings2_2 = savings2_1.processEvents(
        Savings.MakeTransfer(date.plusDays(3), Savings.AssetPart(scheme.id, fund1.id, None, BigDecimal(1), BigDecimal(1)),
          Savings.AssetPart(scheme.id, fund2.id, None, BigDecimal(1), BigDecimal(1)), None)
      )
      savings2_2.assets.list.size shouldBe 3
      savings2_2.assets.byId.size shouldBe 2
      savings2_2.assets.vwaps.size shouldBe 2

      // Empty one entry by transferring to an existing one, keeping 2 different ids
      val savings2_3 = savings2_2.processEvents(
        Savings.MakeTransfer(date.plusDays(4), Savings.AssetPart(scheme.id, fund1.id, None, BigDecimal(1), BigDecimal(1)),
          Savings.AssetPart(scheme.id, fund2.id, None, BigDecimal(1), BigDecimal(1)), None)
      )
      savings2_3.assets.list.size shouldBe 2
      savings2_3.assets.byId.size shouldBe 2
      savings2_3.assets.vwaps.size shouldBe 2

      // Then empty account
      val savings2_4 = savings2_3.processEvents(
        Savings.MakeRefund(date.plusDays(6), Savings.AssetPart(scheme.id, fund1.id, Some(date.plusDays(10)), BigDecimal(1), BigDecimal(1)), None),
        Savings.MakeRefund(date.plusDays(6), Savings.AssetPart(scheme.id, fund2.id, None, BigDecimal(2), BigDecimal(1)), None)
      )
      savings2_4.assets.list shouldBe empty
      savings2_4.assets.byId shouldBe empty
      savings2_4.assets.vwaps shouldBe empty

      savings2_4.getScheme(scheme.id) shouldBe scheme.copy(used = true, active = false)
      savings2_4.getFund(fund1.id) shouldBe fund1.copy(used = true, active = false)
      savings2_4.getFund(fund2.id) shouldBe fund2.copy(used = true, active = false)
    }

    "handle resolving assets availability by date" in {
      val scheme = savings2.schemes.head
      val fund1 = savings2.funds.head
      val fund2 = savings2.funds(1)
      val date = LocalDate.now.minusDays(10)
      // Check that multiple availability dates get merged at expected date
      // and that asset scheme/fund with only one availability date also gets
      // resolved once date is reached.
      val savings = savings2.processEvents(
        Savings.MakePayment(date.plusDays(1), Savings.AssetPart(scheme.id, fund1.id, None, BigDecimal(1), BigDecimal(1)), None),
        Savings.MakePayment(date.plusDays(2), Savings.AssetPart(scheme.id, fund1.id, Some(date.plusDays(10)), BigDecimal(2), BigDecimal(2)), None),
        Savings.MakePayment(date.plusDays(3), Savings.AssetPart(scheme.id, fund1.id, Some(date.plusDays(11)), BigDecimal(3), BigDecimal(3)), None),
        Savings.MakePayment(date.plusDays(4), Savings.AssetPart(scheme.id, fund2.id, Some(date.plusDays(10)), BigDecimal(4), BigDecimal(4)), None)
      )
      checkSavings(date, savings,
        Savings.Asset(scheme.id, fund1.id, None, BigDecimal(1), 0),
        Savings.Asset(scheme.id, fund1.id, Some(date.plusDays(10)), BigDecimal(2), 0),
        Savings.Asset(scheme.id, fund1.id, Some(date.plusDays(11)), BigDecimal(3), 0),
        Savings.Asset(scheme.id, fund2.id, Some(date.plusDays(10)), BigDecimal(4), 0)
      )
      checkSavings(date.plusDays(9), savings,
        Savings.Asset(scheme.id, fund1.id, None, BigDecimal(1), 0),
        Savings.Asset(scheme.id, fund1.id, Some(date.plusDays(10)), BigDecimal(2), 0),
        Savings.Asset(scheme.id, fund1.id, Some(date.plusDays(11)), BigDecimal(3), 0),
        Savings.Asset(scheme.id, fund2.id, Some(date.plusDays(10)), BigDecimal(4), 0)
      )
      checkSavings(date.plusDays(10), savings,
        Savings.Asset(scheme.id, fund1.id, None, BigDecimal(3), 0),
        Savings.Asset(scheme.id, fund1.id, Some(date.plusDays(11)), BigDecimal(3), 0),
        Savings.Asset(scheme.id, fund2.id, None, BigDecimal(4), 0)
      )
      checkSavings(date.plusDays(11), savings,
        Savings.Asset(scheme.id, fund1.id, None, BigDecimal(6), 0),
        Savings.Asset(scheme.id, fund2.id, None, BigDecimal(4), 0)
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
      checkFlattening(savings2, Savings.UpdateScheme(scheme).copy(name = scheme.name + "-new"))
      checkFlattening(savings2, Savings.UpdateScheme(scheme).copy(comment = Some("New comment")))
      checkFlattening(savings2, Savings.UpdateScheme(scheme).copy(disabled = true))
      checkFlattening(savings2, createFund3)
      checkFlattening(savings2, Savings.UpdateFund(fund1).copy(name = fund1.name + "-new"))
      checkFlattening(savings2, Savings.UpdateFund(fund1).copy(comment = Some("New comment")))
      checkFlattening(savings2, Savings.UpdateFund(fund1).copy(disabled = true))
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

      // Redundant events should be de-duplicated
      checkFlattening(savings2,
        List(Savings.UpdateScheme(scheme)),
        Nil
      )
      checkFlattening(savings2,
        List(Savings.UpdateScheme(scheme.copy(name = scheme.name + "-new")), Savings.UpdateScheme(scheme.copy(name = scheme.name + "-new"))),
        List(Savings.UpdateScheme(scheme.copy(name = scheme.name + "-new")))
      )
      checkFlattening(savings2,
        List(Savings.UpdateScheme(scheme.copy(comment = Some("New comment"))), Savings.UpdateScheme(scheme.copy(comment = Some("New comment")))),
        List(Savings.UpdateScheme(scheme.copy(comment = Some("New comment"))))
      )
      checkFlattening(savings2,
        List(Savings.UpdateScheme(scheme.copy(disabled = true)), Savings.UpdateScheme(scheme.copy(disabled = true))),
        List(Savings.UpdateScheme(scheme.copy(disabled = true)))
      )
      checkFlattening(savings2,
        List(Savings.UpdateFund(fund1)),
        Nil
      )
      checkFlattening(savings2,
        List(Savings.UpdateFund(fund1.copy(name = fund1.name + "-new")), Savings.UpdateFund(fund1.copy(name = fund1.name + "-new"))),
        List(Savings.UpdateFund(fund1.copy(name = fund1.name + "-new")))
      )
      checkFlattening(savings2,
        List(Savings.UpdateFund(fund1.copy(comment = Some("New comment"))), Savings.UpdateFund(fund1.copy(comment = Some("New comment")))),
        List(Savings.UpdateFund(fund1.copy(comment = Some("New comment"))))
      )
      checkFlattening(savings2,
        List(Savings.UpdateFund(fund1.copy(disabled = true)), Savings.UpdateFund(fund1.copy(disabled = true))),
        List(Savings.UpdateFund(fund1.copy(disabled = true)))
      )

      // Events cancelling each other should disappear
      checkFlattening(savings2,
        List(createScheme2, Savings.DeleteScheme(createScheme2.schemeId)),
        Nil
      )
      checkFlattening(savings2,
        List(Savings.UpdateScheme(scheme.copy(name = scheme.name + "-new")), Savings.UpdateScheme(scheme)),
        Nil
      )
      checkFlattening(savings2,
        List(Savings.UpdateScheme(scheme.copy(comment = Some("New comment"))), Savings.UpdateScheme(scheme)),
        Nil
      )
      checkFlattening(savings2,
        List(Savings.UpdateScheme(scheme.copy(disabled = true)), Savings.UpdateScheme(scheme)),
        Nil
      )
      checkFlattening(savings2,
        List(createFund3, Savings.DeleteFund(createFund3.fundId)),
        Nil
      )
      checkFlattening(savings2,
        List(Savings.UpdateFund(fund1.copy(name = fund1.name + "-new")), Savings.UpdateFund(fund1)),
        Nil
      )
      checkFlattening(savings2,
        List(Savings.UpdateFund(fund1.copy(comment = Some("New comment"))), Savings.UpdateFund(fund1)),
        Nil
      )
      checkFlattening(savings2,
        List(Savings.UpdateFund(fund1.copy(disabled = true)), Savings.UpdateFund(fund1)),
        Nil
      )
      checkFlattening(savings2,
        List(Savings.AssociateFund(scheme.id, createFund3.fundId), Savings.DissociateFund(scheme.id, createFund3.fundId)),
        Nil
      )
    }

    "keep track of latest asset action" in {
      val scheme = savings2.schemes.head
      val fund = savings2.funds.head
      val date = LocalDate.now.minusDays(10)
      // Out-of-order actions should be handled
      val savings = savings2.processEvents(
        Savings.MakePayment(date.plusDays(1), Savings.AssetPart(scheme.id, fund.id, None, BigDecimal(1), BigDecimal(1)), None),
        Savings.MakePayment(date.plusDays(4), Savings.AssetPart(scheme.id, fund.id, None, BigDecimal(1), BigDecimal(1)), None),
        Savings.MakePayment(date.plusDays(1), Savings.AssetPart(scheme.id, fund.id, None, BigDecimal(1), BigDecimal(1)), None)
      )
      savings.latestAssetAction shouldBe Some(date.plusDays(4))
    }
  }

  "Savings (de)serialization" should {
    "handle CreateScheme" in {
      checkEventSerialization(savings0.createSchemeEvent("scheme name"))
    }

    "handle UpdateScheme" in {
      checkEventSerialization(Savings.UpdateScheme(savings1.schemes.head.id, "scheme new name", None, disabled = false))
      checkEventSerialization(Savings.UpdateScheme(savings1.schemes.head.id, "scheme new name", Some("scheme new comment"), disabled = true))
    }

    "handle DeleteScheme" in {
      checkEventSerialization(Savings.DeleteScheme(savings1.schemes.head.id))
    }

    "handle CreateFund" in {
      checkEventSerialization(savings0.createFundEvent("fund name"))
    }

    "handle UpdateFund" in {
      checkEventSerialization(Savings.UpdateFund(savings1.funds.head.id, "fund new name", None, disabled = false))
      checkEventSerialization(Savings.UpdateFund(savings1.funds.head.id, "fund new name", Some("fund new comment"), disabled = true))
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
        Savings.AssetPart(savings1.schemes.head.id, savings1.funds.head.id, Some(LocalDate.now), BigDecimal(10), BigDecimal(10)), None))
      checkEventSerialization(Savings.MakePayment(LocalDate.now,
        Savings.AssetPart(savings1.schemes.head.id, savings1.funds.head.id, Some(LocalDate.now), BigDecimal(10), BigDecimal(10)), Some("comment")))
    }

    "handle MakeRefund" in {
      checkEventSerialization(Savings.MakeRefund(LocalDate.now,
        Savings.AssetPart(savings1.schemes.head.id, savings1.funds.head.id, Some(LocalDate.now), BigDecimal(10), BigDecimal(10)), None))
      checkEventSerialization(Savings.MakeRefund(LocalDate.now,
        Savings.AssetPart(savings1.schemes.head.id, savings1.funds.head.id, Some(LocalDate.now), BigDecimal(10), BigDecimal(10)), Some("comment")))
    }

    "handle MakeTransfer" in {
      val scheme = savings1.schemes.head
      val fund = savings1.funds.head
      checkEventSerialization(Savings.MakeTransfer(LocalDate.now,
        Savings.AssetPart(scheme.id, fund.id, Some(LocalDate.now), BigDecimal(10), BigDecimal(10)),
        Savings.AssetPart(fund.id, scheme.id, None, BigDecimal(1), BigDecimal(1)),
        None))
      checkEventSerialization(Savings.MakeTransfer(LocalDate.now,
        Savings.AssetPart(scheme.id, fund.id, Some(LocalDate.now), BigDecimal(10), BigDecimal(10)),
        Savings.AssetPart(fund.id, scheme.id, None, BigDecimal(1), BigDecimal(1)),
        Some("comment")))
    }
  }

  "Savings asset" should {
    "handle VWAP" in {
      val scheme = savings2.schemes.head
      val fund1 = savings2.funds.head
      val fund2 = savings2.funds(1)
      val id1 = Savings.AssetId(scheme.id, fund1.id)
      val id2 = Savings.AssetId(scheme.id, fund2.id)
      val date = LocalDate.now.minusDays(10)

      // Invest 1 unit at price 6; VWAP = 1*6 / 1 = 6
      val savings2_1 = savings2.processEvent(
        Savings.MakePayment(date.plusDays(1), Savings.AssetPart(scheme.id, fund1.id, None, BigDecimal(1), BigDecimal(6)), None)
      )
      checkSavings(date, savings2_1,
        Map(id1 -> BigDecimal(6)),
        Savings.Asset(scheme.id, fund1.id, None, BigDecimal(1), BigDecimal(6))
      )

      // Invest 2 units at price 3; VWAP = (1*6 + 2*3) / (1 + 2) = 4
      val savings2_2 = savings2_1.processEvent(
        Savings.MakePayment(date.plusDays(1), Savings.AssetPart(scheme.id, fund1.id, None, BigDecimal(2), BigDecimal(3)), None)
      )
      checkSavings(date, savings2_2,
        Map(id1 -> BigDecimal(4)),
        Savings.Asset(scheme.id, fund1.id, None, BigDecimal(3), BigDecimal(4))
      )

      // VWAP remains unchanged upon refund
      val savings2_3 = savings2_2.processEvent(
        Savings.MakeRefund(date.plusDays(1), Savings.AssetPart(scheme.id, fund1.id, None, BigDecimal(1), BigDecimal(100)), None)
      )
      checkSavings(date, savings2_3,
        Map(id1 -> BigDecimal(4)),
        Savings.Asset(scheme.id, fund1.id, None, BigDecimal(2), BigDecimal(4))
      )

      // Invest 2 units at price 2; (once all available) VWAP = (2*4 + 2*2) / (2 + 2) = 3
      val savings2_4 = savings2_3.processEvent(
        Savings.MakePayment(date.plusDays(1), Savings.AssetPart(scheme.id, fund1.id, Some(date.plusDays(10)), BigDecimal(2), BigDecimal(2)), None)
      )
      checkSavings(date, savings2_4,
        Map(id1 -> BigDecimal(3)),
        Savings.Asset(scheme.id, fund1.id, None, BigDecimal(2), BigDecimal(4)),
        Savings.Asset(scheme.id, fund1.id, Some(date.plusDays(10)), BigDecimal(2), BigDecimal(2))
      )
      checkSavings(date.plusDays(11), savings2_4,
        Map(id1 -> BigDecimal(3)),
        Savings.Asset(scheme.id, fund1.id, None, BigDecimal(4), BigDecimal(3))
      )

      // Transfer 2 units at price 2 (with VWAP = 4) to 4 units at price 1; dstVWAP = (2 * 4) / 4 = 2
      // With VWAP = 3 (availability independent); dstVWAP = (2 * 3) / 4 = 1.5
      val savings2_5 = savings2_4.processEvent(
        Savings.MakeTransfer(date.plusDays(1), Savings.AssetPart(scheme.id, fund1.id, None, BigDecimal(2), BigDecimal(2)),
          Savings.AssetPart(scheme.id, fund2.id, None, BigDecimal(4), BigDecimal(1)), None)
      )
      checkSavings(date, savings2_5,
        Map(id1 -> BigDecimal(3), id2 -> BigDecimal("1.5")),
        Savings.Asset(scheme.id, fund1.id, Some(date.plusDays(10)), BigDecimal(2), BigDecimal(2)),
        Savings.Asset(scheme.id, fund2.id, None, BigDecimal(4), BigDecimal(2))
      )

      // Transfer 2 units at price 4 (with VWAP = 2) to 8 units at price 1; dstVWAP = (4 * 2 + 2 * 2) / (4 + 8) = 1
      // With VWAP = 3 (availability independent) we should get the same result since all amounts are merged in the asset
      //   dstVWAP = (4 * 1.5 + 2 * 3) / (4 + 8) = 1  (as expected)
      val savings2_6 = savings2_5.processEvent(
        Savings.MakeTransfer(date.plusDays(1), Savings.AssetPart(scheme.id, fund1.id, Some(date.plusDays(10)), BigDecimal(2), BigDecimal(4)),
          Savings.AssetPart(scheme.id, fund2.id, None, BigDecimal(8), BigDecimal(1)), None)
      )
      checkSavings(date, savings2_6,
        Map(id2 -> BigDecimal(1)),
        Savings.Asset(scheme.id, fund2.id, None, BigDecimal(12), BigDecimal(1))
      )
    }
  }

  "Savings Event normalizing" should {
    "do nothing if events are already properly ordered" in {
      val date0 = LocalDate.now.minusDays(60)
      val g1e1 = savings0.createSchemeEvent("scheme 1")
      val g1e2 = savings0.createFundEvent("fund 1")
      val schemeId = g1e1.schemeId
      val schemeName = g1e1.name
      val fundId = g1e2.fundId
      val fundName = g1e2.name
      val g1e3 = Savings.AssociateFund(schemeId, fundId)
      val g2e1 = Savings.MakePayment(date0.plusDays(10), Savings.AssetPart(schemeId, fundId, None, 11, 11), None)
      val g2e2 = Savings.MakePayment(date0.plusDays(10), Savings.AssetPart(schemeId, fundId, None, 12, 12), None)
      val g2e3 = Savings.MakePayment(date0.plusDays(11), Savings.AssetPart(schemeId, fundId, None, 13, 13), None)
      val g2e4 = Savings.MakePayment(date0.plusDays(20), Savings.AssetPart(schemeId, fundId, None, 14, 14), None)

      val g3e1 = Savings.UpdateScheme(schemeId, schemeName + " - new", None, disabled = false)
      val g3e2 = Savings.UpdateFund(fundId, fundName + " - new", None, disabled = false)
      val g4e1 = Savings.MakePayment(date0.plusDays(20), Savings.AssetPart(schemeId, fundId, None, 15, 15), None)
      val g4e2 = Savings.MakePayment(date0.plusDays(21), Savings.AssetPart(schemeId, fundId, None, 16, 16), None)

      val g5e1 = Savings.UpdateScheme(schemeId, schemeName, None, disabled = false)
      val g5e2 = Savings.UpdateFund(fundId, fundName, None, disabled = false)
      val g6e1 = Savings.MakePayment(date0.plusDays(21), Savings.AssetPart(schemeId, fundId, None, 17, 17), None)
      val g6e2 = Savings.MakePayment(date0.plusDays(21), Savings.AssetPart(schemeId, fundId, None, 18, 18), None)

      val events = List[Savings.Event](
        g1e1, g1e2, g1e3,
        g2e1, g2e2, g2e3, g2e4,
        g3e1, g3e2,
        g4e1, g4e2,
        g5e1, g5e2,
        g6e1, g6e2
      )

      val (normalized, modified) = Savings.normalizeEvents(events)
      normalized shouldBe events
      modified shouldBe false
    }

    "handle grouping/sorting actions by date and flattening other events" in {
      // Actions should be re-grouped by date and sorted
      // Non-action events should be moved and flattened as necessary (note
      // flattening also reorders events).
      val date0 = LocalDate.now.minusDays(60)
      val g1e1 = savings0.createSchemeEvent("scheme 1")
      val g1e2 = savings0.createFundEvent("fund 1")
      val schemeId = g1e1.schemeId
      val schemeName = g1e1.name
      val fundId = g1e2.fundId
      val fundName = g1e2.name
      val g1e3 = Savings.AssociateFund(schemeId, fundId)
      val g2e1 = Savings.MakePayment(date0.plusDays(20), Savings.AssetPart(schemeId, fundId, None, 11, 11), None)
      val g2e2 = Savings.MakePayment(date0.plusDays(20), Savings.AssetPart(schemeId, fundId, None, 12, 12), None)
      val g2e3 = Savings.MakePayment(date0.plusDays(10), Savings.AssetPart(schemeId, fundId, None, 13, 13), None)
      val g2e4 = Savings.MakePayment(date0.plusDays(20), Savings.AssetPart(schemeId, fundId, None, 14, 14), None)
      val g2e5 = Savings.MakePayment(date0.plusDays(15), Savings.AssetPart(schemeId, fundId, None, 15, 15), None)

      // This event should be moved because an associated asset action will be moved before
      val g3e1mg1 = savings0.createSchemeEvent("scheme 2")
      val scheme2Id = g3e1mg1.schemeId
      val scheme2Name = g3e1mg1.name
      val g3e2 = Savings.UpdateScheme(schemeId, schemeName + " - new", None, disabled = false)
      val g3e3 = Savings.UpdateFund(fundId, fundName + " - new", None, disabled = false)
      val g4e1 = Savings.MakePayment(date0.plusDays(20), Savings.AssetPart(schemeId, fundId, None, 16, 16), None)
      val g4e2 = Savings.MakePayment(date0.plusDays(21), Savings.AssetPart(schemeId, fundId, None, 17, 17), None)
      val g4e1mg2 = Savings.MakePayment(date0.plusDays(15), Savings.AssetPart(schemeId, fundId, None, 18, 18), None)
      val g4e2mg2 = Savings.MakePayment(date0.plusDays(10), Savings.AssetPart(schemeId, fundId, None, 19, 19), None)

      // Those events should also be moved because an associated asset action will be moved before
      val g5e1mg1 = savings0.createFundEvent("fund 2")
      val fund2Id = g5e1mg1.fundId
      val fund2Name = g5e1mg1.name
      val g5e2mg1 = Savings.AssociateFund(scheme2Id, fund2Id)
      // Those events will be flattened (scheme&fund creation moved in the same group)
      val g5e3mg1 = Savings.UpdateScheme(scheme2Id, scheme2Name + " - new", None, disabled = false)
      val g5e4mg1 = Savings.UpdateFund(fund2Id, fund2Name + " - new", None, disabled = false)
      // Those event won't be moved, but will be flattened with those from the next group
      val g5e5 = Savings.UpdateScheme(schemeId, schemeName, None, disabled = false)
      val g5e6 = Savings.UpdateFund(fundId, fundName, None, disabled = false)
      val g6e1mg2 = Savings.MakePayment(date0.plusDays(5), Savings.AssetPart(schemeId, fundId, None, 20, 20), None)
      val g6e2mg2 = Savings.MakePayment(date0.plusDays(16), Savings.AssetPart(schemeId, fundId, None, 21, 21), None)
      val g6e3mg2 = Savings.MakePayment(date0.plusDays(16), Savings.AssetPart(scheme2Id, fund2Id, None, 22, 22), None)

      // Those events should not be moved because either
      //   * no asset action will be moved before
      //   * an asset action will be moved, but not before the asset creation
      // Those events should be merged with the ones from the previous group
      val g7e1 = Savings.UpdateScheme(schemeId, schemeName + " - new", None, disabled = false)
      val g7e2 = Savings.UpdateFund(fundId, fundName + " - new", None, disabled = false)
      val g7e3 = Savings.UpdateScheme(scheme2Id, scheme2Name, None, disabled = false)
      val g7e4 = Savings.UpdateFund(fund2Id, fund2Name, None, disabled = false)
      val g8e1mg3 = Savings.MakePayment(date0.plusDays(20), Savings.AssetPart(schemeId, fundId, None, 23, 23), None)
      // Those events should remain in this group but be reordered
      val g8e2 = Savings.MakePayment(date0.plusDays(22), Savings.AssetPart(schemeId, fundId, None, 24, 24), None)
      val g8e3 = Savings.MakePayment(date0.plusDays(21), Savings.AssetPart(schemeId, fundId, None, 25, 25), None)

      val events = List[Savings.Event](
        g1e1, g1e2, g1e3,
        g2e1, g2e2, g2e3, g2e4, g2e5,
        g3e1mg1, g3e2, g3e3,
        g4e1, g4e2, g4e1mg2, g4e2mg2,
        g5e1mg1, g5e2mg1, g5e3mg1, g5e4mg1, g5e5, g5e6,
        g6e1mg2, g6e2mg2, g6e3mg2,
        g7e1, g7e2, g7e3, g7e4,
        g8e1mg3, g8e2, g8e3
      )
      val eventsExpected = List[Savings.Event](
        g1e1, g3e1mg1.copy(name = g5e3mg1.name), g1e2, g5e1mg1.copy(name = g5e4mg1.name), g1e3, g5e2mg1,
        g6e1mg2, g2e3, g4e2mg2, g2e5, g4e1mg2, g6e2mg2, g6e3mg2, g2e1, g2e2, g2e4,
        g3e2, g3e3,
        g4e1, g8e1mg3, g4e2,
        g7e3, g7e4,
        g8e3, g8e2
      )
      val (normalized, modified) = Savings.normalizeEvents(events)
      normalized should not be events
      normalized shouldBe eventsExpected
      modified shouldBe true
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

    val scheme = savings.getScheme(expectedAsset.schemeId)
    scheme.used shouldBe true
    scheme.active shouldBe true
    val fund = savings.getFund(expectedAsset.fundId)
    fund.used shouldBe true
    fund.active shouldBe true
  }

  private def checkSavings(date: LocalDate, savings0: Savings, expectedAssets: Savings.Asset*): Unit = {
    checkSavings(date, savings0, Map.empty[Savings.AssetId, BigDecimal], expectedAssets:_*)
  }

  private def checkSavings(date: LocalDate, savings0: Savings, expectedVWAPs: Map[Savings.AssetId, BigDecimal], expectedAssets: Savings.Asset*): Unit = {
    val savings = savings0.computeAssets(date)
    withClue(s"Expecting ${expectedAssets.toList} for $savings:\n") {
      savings.assets.list.size shouldBe expectedAssets.size
      expectedAssets.foreach { expectedAsset =>
        checkAsset(date, savings, expectedAsset)
      }
    }
    if (expectedVWAPs.nonEmpty) {
      withClue(s"Expecting $expectedVWAPs for $savings:\n") {
        savings.assets.vwaps shouldBe expectedVWAPs
      }
    }
  }

  private def checkEventSerialization(event: Savings.Event): Unit = {
    val format = Savings.JsonProtocol.EventJsonFormat
    format.read(format.write(event)) shouldBe event
  }

}
