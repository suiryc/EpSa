package epsa.model

import epsa.storage.DataStore
import java.time.LocalDate
import java.util.UUID
import org.scalatest.{Matchers, WordSpec}
import spray.json._
import suiryc.scala.concurrent.RichFuture._
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class LeviesSpec extends WordSpec with Matchers {

  import epsa.Main.Akka._
  import Levies.JsonProtocol._

  protected val today = LocalDate.now

  lazy protected val leviesComplex = buildLevies(s"""
  "name1": {
    "periods": [{ "rate": 0.5, "start": "2001-02-03" }]
  },

  "name2": {
    "periods": [{ "rate": 0.3, "start": "2002-01-01", "end": "2003-01-01" }]
  },

  "name3": {
    "periods": [{ "rate": 1.0, "start": "1990-01-01" },
                { "rate": 0.5, "start": "2000-01-01" },
                { "rate": 1.5, "start": "2010-01-01" }]
  },

  "name4": {
    "periods": [{ "rate": 2.0, "start": "1991-01-01", "end": "1991-12-31" },
                { "rate": 1.0, "start": "2001-01-01" }]
  }""", normalize = false)

  protected val levy1 = "name1"

  lazy protected val levies_1 = buildLevies(s"""
  "$levy1": {
    "periods": [{ "rate": 10, "start": "2001-01-01" }]
  }""")

  lazy protected val levies_1_end = buildLevies(s"""
  "$levy1": {
    "periods": [{ "rate": 10, "start": "2001-01-01", "end": "2009-12-31" }]
  }""")

  lazy protected val levies_3 = buildLevies(s"""
  "$levy1": {
    "periods": [{ "rate": 20, "start": "2001-01-01" },
                { "rate": 40, "start": "2004-01-01" },
                { "rate": 60, "start": "2008-01-01" }]
  }""")

  "Levies" should {
    "have a JSON deserializer" in {
      val actual = leviesComplex
      val expected = Levies(
        name = "Test",
        date = today,
        levies = Map(
          "name1" -> Levy(
            name = "name1",
            periods = List(LevyPeriod(rate = BigDecimal("0.5"), start = LocalDate.parse("2001-02-03"), end = None))
          ),
          "name2" -> Levy(
            name = "name2",
            periods = List(LevyPeriod(rate = BigDecimal("0.3"), start = LocalDate.parse("2002-01-01"), end = Some(LocalDate.parse("2003-01-01"))))
          ),
          "name3" -> Levy(
            name = "name3",
            periods = List(
              LevyPeriod(rate = BigDecimal("1.0"), start = LocalDate.parse("1990-01-01"), end = None),
              LevyPeriod(rate = BigDecimal("0.5"), start = LocalDate.parse("2000-01-01"), end = None),
              LevyPeriod(rate = BigDecimal("1.5"), start = LocalDate.parse("2010-01-01"), end = None)
            )
          ),
          "name4" -> Levy(
            name = "name4",
            periods = List(
              LevyPeriod(rate = BigDecimal("2.0"), start = LocalDate.parse("1991-01-01"), end = Some(LocalDate.parse("1991-12-31"))),
              LevyPeriod(rate = BigDecimal("1.0"), start = LocalDate.parse("2001-01-01"), end = None)
            )
          )
        )
      )

      actual shouldBe expected
    }

    "handle normalizing levies periods" in {
      val actual = leviesComplex.normalized
      val expected = Levies(
        name = "Test",
        date = today,
        levies = Map(
          "name1" -> Levy(
            name = "name1",
            periods = List(LevyPeriod(rate = BigDecimal("0.5"), start = LocalDate.parse("2001-02-03"), end = None))
          ),
          "name2" -> Levy(
            name = "name2",
            periods = List(
              LevyPeriod(rate = BigDecimal("0.3"), start = LocalDate.parse("2002-01-01"), end = Some(LocalDate.parse("2003-01-01"))),
              LevyPeriod(rate = BigDecimal("0"), start = LocalDate.parse("2003-01-02"), end = None)
            )
          ),
          "name3" -> Levy(
            name = "name3",
            periods = List(
              LevyPeriod(rate = BigDecimal("1.0"), start = LocalDate.parse("1990-01-01"), end = Some(LocalDate.parse("1999-12-31"))),
              LevyPeriod(rate = BigDecimal("0.5"), start = LocalDate.parse("2000-01-01"), end = Some(LocalDate.parse("2009-12-31"))),
              LevyPeriod(rate = BigDecimal("1.5"), start = LocalDate.parse("2010-01-01"), end = None)
            )
          ),
          "name4" -> Levy(
            name = "name4",
            periods = List(
              LevyPeriod(rate = BigDecimal("2.0"), start = LocalDate.parse("1991-01-01"), end = Some(LocalDate.parse("1991-12-31"))),
              LevyPeriod(rate = BigDecimal("0"), start = LocalDate.parse("1992-01-01"), end = Some(LocalDate.parse("2000-12-31"))),
              LevyPeriod(rate = BigDecimal("1.0"), start = LocalDate.parse("2001-01-01"), end = None)
            )
          )
        )
      )

      actual shouldBe expected
    }
  }

  "Savings levies computation" should {
    val savings0 = Savings().processActions(
      _.createSchemeEvent("scheme 1"),
      _.createFundEvent("fund 1"),
      { s => Savings.AssociateFund(s.schemes.head.id, s.funds.head.id) },
      _.createFundEvent("fund 2"),
      { s => Savings.AssociateFund(s.schemes.head.id, s.funds(1).id) }
    )
    val scheme = savings0.schemes.head
    val fund = savings0.funds.head
    val fund2 = savings0.funds(1)
    val assetId = Savings.AssetId(scheme.id, fund.id)
    val assetId2 = Savings.AssetId(scheme.id, fund2.id)

    "handle 1 period without end" in {
      val savings1 = savings0.copy(levies = levies_1_end)
      val levyPeriod1 = savings1.levies.levies(levy1).periods.head
      val levyStart = levyPeriod1.start
      val date_b1 = levyStart.minusDays(10)
      val date0 = levyStart.minusDays(1)
      val date1 = levyStart.plusDays(1)
      val date2 = levyStart.plusDays(2)
      val values = List(
        Savings.AssetValue(date_b1, 100),
        Savings.AssetValue(date0, 5),
        Savings.AssetValue(levyStart, 10),
        Savings.AssetValue(date1, 20),
        Savings.AssetValue(date2, 5)
      )
      val valuesMap = values.map(v => v.date -> v).toMap

      def payment(date: LocalDate, units: BigDecimal) =
        Savings.MakePayment(date, Savings.AssetPart(scheme.id, fund.id, None, units, valuesMap(date).value), None)
      def refund(date: LocalDate, units: BigDecimal) =
        Savings.MakeRefund(date, Savings.AssetPart(scheme.id, fund.id, None, units, valuesMap(date).value), None)

      // Testing investing and refunding during period, and having gain or loss
      usingDataStore {
        buildNAVHistory(NAVHistory(fund.id, values))
        val savings = savings1.processEvents(
          payment(levyStart, BigDecimal(4)),
          refund(date1, BigDecimal(3))
        )
        // Invested 4 units at price 10; refunded 3 units.
        // Remaining 1 unit (invested = 10) at price 20 (gross = 20; gain = 10; levies = 1)
        val actual1 = savings.computeLevies(assetId, date1, valuesMap(date1).value)
        val expected1 = LeviesPeriodsData(Map(
          levy1 -> List(LevyPeriodData(levyPeriod1, BigDecimal(10), Some(BigDecimal(10))))
        ))
        actual1 shouldBe expected1
        actual1.amount shouldBe BigDecimal(1)

        // Remaining 1 unit (invested = 10) at price 5 (gross = 5; gain = -5; levies = 0)
        val actual2 = savings.computeLevies(assetId, date2, valuesMap(date2).value)
        val expected2 = LeviesPeriodsData(Map(
          levy1 -> List(LevyPeriodData(levyPeriod1, BigDecimal(10), Some(BigDecimal(-5))))
        ))
        actual2 shouldBe expected2
        actual2.amount shouldBe BigDecimal(0)
      }

      // Testing investing before period, refunding during period, and having gain or loss
      usingDataStore {
        buildNAVHistory(NAVHistory(fund.id, values))
        val savings = savings1.processEvents(
          payment(date_b1, BigDecimal(4)),
          refund(date1, BigDecimal(3))
        )
        // Invested 4 units; period started at price 5; refunded 3 units.
        // Remaining 1 unit (invested = 5) at price 20 (gross = 20; gain = 15; levies = 1.5)
        val actual1 = savings.computeLevies(assetId, date1, valuesMap(date1).value)
        val expected1 = LeviesPeriodsData(Map(
          levy1 -> List(LevyPeriodData(levyPeriod1, BigDecimal(5), Some(BigDecimal(15))))
        ))
        actual1 shouldBe expected1
        actual1.amount shouldBe BigDecimal("1.5")

        // Remaining 1 unit (invested = 5) at price 5 (gross = 5; gain = 0; levies = 0)
        val actual2 = savings.computeLevies(assetId, date2, valuesMap(date2).value)
        val expected2 = LeviesPeriodsData(Map(
          levy1 -> List(LevyPeriodData(levyPeriod1, BigDecimal(5), Some(BigDecimal(0))))
        ))
        actual2 shouldBe expected2
        actual2.amount shouldBe BigDecimal(0)
      }
    }

    "handle 1 period with gain at the end" in {
      val savings1 = savings0.copy(levies = levies_1_end)
      val levyPeriod1 = savings1.levies.levies(levy1).periods.head
      val levyPeriod2 = savings1.levies.levies(levy1).periods(1)
      val levyStart = levyPeriod1.start
      val levyEnd = levyPeriod1.end.get
      val date_b1 = levyStart.minusDays(10)
      val date0 = levyStart.minusDays(1)
      val date_a1 = levyEnd.plusDays(1)
      val date_a2 = levyEnd.plusDays(2)
      val date_a3 = levyEnd.plusDays(3)
      val values = List(
        Savings.AssetValue(date_b1, 100),
        Savings.AssetValue(date0, 5),
        Savings.AssetValue(levyEnd, 15),
        Savings.AssetValue(date_a1, 20),
        Savings.AssetValue(date_a2, 11),
        Savings.AssetValue(date_a3, 4)
      )
      val valuesMap = values.map(v => v.date -> v).toMap

      def payment(date: LocalDate, units: BigDecimal) =
        Savings.MakePayment(date, Savings.AssetPart(scheme.id, fund.id, None, units, valuesMap(date).value), None)
      def refund(date: LocalDate, units: BigDecimal) =
        Savings.MakeRefund(date, Savings.AssetPart(scheme.id, fund.id, None, units, valuesMap(date).value), None)

      // Testing investing before period, having gain, refunding after period, and having more or less gain
      usingDataStore {
        buildNAVHistory(NAVHistory(fund.id, values))
        val savings = savings1.processEvents(
          payment(date_b1, BigDecimal(4)),
          refund(date_a1, BigDecimal(3))
        )
        // Invested 4 units; period started at price 5 and ended at price 15; refunded 3 units.
        // Remaining 1 unit (invested = 5) at price 20.
        //   gross = 15 at end of period, 20 after
        //   gain = 10 during period, +5 after; retained gain (during period) = 10
        //   levies = 1
        val actual1 = savings.computeLevies(assetId, date_a1, valuesMap(date_a1).value)
        val expected1 = LeviesPeriodsData(Map(
          levy1 -> List(
            LevyPeriodData(levyPeriod2, BigDecimal(15), Some(BigDecimal(5))),
            LevyPeriodData(levyPeriod1, BigDecimal(5), Some(BigDecimal(10)))
          )
        ))
        actual1 shouldBe expected1
        actual1.amount shouldBe BigDecimal(1)

        // Remaining 1 unit (invested = 5) at price 11.
        //   gross = 15 at end of period, 11 after
        //   gain = 10 during period, -4 after; retained gain (loss after period pushed back) = 6
        //   levies = 0.6
        val actual2 = savings.computeLevies(assetId, date_a2, valuesMap(date_a2).value)
        val expected2 = LeviesPeriodsData(Map(
          levy1 -> List(
            LevyPeriodData(levyPeriod2, BigDecimal(15), Some(BigDecimal(0))),
            LevyPeriodData(levyPeriod1, BigDecimal(5), Some(BigDecimal(6)))
          )
        ))
        actual2 shouldBe expected2
        actual2.amount shouldBe BigDecimal("0.6")

        // Remaining 1 unit (invested = 5) at price 4.
        //   gross = 15 at end of period, 4 after
        //   gain = 10 during period, -11 after; retained gain (loss after period pushed back) = -1
        //   levies = 0
        val actual3 = savings.computeLevies(assetId, date_a3, valuesMap(date_a3).value)
        val expected3 = LeviesPeriodsData(Map(
          levy1 -> List(
            LevyPeriodData(levyPeriod2, BigDecimal(15), Some(BigDecimal(0))),
            LevyPeriodData(levyPeriod1, BigDecimal(5), Some(BigDecimal(-1)))
          )
        ))
        actual3 shouldBe expected3
        actual3.amount shouldBe BigDecimal(0)
      }
    }

    "handle 1 period with loss at the end" in {
      val savings1 = savings0.copy(levies = levies_1_end)
      val levyPeriod1 = savings1.levies.levies(levy1).periods.head
      val levyPeriod2 = savings1.levies.levies(levy1).periods(1)
      val levyStart = levyPeriod1.start
      val levyEnd = levyPeriod1.end.get
      val date_b1 = levyStart.minusDays(10)
      val date0 = levyStart.minusDays(1)
      val date_a1 = levyEnd.plusDays(1)
      val values = List(
        Savings.AssetValue(date_b1, 100),
        Savings.AssetValue(date0, 20),
        Savings.AssetValue(levyEnd, 15),
        Savings.AssetValue(date_a1, 100)
      )
      val valuesMap = values.map(v => v.date -> v).toMap

      def payment(date: LocalDate, units: BigDecimal) =
        Savings.MakePayment(date, Savings.AssetPart(scheme.id, fund.id, None, units, valuesMap(date).value), None)
      def refund(date: LocalDate, units: BigDecimal) =
        Savings.MakeRefund(date, Savings.AssetPart(scheme.id, fund.id, None, units, valuesMap(date).value), None)

      // Testing investing before period, having gain, refunding after period, and having more or less gain
      usingDataStore {
        buildNAVHistory(NAVHistory(fund.id, values))
        val savings = savings1.processEvents(
          payment(date_b1, BigDecimal(4)),
          refund(date_a1, BigDecimal(3))
        )
        // Invested 4 units; period started at price 20 and ended at price 15; refunded 3 units.
        // Remaining 1 unit (invested = 20) at price 100.
        //   gross = 15 at end of period, 100 after
        //   gain = -5 during period, +85 after; retained gain (during period) = -5 (invested amount retained for next period)
        //   levies = 0
        val actual1 = savings.computeLevies(assetId, date_a1, valuesMap(date_a1).value)
        val expected1 = LeviesPeriodsData(Map(
          levy1 -> List(
            LevyPeriodData(levyPeriod2, BigDecimal(20), Some(BigDecimal(80))),
            LevyPeriodData(levyPeriod1, BigDecimal(20), Some(BigDecimal(0)))
          )
        ))
        actual1 shouldBe expected1
        actual1.amount shouldBe BigDecimal(0)
      }
    }

    "handle 3 periods with gain on first and second" in {
      val savings1 = savings0.copy(levies = levies_3)
      val levyPeriod1 = savings1.levies.levies(levy1).periods.head
      val levyPeriod2 = savings1.levies.levies(levy1).periods(1)
      val levy1Start = levyPeriod1.start
      val levy2Start = levyPeriod2.start
      val date_b1 = levy1Start.minusDays(10)
      val date_p1_0 = levy1Start.minusDays(1)
      val date_p1_1 = levy1Start.plusDays(1)
      val date_p2_0 = levy2Start.minusDays(1)
      val date_p2_1 = levy2Start.plusDays(1)
      val values = List(
        Savings.AssetValue(date_b1, 100),
        Savings.AssetValue(date_p1_0, 10),
        Savings.AssetValue(date_p1_1, 15),
        Savings.AssetValue(date_p2_0, 20),
        Savings.AssetValue(date_p2_1, 25)
      )
      val valuesMap = values.map(v => v.date -> v).toMap

      def payment(date: LocalDate, units: BigDecimal) =
        Savings.MakePayment(date, Savings.AssetPart(scheme.id, fund.id, None, units, valuesMap(date).value), None)
      def refund(date: LocalDate, units: BigDecimal) =
        Savings.MakeRefund(date, Savings.AssetPart(scheme.id, fund.id, None, units, valuesMap(date).value), None)

      // Testing investing before period1, and having gain on period1 then period2
      usingDataStore {
        buildNAVHistory(NAVHistory(fund.id, values))
        val savings = savings1.processEvents(
          payment(date_b1, BigDecimal(4)),
          refund(date_p1_1, BigDecimal(3))
        )
        // Invested 4 units; period1 started at price 10; refunded 3 units.
        // Remaining 1 unit (invested = 10) at price 15 (gross = 15; gain = 5; levies = 1)
        val actual1 = savings.computeLevies(assetId, date_p1_1, valuesMap(date_p1_1).value)
        val expected1 = LeviesPeriodsData(Map(
          levy1 -> List(LevyPeriodData(levyPeriod1, BigDecimal(10), Some(BigDecimal(5))))
        ))
        actual1 shouldBe expected1
        actual1.amount shouldBe BigDecimal(1)

        // Period1 ended (and period2 started) at price 20.
        // Remaining 1 unit at price 25
        //   invested = 10 for period1, 20 for period2
        //   gross = 20 at end of period1, 25 at action
        //   gain = 10 for period1, 5 for period2
        //   levies = 2 + 2 = 4
        val actual2 = savings.computeLevies(assetId, date_p2_1, valuesMap(date_p2_1).value)
        val expected2 = LeviesPeriodsData(Map(
          levy1 -> List(
            LevyPeriodData(levyPeriod2, BigDecimal(20), Some(BigDecimal(5))),
            LevyPeriodData(levyPeriod1, BigDecimal(10), Some(BigDecimal(10)))
          )
        ))
        actual2 shouldBe expected2
        actual2.amount shouldBe BigDecimal(4)
      }
    }

    "handle 3 periods with loss on first and more or less gain on second" in {
      val savings1 = savings0.copy(levies = levies_3)
      val levyPeriod1 = savings1.levies.levies(levy1).periods.head
      val levyPeriod2 = savings1.levies.levies(levy1).periods(1)
      val levy1Start = levyPeriod1.start
      val levy2Start = levyPeriod2.start
      val date_b1 = levy1Start.minusDays(10)
      val date_p1_0 = levy1Start.minusDays(1)
      val date_p2_0 = levy2Start.minusDays(1)
      val date_p2_1 = levy2Start.plusDays(1)
      val date_p2_2 = levy2Start.plusDays(2)
      val values = List(
        Savings.AssetValue(date_b1, 100),
        Savings.AssetValue(date_p1_0, 20),
        Savings.AssetValue(date_p2_0, 10),
        Savings.AssetValue(date_p2_1, 15),
        Savings.AssetValue(date_p2_2, 25)
      )
      val valuesMap = values.map(v => v.date -> v).toMap

      def payment(date: LocalDate, units: BigDecimal) =
        Savings.MakePayment(date, Savings.AssetPart(scheme.id, fund.id, None, units, valuesMap(date).value), None)
      def refund(date: LocalDate, units: BigDecimal) =
        Savings.MakeRefund(date, Savings.AssetPart(scheme.id, fund.id, None, units, valuesMap(date).value), None)

      // Testing investing before period1, and having loss on period1 then gain on period2
      usingDataStore {
        buildNAVHistory(NAVHistory(fund.id, values))
        val savings = savings1.processEvents(
          payment(date_b1, BigDecimal(4)),
          refund(date_p2_1, BigDecimal(3))
        )
        // Invested 4 units; period1 started at price 20 and ended at price 10; refunded 3 units.
        // Remaining 1 unit at price 15
        //   invested = 20 for period1, 10 for period2 (replaced by 20 from period1 due to loss)
        //   gross = 10 at end of period1, 15 at action
        //   gain = -10 for period1 (zeroed), -5 for period2; final gain = -5 on period1 (pushed back from period2) and 0 on period2
        //   levies = 0
        val actual1 = savings.computeLevies(assetId, date_p2_1, valuesMap(date_p2_1).value)
        val expected1 = LeviesPeriodsData(Map(
          levy1 -> List(
            LevyPeriodData(levyPeriod2, BigDecimal(20), Some(BigDecimal(0))),
            LevyPeriodData(levyPeriod1, BigDecimal(20), Some(BigDecimal(-5)))
          )
        ))
        actual1 shouldBe expected1
        actual1.amount shouldBe BigDecimal(0)

        // Remaining 1 unit at price 25
        //   invested = 20 for period1, 10 for period2 (replaced by 20 from period1 due to loss)
        //   gross = 10 at end of period1, 25 at action
        //   gain = -10 for period1 (zeroed), 5 for period2
        //   levies = 0 + 2
        val actual2 = savings.computeLevies(assetId, date_p2_2, valuesMap(date_p2_2).value)
        val expected2 = LeviesPeriodsData(Map(
          levy1 -> List(
            LevyPeriodData(levyPeriod2, BigDecimal(20), Some(BigDecimal(5))),
            LevyPeriodData(levyPeriod1, BigDecimal(20), Some(BigDecimal(0)))
          )
        ))
        actual2 shouldBe expected2
        actual2.amount shouldBe BigDecimal(2)
      }
    }

    "handle 3 periods with gain on first, loss on second and gain or loss on third" in {
      val savings1 = savings0.copy(levies = levies_3)
      val levyPeriod1 = savings1.levies.levies(levy1).periods.head
      val levyPeriod2 = savings1.levies.levies(levy1).periods(1)
      val levyPeriod3 = savings1.levies.levies(levy1).periods(2)
      val levy1Start = levyPeriod1.start
      val levy2Start = levyPeriod2.start
      val levy3Start = levyPeriod3.start
      val date_b1 = levy1Start.minusDays(10)
      val date_p1_0 = levy1Start.minusDays(1)
      val date_p2_0 = levy2Start.minusDays(1)
      val date_p3_0 = levy3Start.minusDays(1)
      val date_p3_1 = levy3Start.plusDays(1)
      val date_p3_2 = levy3Start.plusDays(2)
      val date_p3_3 = levy3Start.plusDays(3)
      val values = List(
        Savings.AssetValue(date_b1, 100),
        Savings.AssetValue(date_p1_0, 10),
        Savings.AssetValue(date_p2_0, 20),
        Savings.AssetValue(date_p3_0, 15),
        Savings.AssetValue(date_p3_1, 25),
        Savings.AssetValue(date_p3_2, 20),
        Savings.AssetValue(date_p3_3, 15)
      )
      val valuesMap = values.map(v => v.date -> v).toMap

      def payment(date: LocalDate, units: BigDecimal) =
        Savings.MakePayment(date, Savings.AssetPart(scheme.id, fund.id, None, units, valuesMap(date).value), None)
      def refund(date: LocalDate, units: BigDecimal) =
        Savings.MakeRefund(date, Savings.AssetPart(scheme.id, fund.id, None, units, valuesMap(date).value), None)

      // Testing investing before period1, and having gain on period1, loss on period2, and gain or loss on period3
      usingDataStore {
        buildNAVHistory(NAVHistory(fund.id, values))
        val savings = savings1.processEvents(
          payment(date_b1, BigDecimal(4)),
          refund(date_p3_1, BigDecimal(3))
        )
        // Invested 4 units; period1 started at price 10 and ended at price 20; period2 ended at 15; refunded 3 units.
        // Remaining 1 unit at price 25
        //   invested = 10 for period1, 20 for period2, 15 for period3 (replaced by 20 from period2 due to loss)
        //   gross = 20 at end of period1, 15 at end of period2, 25 at action
        //   gain = 10 for period1, -5 for period2 (zeroed), 5 at action
        //   levies = 2 + 0 + 3
        val actual1 = savings.computeLevies(assetId, date_p3_1, valuesMap(date_p3_1).value)
        val expected1 = LeviesPeriodsData(Map(
          levy1 -> List(
            LevyPeriodData(levyPeriod3, BigDecimal(20), Some(BigDecimal(5))),
            LevyPeriodData(levyPeriod2, BigDecimal(20), Some(BigDecimal(0))),
            LevyPeriodData(levyPeriod1, BigDecimal(10), Some(BigDecimal(10)))
          )
        ))
        actual1 shouldBe expected1
        actual1.amount shouldBe BigDecimal(5)

        // Remaining 1 unit at price 20
        //   invested = 10 for period1, 20 for period2, 15 for period3 (replaced by 20 from period2 due to loss)
        //   gross = 20 at end of period1, 15 at end of period2, 20 at action
        //   gain = 10 for period1, -5 for period2 (zeroed), 0 at action
        //   levies = 2 + 0 + 0
        val actual2 = savings.computeLevies(assetId, date_p3_2, valuesMap(date_p3_2).value)
        val expected2 = LeviesPeriodsData(Map(
          levy1 -> List(
            LevyPeriodData(levyPeriod3, BigDecimal(20), Some(BigDecimal(0))),
            LevyPeriodData(levyPeriod2, BigDecimal(20), Some(BigDecimal(0))),
            LevyPeriodData(levyPeriod1, BigDecimal(10), Some(BigDecimal(10)))
          )
        ))
        actual2 shouldBe expected2
        actual2.amount shouldBe BigDecimal(2)

        // Remaining 1 unit at price 15
        //   invested = 10 for period1, 20 for period2, 15 for period3 (replaced by 20 from period2 due to loss)
        //   gross = 20 at end of period1, 15 at end of period2, 15 at action
        //   gain = 10 for period1, -5 for period2 (zeroed), -5 at action; final gain = 5 for period1 (loss pushed back), 0 for other periods
        //   levies = 1 + 0 + 0
        val actual3 = savings.computeLevies(assetId, date_p3_3, valuesMap(date_p3_3).value)
        val expected3 = LeviesPeriodsData(Map(
          levy1 -> List(
            LevyPeriodData(levyPeriod3, BigDecimal(20), Some(BigDecimal(0))),
            LevyPeriodData(levyPeriod2, BigDecimal(20), Some(BigDecimal(0))),
            LevyPeriodData(levyPeriod1, BigDecimal(10), Some(BigDecimal(5)))
          )
        ))
        actual3 shouldBe expected3
        actual3.amount shouldBe BigDecimal(1)
      }
    }

    "handle 3 periods with gain on second and third" in {
      val savings1 = savings0.copy(levies = levies_3)
      val levyPeriod2 = savings1.levies.levies(levy1).periods(1)
      val levyPeriod3 = savings1.levies.levies(levy1).periods(2)
      val levy2Start = levyPeriod2.start
      val levy3Start = levyPeriod3.start
      val date_p2_0 = levy2Start.minusDays(1)
      val date_p2_1 = levy2Start.plusDays(1)
      val date_p3_0 = levy3Start.minusDays(1)
      val date_p3_1 = levy3Start.plusDays(1)
      val values = List(
        Savings.AssetValue(date_p2_0, 10),
        Savings.AssetValue(date_p2_1, 15),
        Savings.AssetValue(date_p3_0, 20),
        Savings.AssetValue(date_p3_1, 30)
      )
      val valuesMap = values.map(v => v.date -> v).toMap

      def payment(date: LocalDate, units: BigDecimal) =
        Savings.MakePayment(date, Savings.AssetPart(scheme.id, fund.id, None, units, valuesMap(date).value), None)
      def refund(date: LocalDate, units: BigDecimal) =
        Savings.MakeRefund(date, Savings.AssetPart(scheme.id, fund.id, None, units, valuesMap(date).value), None)

      // Testing investing on period2, and having gain on period2 and period3
      usingDataStore {
        buildNAVHistory(NAVHistory(fund.id, values))
        val savings = savings1.processEvents(
          payment(date_p2_1, BigDecimal(4)),
          refund(date_p3_1, BigDecimal(3))
        )
        // Invested 4 units at price 15; period2 ended at price 20; refunded 3 units.
        // Remaining 1 unit at price 30
        //   invested = 15 for period2, 20 for period3
        //   gross = 20 at end of period2, 30 at action
        //   gain = 5 for period2, 10 at action
        //   levies = 2 + 6
        val actual1 = savings.computeLevies(assetId, date_p3_1, valuesMap(date_p3_1).value)
        val expected1 = LeviesPeriodsData(Map(
          levy1 -> List(
            LevyPeriodData(levyPeriod3, BigDecimal(20), Some(BigDecimal(10))),
            LevyPeriodData(levyPeriod2, BigDecimal(15), Some(BigDecimal(5)))
          )
        ))
        actual1 shouldBe expected1
        actual1.amount shouldBe BigDecimal(8)
      }
    }

    "handle transfer" in {
      val savings1 = savings0.copy(levies = levies_3)
      val levyPeriod1 = savings1.levies.levies(levy1).periods.head
      val levyPeriod2 = savings1.levies.levies(levy1).periods(1)
      val levyPeriod3 = savings1.levies.levies(levy1).periods(2)
      val levy1Start = levyPeriod1.start
      val levy2Start = levyPeriod2.start
      val levy3Start = levyPeriod3.start
      val date_b1 = levy1Start.minusDays(10)
      val date_p1_0 = levy1Start.minusDays(1)
      val date_p2_0 = levy2Start.minusDays(1)
      val date_p3_0 = levy3Start.minusDays(1)
      val date_p3_1 = levy3Start.plusDays(1)
      val values = List(
        Savings.AssetValue(date_b1, 100),
        Savings.AssetValue(date_p1_0, 10),
        Savings.AssetValue(date_p2_0, 20),
        Savings.AssetValue(date_p3_0, 15),
        Savings.AssetValue(date_p3_1, 25)
      )
      val values2 = List(
        Savings.AssetValue(date_p3_1, 30)
      )
      val valuesMap = values.map(v => v.date -> v).toMap
      val values2Map = values2.map(v => v.date -> v).toMap

      def payment(date: LocalDate, units: BigDecimal) =
        Savings.MakePayment(date, Savings.AssetPart(scheme.id, fund.id, None, units, valuesMap(date).value), None)
      def transfer(date: LocalDate, units: BigDecimal) =
        Savings.MakeTransfer(date, Savings.AssetPart(scheme.id, fund.id, None, units, valuesMap(date).value)
          , Savings.AssetPart(scheme.id, fund2.id, None, units * valuesMap(date).value / values2Map(date).value, values2Map(date).value), None)

      // Testing transferring 75% of fund.
      usingDataStore {
        buildNAVHistory(NAVHistory(fund.id, values), NAVHistory(fund2.id, values2))
        val savings = savings1.processEvents(
          payment(date_b1, BigDecimal(4)),
          transfer(date_p3_1, BigDecimal(3))
        )
        // Invested 4 units; period1 started at price 10 and ended at price 20; period2 ended at 15; transferred 3 units.
        // Remaining 1 unit at price 25 in fund1
        //   invested = 10 for period1, 20 for period2, 15 for period3 (replaced by 20 from period2 due to loss)
        //   gross = 20 at end of period1, 15 at end of period2, 25 at action
        //   gain = 10 for period1, -5 for period2 (zeroed), 5 at action
        //   levies = 2 + 0 + 3
        val actual1 = savings.computeLevies(assetId, date_p3_1, valuesMap(date_p3_1).value)
        val expected1 = LeviesPeriodsData(Map(
          levy1 -> List(
            LevyPeriodData(levyPeriod3, BigDecimal(20), Some(BigDecimal(5))),
            LevyPeriodData(levyPeriod2, BigDecimal(20), Some(BigDecimal(0))),
            LevyPeriodData(levyPeriod1, BigDecimal(10), Some(BigDecimal(10)))
          )
        ))
        actual1 shouldBe expected1
        actual1.amount shouldBe BigDecimal(5)
        savings.assets.units(assetId) shouldBe BigDecimal(1)

        // Remaining 2.5 (3*25=2.5*30) units at price 30 in fund2
        //   invested = 10*3 for period1, 20*3 for period2, 15*3 for period3 (replaced by 20*3 from period2 due to loss)
        //   gross = 20*3 at end of period1, 15*3 at end of period2, 30*2.5 at action
        //   gain = 10*3 for period1, -5*3 for period2 (zeroed), 15 at action
        //   levies = 6 + 0 + 9
        // Note: since fund2 NAV does not change after we transfer, invested
        // amounts, gains and levies shall be those of fund1 * 3 (ratio of
        // original actions).
        val actual2 = savings.computeLevies(assetId2, date_p3_1, values2Map(date_p3_1).value)
        val expected2 = LeviesPeriodsData(Map(
          levy1 -> List(
            LevyPeriodData(levyPeriod3, BigDecimal(60), Some(BigDecimal(15))),
            LevyPeriodData(levyPeriod2, BigDecimal(60), Some(BigDecimal(0))),
            LevyPeriodData(levyPeriod1, BigDecimal(30), Some(BigDecimal(30)))
          )
        ))
        actual2 shouldBe expected2
        actual2.amount shouldBe BigDecimal(15)
        savings.assets.units(assetId2) shouldBe BigDecimal("2.5")
      }
    }
  }

  protected def usingDataStore[A](body: => A): Unit = {
    // Ensure DataStore is empty (before and after) when executing some code block.
    DataStore.close()
    try {
      body
    } finally {
      DataStore.close()
    }
  }

  case class NAVHistory(fundId: UUID, values: List[Savings.AssetValue])

  protected def buildNAVHistory(histories: NAVHistory*): Unit = {
    val actions = histories.map { history =>
      Action(DataStore.AssetHistory.writeValues(history.fundId, history.values))
    }
    val f = executeAllSequentially(stopOnError = true, actions)
    Await.ready(f, Duration.Inf)
  }

  protected def buildLevies(str: String, normalize: Boolean = true): Levies = {
    val levies = s"""{
  "name": "Test",
  "date": "$today",
  $str
}""".parseJson.convertTo[Levies]
    if (!normalize) levies
    else levies.normalized
  }

}
