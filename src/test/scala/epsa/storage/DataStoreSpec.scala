package epsa.storage

import epsa.model.Savings
import epsa.storage.DataStore.DBAction
import java.nio.file.Paths
import java.time.Month
import org.scalatest.{Matchers, WordSpec}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Success
import slick.driver.H2Driver.backend.DatabaseDef
import suiryc.scala.concurrent.RichFuture
import suiryc.scala.concurrent.RichFuture.Action

class DataStoreSpec extends WordSpec with Matchers {

  val store = DataStore.UnavailabilityPeriods

  val entry1 = Savings.UnavailabilityPeriod("id1", years = 1, month = None)
  val entry2 = entry1.copy(id = entry1.id + " new", years = entry1.years + 1, month = Some(Month.OCTOBER))

  "DataStore" should {
    "handle failure upon saving changes" in {
      def checkPass(tests: List[TestAppSetting], expectPending: Boolean, expectError: Boolean): Unit = {
        DataStore.hasPendingChanges shouldBe expectPending
        val f = DataStore.saveChanges()
        Await.ready(f, Duration.Inf)
        if (!expectError) f.value shouldBe Some(Success(()))
        else {
          val value = f.value
          value should not be empty
          value.get.isFailure shouldBe true
          value.get.failed.get.getCause shouldBe TestingFailure
        }
        if (expectError) {
          // If we are expecting an error, some actions (up to the issue)
          // should be done (and remain as-is in next attempts).
          val (done, failed) = tests.span { test =>
            (!test.failFirst && (test.attempt == 1)) || (test.failFirst && (test.attempt == 2))
          }
          done.zipWithIndex.foreach {
            case (test, index) =>
              withClue(s"(Done index=$index) ") {
                test.checkDone()
              }
          }
          // The failure should have been attempted, while the remaining
          // actions should not have been attempted at all.
          failed.zipWithIndex.foreach {
            case (test, index) =>
              withClue(s"(NotDone index=$index) ") {
                test.checkNotDone(attempted = index == 0)
              }
          }
        } else tests.foreach(_.checkDone())
      }

      def test(failures: Int, tests: List[TestAppSetting]): Unit = {
        // Apply actions on temporary DB
        import epsa.Main.Akka._
        val actions = tests.map { test =>
          Action(DataStore.AppSettings.newAction(test.dbAction))
        }
        await(RichFuture.executeSequentially(actions))

        tests.count(_.failFirst) shouldBe failures

        // Now save changes
        // For each failure expected it should fail
        (1 to failures).foreach { pass =>
          withClue(s"Pass = $pass: ") {
            checkPass(tests, expectPending = true, expectError = true)
          }
        }
        // Then the last attempt should work
        checkPass(tests, expectPending = true, expectError = false)
        // Finally there should not remain anything, and nothing should change
        checkPass(tests, expectPending = false, expectError = false)
      }

      // Setup in-memory DB as fake 'real' DB
      DataStore.close()
      val fakeReal = await(DataStore.dbOpen("temporary-test"))
      val realDb = fakeReal.db
      DataStore.dbRealOpt = Some(DataStore.DBInfo(fakeReal.db, Paths.get("temporary-test")))

      test(failures = 0, List(
        TestAppSetting(realDb, failFirst = false),
        TestAppSetting(realDb, failFirst = false),
        TestAppSetting(realDb, failFirst = false)
      ))
      test(failures = 3, List(
        TestAppSetting(realDb, failFirst = true),
        TestAppSetting(realDb, failFirst = true),
        TestAppSetting(realDb, failFirst = true)
      ))
      test(failures = 1, List(
        TestAppSetting(realDb, failFirst = true),
        TestAppSetting(realDb, failFirst = false),
        TestAppSetting(realDb, failFirst = false)
      ))
      test(failures = 1, List(
        TestAppSetting(realDb, failFirst = false),
        TestAppSetting(realDb, failFirst = true),
        TestAppSetting(realDb, failFirst = false)
      ))
      test(failures = 1, List(
        TestAppSetting(realDb, failFirst = false),
        TestAppSetting(realDb, failFirst = false),
        TestAppSetting(realDb, failFirst = true)
      ))
      test(failures = 3, List(
          TestAppSetting(realDb, failFirst = false),
          TestAppSetting(realDb, failFirst = true),
          TestAppSetting(realDb, failFirst = true),
          TestAppSetting(realDb, failFirst = false),
          TestAppSetting(realDb, failFirst = true),
          TestAppSetting(realDb, failFirst = false),
          TestAppSetting(realDb, failFirst = false)
      ))
    }
  }

  case object TestingFailure extends Exception

  case class TestAppSetting(real: DatabaseDef, failFirst: Boolean) {
    val key = scala.util.Random.nextString(16)
    var attempt = 0
    def value = attempt.toString
    val dbAction: DBAction[Unit] = (db: DatabaseDef) => {
      lazy val f = DataStore.AppSettings.writeEntry(db, (key, value))
      if (!(db eq real)) f
      else {
        attempt += 1
        if (!failFirst || (attempt > 1)) f
        else Future.failed(TestingFailure)
      }
    }

    private def read(): Option[String] =
      await(DataStore.AppSettings.readEntries(real)).find(_._1 == key).map(_._2)

    def checkDone(): Unit = {
      if (failFirst) attempt shouldBe 2
      else attempt shouldBe 1
      read() shouldBe Some(value)
    }

    def checkNotDone(attempted: Boolean): Unit = {
      if (attempted) attempt shouldBe 1
      else attempt shouldBe 0
      read() shouldBe None
    }
  }

  "DataStore UnavailabilityPeriods" should {
    "be empty at first" in {
      val entries = await(store.readEntries())
      entries shouldBe empty
    }

    "handle writing an entry" in {
      await(store.writeEntry(entry1))
      val entries = await(store.readEntries())
      entries shouldBe List(entry1)
    }

    "handle updating an entry" in {
      await(store.updateEntry(entry1.id, entry2))
      val entries = await(store.readEntries())
      entries shouldBe List(entry2)
    }

    "handle writing a second entry" in {
      await(store.writeEntry(entry1))
      val entries = await(store.readEntries())
      entries should contain theSameElementsAs List(entry1, entry2)
    }

    "handle deleting an entry" in {
      await(store.deleteEntry(entry2.id))
      val entries = await(store.readEntries())
      entries shouldBe List(entry1)
    }
  }

  def await[A](f: Future[A]): A =
    Await.result(f, Duration.Inf)

}
