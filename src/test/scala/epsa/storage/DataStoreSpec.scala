package epsa.storage

import epsa.model.Savings
import java.time.Month
import org.scalatest.{Matchers, WordSpec}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class DataStoreSpec extends WordSpec with Matchers {

  val store = DataStore.UnavailabilityPeriods

  val entry1 = Savings.UnavailabilityPeriod("id1", years = 1, month = None)
  val entry2 = entry1.copy(id = entry1.id + " new", years = entry1.years + 1, month = Some(Month.OCTOBER))

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
