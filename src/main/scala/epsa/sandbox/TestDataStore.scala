package epsa.sandbox

import epsa.model.Savings
import epsa.storage.DataStore
import epsa.tools.EsaliaInvestmentFundProber
import java.nio.file.Path
import java.util.UUID
import javafx.application.Application
import javafx.stage.{FileChooser, Stage}
import suiryc.scala.javafx.stage.FileChoosers

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration.Duration
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.settings.Preference

object TestDataStore {

  def main(args: Array[String]): Unit = {
    (new TestDataStore).launch()
  }

}

class TestDataStore extends Application {

  def launch(): Unit = {
    Application.launch()
  }

  override def start(stage: Stage) {
    import epsa.Main.Akka._

    val promise = Promise[Unit]()
    promise.future.onComplete {
      case _ => epsa.Main.shutdown()
    }

    try {
      DataStore.open(None, change = false, save = false).orElse {
        DataStore.open(None, change = true, save = true)
      }.foreach { r =>
        Await.ready(r, Duration.Inf)
      }

      DataStore.EventSource.readEvents().onComplete {
        case read =>
          println(s"EventSource.readEvents => $read")
          read.toOption.foreach { events =>
            println(Savings().processEvents(events:_*))
          }
      }

      val uuid = UUID.fromString("3030f78e-bfa8-4df7-9e2e-f49625f1d2f6")

      DataStore.AssetHistory.readValues(uuid).onComplete {
        case read =>
          println(s"AssetHistory.readValue => $read")
          fundGraph(stage).map { hist =>
            DataStore.AssetHistory.writeValues(uuid, hist.values)
          }.getOrElse(Future.successful(())).onComplete {
            case r => promise.complete(r)
          }
      }
    } catch {
      case ex: Throwable =>
        ex.printStackTrace()
        promise.failure(ex)
    }
  }

  def fundGraph(stage: Stage): Option[Savings.AssetValueHistory] = JFXSystem.await {
    import epsa.Settings.prefs
    import Preference._

    val fundPath = Preference.from("fund.path", null:Path)

    val fileChooser = new FileChooser()
    fileChooser.setTitle("Open Investment Fund File")
    fileChooser.getExtensionFilters.addAll(
      new FileChooser.ExtensionFilter("Excel Files", "*.xls", "*.xlsx"),
      new FileChooser.ExtensionFilter("All Files", "*.*")
    )
    fundPath.option.foreach { path =>
      FileChoosers.setInitialPath(fileChooser, path.toFile)
    }
    val selectedFile = fileChooser.showOpenDialog(stage)
    Option(selectedFile).flatMap { file =>
      fundPath() = selectedFile.toPath
      EsaliaInvestmentFundProber.probe(file.toPath)
    }
  }

}
