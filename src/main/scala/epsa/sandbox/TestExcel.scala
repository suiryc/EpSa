package epsa.sandbox

import epsa.I18N.Strings
import epsa.charts.{ChartHandler, ChartSettings}
import epsa.model.Savings
import epsa.tools.EsaliaInvestmentFundProber
import java.nio.file.Path
import java.util.prefs.Preferences
import javafx.application.Application
import javafx.scene.Scene
import javafx.stage.FileChooser.ExtensionFilter
import javafx.stage.{FileChooser, Stage, WindowEvent}
import suiryc.scala.javafx.stage.FileChoosers
import suiryc.scala.settings.Preference

object TestExcel {

  def main(args: Array[String]): Unit = {
    val savings = Savings().processActions(
      _.createSchemeEvent("Scheme 1"),
      _.createSchemeEvent("Scheme 2"),
      _.createFundEvent("Fund 1"),
      _.createFundEvent("Fund 2"))
    val scheme1 = savings.schemes.find(_.name == "Scheme 1").get
    val scheme2 = savings.schemes.find(_.name == "Scheme 2").get
    val events = savings.funds.flatMap { fund =>
      List(
        Savings.AssociateFund(scheme1.id, fund.id),
        Savings.AssociateFund(scheme2.id, fund.id)
      )
    }
    val savings2 = savings.processEvents(events)
    println(savings2)

    (new TestExcel).launch()
  }

}

class TestExcel extends Application {

  /** Settings. */
  implicit val prefs = Preferences.userRoot.node("suiryc.epsa").node("epsa")

  def launch() {
    Application.launch()
  }

  override def start(stage: Stage) {
    import Preference._

    val fundPath = Preference.from("fund.path", null:Path)

    val fileChooser = new FileChooser()
    fileChooser.setTitle("Open Investment Fund File")
    fileChooser.getExtensionFilters.addAll(
      new ExtensionFilter("Excel Files", "*.xls", "*.xlsx"),
      new ExtensionFilter("All Files", "*.*")
    )
    fundPath.option.foreach { path =>
      FileChoosers.setInitialPath(fileChooser, path.toFile)
    }
    val selectedFile = fileChooser.showOpenDialog(stage)
    Option(selectedFile).flatMap { file =>
      EsaliaInvestmentFundProber.probe(file.toPath)
    } match {
      case Some(hist) =>
        // Save path in preferences
        fundPath() = selectedFile.toPath
        // Then build and display chart
        val chartHandler = new ChartHandler(
          seriesName = hist.name.orNull,
          seriesValues = hist.values,
          settings = ChartSettings.hidden.copy(
            xLabel = Strings.date,
            yLabel = Strings.nav,
            ySuffix = epsa.Settings.defaultCurrency
          )
        )
        val chartPane = chartHandler.chartPane
        chartPane.setPrefSize(640, 480)
        val scene = new Scene(chartPane)
        stage.setScene(scene)
        stage.show()

      case None =>
        stage.show()
        stage.fireEvent(new WindowEvent(stage, WindowEvent.WINDOW_CLOSE_REQUEST))
    }
  }

}
