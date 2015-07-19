package epsa

import java.nio.file.Paths
import javafx.application.Application
import javafx.scene.Scene
import javafx.stage.Stage

object TestExcel {

  var support: Support = _

  def main(args: Array[String]): Unit = {
    val path = Paths.get("D:", "data.xls")

    EsaliaSupportProber.probe(path).foreach(support = _)
    println(support)

    (new TestExcel).launch()
  }

}

class TestExcel extends Application {

  def launch() {
    Application.launch()
  }

  override def start(stage: Stage) {
    val chartHandler = new ChartHandler(TestExcel.support)
    val chartPane = chartHandler.chartPane
    chartPane.setPrefSize(640, 480)
    val scene = new Scene(chartPane)
    stage.setScene(scene)
    stage.show()
  }

}
