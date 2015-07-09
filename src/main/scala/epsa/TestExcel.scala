package epsa

import java.nio.file.{Path, Paths}
import java.text.SimpleDateFormat
import java.util.Date
import javafx.application.Application
import javafx.geometry.Bounds
import javafx.scene.{Node, Scene}
import javafx.scene.chart.{XYChart, LineChart, NumberAxis, CategoryAxis}
import javafx.scene.input.MouseEvent
import javafx.scene.layout.{Pane, Region}
import javafx.scene.shape.Line
import javafx.stage.Stage

import org.apache.poi.ss.usermodel.WorkbookFactory

import suiryc.scala.javafx.event.EventHandler._

object TestExcel {

  var support: Support = _

  def main(args: Array[String]): Unit = {
    val path = Paths.get("D:", "data.xls")

    checkEsalia(path).foreach(support = _)
    println(support)

    (new TestExcel).launch()
  }

  def checkEsalia(path: Path): Option[Support] = {
    Option(WorkbookFactory.create(path.toFile)).filter { book =>
      // There should only be one sheet
      book.getNumberOfSheets == 1
    }.map { book =>
      book.getSheetAt(0)
    }.filter { sheet =>
      // Rows 0 to 5 contain general information
      // Rows 6 and beyond contain dated values
      sheet.getSheetName.startsWith("Historique des valeurs liquida") &&
        (sheet.getFirstRowNum == 0) && (sheet.getLastRowNum >= 6)
    }.flatMap { sheet =>
      // Row 1 contains the support name
      // Row 5 indicates which data are listed: cell 4 shall contain the date
      // and cell 5 the value at the given date
      val dateCellIdx = 4
      val valueCellIdx = 5
      val row1 = sheet.getRow(1)
      val row5 = sheet.getRow(5)

      if ((row1.getFirstCellNum == 0) && (row1.getLastCellNum >= 1) &&
        (row1.getCell(0).getStringCellValue == "Nom du fonds") &&
        (row5.getFirstCellNum == 0) && (row5.getLastCellNum >= 5) &&
        (row5.getCell(dateCellIdx).getStringCellValue == "Date VL") &&
        (row5.getCell(valueCellIdx).getStringCellValue == "VL"))
      {
        val dateParser = new SimpleDateFormat("dd/MM/yyyy")
        val name = row1.getCell(1).getStringCellValue
        val values = (6 to sheet.getLastRowNum).toList.map { rowIdx =>
          val row = sheet.getRow(rowIdx)
          AssetValue(
            date = dateParser.parse(row.getCell(dateCellIdx).getStringCellValue),
            value = row.getCell(valueCellIdx).getNumericCellValue
          )
        }
        Some(Support(name, values))
      } else {
        None
      }
    }
  }

}

class TestExcel extends Application {

  import TestExcel._

  var previousXPos: Option[String] = None
  var xZoomPos1: Option[String] = None
  var xZoomPos2: Option[String] = None

  val verticalLine = new Line(0, 0, 0, 0)
  verticalLine.setStrokeWidth(0.5)
  verticalLine.setVisible(false)
  // Note: it is important to disable the line, otherwise it will somehow
  // steal mouse pointer and trigger spurious onMouseExited/onMouseEntered
  // events on underlying chart.
  verticalLine.setDisable(true)

  val horizontalLine = new Line(0, 0, 0, 0)
  horizontalLine.setStrokeWidth(0.5)
  horizontalLine.setVisible(false)
  horizontalLine.setDisable(true)

  def launch() {
    Application.launch()
  }

  override def start(stage: Stage) {
    val xAxis = new CategoryAxis()
    xAxis.setLabel("Date")

    val dateFormat = new SimpleDateFormat("yyyy-MM-dd")

    val yAxis = new NumberAxis()
    yAxis.setLabel("VL")
    yAxis.setTickLabelFormatter(new NumberAxis.DefaultFormatter(yAxis, null, "€"))
    yAxis.setForceZeroInRange(false)
    yAxis.setAutoRanging(true)

    val series = new XYChart.Series[String, Number]()
    series.setName(support.name)
    val valuesList = support.values.takeRight(100).map { v =>
      (dateFormat.format(v.date), v.value)
    }
    val valuesMap = valuesList.toMap
    for (value <- valuesList) {
      series.getData.add(
        new XYChart.Data[String, Number](
          value._1,
          value._2
        )
      )
    }

    val chart = new LineChart[String, Number](xAxis, yAxis)
    chart.getStylesheets.add("test.css")
    chart.getStyleClass.add("custom-chart")
    chart.setTitle("Valeurs liquidatives")
    chart.setCreateSymbols(false)
    chart.setPrefSize(640, 480)
    chart.getData.add(series)
    val chartBackground = chart.lookup(".chart-plot-background").asInstanceOf[Region]

    val chartPane = new Pane()
    chartPane.getChildren.addAll(chart, verticalLine, horizontalLine)
    // Note: it is not a good idea to track mouse from chartBackground, since
    // crossing any displayed element (e.g. grid) will trigger exited/entered.
    // Better track mouse on chart, and check whether it is over the graph.
    chart.setOnMouseEntered(onMouseEntered(chartPane, chartBackground) _)
    chart.setOnMouseExited(onMouseExited _)
    chart.setOnMouseMoved(onMouseMoved(chartPane, chartBackground, xAxis, yAxis, valuesMap) _)
    chart.setOnMousePressed(onMousePressed(chartPane, chartBackground, xAxis) _)
    chart.setOnMouseReleased(onMouseReleased _)
    chart.setOnMouseDragged(onMouseDragged(chartPane, chartBackground, xAxis, yAxis, valuesMap) _)

    val scene = new Scene(chartPane)
    stage.setScene(scene)
    stage.show()
  }

  private def onMouseEntered(chartPane: Pane, chartBackground: Region)(event: MouseEvent): Unit = {
    // Note: we need to determine the position of the chart background
    // relatively to the vertical line parent.
    val bounds = getBounds(chartBackground, chartPane)
    verticalLine.setStartY(bounds.getMinY)
    verticalLine.setEndY(bounds.getMinY + bounds.getHeight)
  }

  private def onMouseExited(event: MouseEvent): Unit = {
    verticalLine.setVisible(false)
    horizontalLine.setVisible(false)
  }

  private def onMouseMoved(chartPane: Pane, chartBackground: Region, xAxis: CategoryAxis, yAxis: NumberAxis, valuesMap: Map[String, Double])(event: MouseEvent): Unit = {
    val bounds = getBounds(chartBackground, chartPane)

    if (bounds.contains(event.getX, event.getY)) {
      // Note: mouse position is relative to the chart, while xAxis works
      // relatively to the background. So adjust.
      Option(xAxis.getValueForDisplay(event.getX - bounds.getMinX)).foreach { xPos =>
        if (!previousXPos.contains(xPos)) {
          val x = bounds.getMinX + xAxis.getDisplayPosition(xPos)
          val y = bounds.getMinY + yAxis.getDisplayPosition(valuesMap(xPos))
          verticalLine.setStartX(x)
          verticalLine.setEndX(x)
          verticalLine.setVisible(true)
          horizontalLine.setStartX(bounds.getMinX)
          horizontalLine.setEndX(x)
          horizontalLine.setStartY(y)
          horizontalLine.setEndY(y)
          horizontalLine.setVisible(true)
        }
        previousXPos = Some(xPos)
      }
    } else {
      verticalLine.setVisible(false)
      horizontalLine.setVisible(false)
    }
  }

  private def onMousePressed(chartPane: Pane, chartBackground: Region, xAxis: CategoryAxis)(event: MouseEvent): Unit = {
    val bounds = getBounds(chartBackground, chartPane)

    if (bounds.contains(event.getX, event.getY)) {
      // Note: mouse position is relative to the chart, while xAxis works
      // relatively to the background. So adjust.
      Option(xAxis.getValueForDisplay(event.getX - bounds.getMinX)).foreach { xPos =>
        xZoomPos1 = Some(xPos)
      }
    }
  }

  private def onMouseReleased(event: MouseEvent): Unit = {
    for {
      pos1 <- xZoomPos1
      pos2 <- xZoomPos2
    } {
      println(s"xZoomPos1: $pos1; xZoomPos2: $pos2")
    }

    xZoomPos1 = None
    xZoomPos2 = None
  }

  private def onMouseDragged(chartPane: Pane, chartBackground: Region, xAxis: CategoryAxis, yAxis: NumberAxis, valuesMap: Map[String, Double])(event: MouseEvent): Unit = {
    val bounds = getBounds(chartBackground, chartPane)

    if (bounds.contains(event.getX, event.getY)) {
      // Note: mouse position is relative to the chart, while xAxis works
      // relatively to the background. So adjust.
      Option(xAxis.getValueForDisplay(event.getX - bounds.getMinX)).foreach { xPos =>
        xZoomPos2 = Some(xPos)
      }
    }

    onMouseMoved(chartPane, chartBackground, xAxis, yAxis, valuesMap)(event)
  }

  private def getBounds(node: Node, root: Node): Bounds = {
    @scala.annotation.tailrec
    def loop(bounds: Option[Bounds], node: Node): Bounds = {
      val boundsActual = bounds.getOrElse(node.getBoundsInLocal)
      Option(node.getParent).filterNot(_ eq root) match {
        case Some(parent) => loop(Some(node.localToParent(boundsActual)), parent)
        case None => boundsActual
      }
    }

    loop(None, node)
  }

}

case class AssetValue(date: Date, value: Double)
case class Support(name: String, values: List[AssetValue] = Nil)
