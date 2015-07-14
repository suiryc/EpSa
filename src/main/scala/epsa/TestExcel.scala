package epsa

import java.nio.file.{Path, Paths}
import java.text.SimpleDateFormat
import java.util.Date
import javafx.application.Application
import javafx.geometry.Bounds
import javafx.scene.{Node, Scene}
import javafx.scene.chart.{XYChart, LineChart, NumberAxis, CategoryAxis}
import javafx.scene.control.Label
import javafx.scene.input.{MouseEvent, ScrollEvent}
import javafx.scene.layout.{Pane, Region}
import javafx.scene.shape.Line
import javafx.stage.Stage
import org.apache.poi.ss.usermodel.WorkbookFactory
import suiryc.scala.concurrent.Cancellable
import suiryc.scala.javafx.beans.property.RichReadOnlyProperty
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

  def launch() {
    Application.launch()
  }

  override def start(stage: Stage) {
    val chartHandler = new ChartHandler(TestExcel.support)

    val scene = new Scene(chartHandler.chartPane)
    stage.setScene(scene)
    stage.show()
  }

}

case class AssetValue(date: Date, value: Double)
case class Support(name: String, values: List[AssetValue] = Nil)

class ChartHandler(support: Support) {

  var previousXPos: Option[String] = None
  var xZoomPos1: Option[String] = None
  var xZoomPos2: Option[String] = None
  var xDropLeft = 0
  var xDropRight = 0
  val minValues = 20

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

  val labelVL = new Label("")
  labelVL.getStyleClass.addAll("default-color0", "chart-line-symbol", "chart-series-line")
  labelVL.setStyle("-fx-font-size: 14; -fx-opacity: 0.6;")
  labelVL.setMinSize(Region.USE_PREF_SIZE, Region.USE_PREF_SIZE)
  labelVL.setVisible(false)
  labelVL.setDisable(true)
  var labelVLCancellable: Option[Cancellable] = None

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
  val valuesCount = valuesList.length
  val valuesMap = valuesList.toMap

  val chart = new LineChart[String, Number](xAxis, yAxis)
  // We don't really need animation
  chart.setAnimated(false)
  chart.getStylesheets.add("test.css")
  chart.getStyleClass.add("custom-chart")
  chart.setTitle("Valeurs liquidatives")
  chart.setCreateSymbols(false)
  chart.setPrefSize(640, 480)
  chart.getData.add(series)
  val chartBackground = chart.lookup(".chart-plot-background").asInstanceOf[Region]

  setData()

  val chartPane = new Pane()
  chartPane.getChildren.addAll(chart, verticalLine, horizontalLine, labelVL)
  // Note: it is not a good idea to track mouse from chartBackground, since
  // crossing any displayed element (e.g. grid) will trigger exited/entered.
  // Better track mouse on chart, and check whether it is over the graph.
  chart.setOnMouseEntered(onMouseEntered _)
  chart.setOnMouseExited(onMouseExited _)
  chart.setOnMouseMoved(onMouseMoved _)
  chart.setOnMousePressed(onMousePressed _)
  chart.setOnMouseReleased(onMouseReleased _)
  chart.setOnMouseDragged(onMouseDragged _)
  chart.setOnScroll(onScroll _)

  private def getValueIndex(x: String): Int = {
    @scala.annotation.tailrec
    def loop(values: List[(String, Double)], idx: Int): Int = values.headOption match {
      case Some((valueX, _)) =>
        if (valueX == x) {
          idx
        } else {
          loop(values.tail, idx + 1)
        }

      case None =>
        -1
    }

    loop(valuesList, 0)
  }

  private def setData(): Unit = {
    val data = valuesList.drop(xDropLeft).dropRight(xDropRight).map { case (valueX, valueY) =>
      new XYChart.Data[String, Number](valueX, valueY)
    }

    // Since we are about to change the chart data, hide lines
    hideLines()

    // Note: clearing series data while chart is animated triggers an Exception
    // See: http://stackoverflow.com/a/30396889
    series.getData.clear()
    series.getData.setAll(data : _*)
  }

  private def hideLines(): Unit = {
    labelVLCancellable.foreach(_.cancel())
    labelVLCancellable = None
    labelVL.setVisible(false)
    labelVL.setText("")
    verticalLine.setVisible(false)
    horizontalLine.setVisible(false)
    // Reset previous position, so that it can be redrawn if we re-enter
    previousXPos = None
  }

  private def drawLines(event: MouseEvent, obounds: Option[Bounds] = None, xPos: Option[String] = None): Unit = {
    val bounds = obounds.getOrElse(getBounds(chartBackground, chartPane))

    def getXY(xPos: String): (Double, Double) = {
      val x = bounds.getMinX + xAxis.getDisplayPosition(xPos)
      val y = bounds.getMinY + yAxis.getDisplayPosition(valuesMap(xPos))
      (x, y)
    }

    // Note: mouse position is relative to the chart, while xAxis works
    // relatively to the background. So adjust.
    xPos.orElse(Option(xAxis.getValueForDisplay(event.getX - bounds.getMinX))).foreach { xPos =>
      val (x, y) = getXY(xPos)
      verticalLine.setStartX(x)
      verticalLine.setEndX(x)
      verticalLine.setStartY(y)
      verticalLine.setEndY(bounds.getMaxY)
      verticalLine.setVisible(true)
      horizontalLine.setStartX(bounds.getMinX)
      horizontalLine.setEndX(x)
      horizontalLine.setStartY(y)
      horizontalLine.setEndY(y)
      horizontalLine.setVisible(true)

      def setLabelX() = if (labelVL.isVisible) {
        labelVL.setTranslateX(x - labelVL.getWidth / 2)
      }

      def setLabelY() = if (labelVL.isVisible) {
        labelVL.setTranslateY(y - labelVL.getHeight - 10)
      }

      def checkLabelPosition() = if (labelVL.isVisible) {
        val xy = previousXPos.map(getXY)
        if (labelVL.getLayoutX + labelVL.getTranslateX + labelVL.getWidth > bounds.getMaxX) {
          // right end of label is going beyond chart
          labelVL.setTranslateX(bounds.getMaxX - labelVL.getLayoutX - labelVL.getWidth)
        } else if (labelVL.getLayoutX + labelVL.getTranslateX < bounds.getMinX) {
          // left end of label is going beyond chart
          labelVL.setTranslateX(bounds.getMinX - labelVL.getLayoutX)
        }
        if (labelVL.getLayoutY + labelVL.getTranslateY + labelVL.getHeight > bounds.getMaxY) {
          // bottom end of label is going beyond chart
          labelVL.setTranslateY(bounds.getMaxY - labelVL.getLayoutY - labelVL.getHeight)
        } else if (labelVL.getLayoutY + labelVL.getTranslateY < bounds.getMinY) {
          // top end of label is going beyond chart
          xy match {
            case Some((_, currentY)) =>
              // We don't want to go above the chart top, but would like not to
              // display the label in front of the chart point, unless that make
              // it go beyond the chart bottom.
              if ((bounds.getMinY + labelVL.getHeight < currentY) || (currentY + 10 + labelVL.getHeight > bounds.getMaxY)) {
                // We still remain above the displayed chart point, or would go
                // beyond the chart bottom by displaying the label underneath it.
                // So just go at the top of the chart.
                labelVL.setTranslateY(bounds.getMinY - labelVL.getLayoutY)
              } else {
                // chart point will be under the label, move it underneath
                labelVL.setTranslateY(currentY + 10)
              }

            case None =>
              labelVL.setTranslateY(bounds.getMinY - labelVL.getLayoutY)
          }
        }
      }

      if (labelVLCancellable.isEmpty) {
        import RichReadOnlyProperty._

        // Listen for position and dimension changes to check the label remains inside the chart
        val s1 = labelVL.boundsInParentProperty.listen(checkLabelPosition())
        // Listen to width and height changes to place the label at the right position
        val s2 = labelVL.widthProperty.listen(s1, setLabelX())
        val s3 = labelVL.heightProperty.listen(s2, setLabelY())
        labelVLCancellable = Some(s3)
      }

      if (!labelVL.isVisible) {
        labelVL.setVisible(true)
      }
      labelVL.setText(s"Date: $xPos\nVL: ${valuesMap(xPos)}€")
      setLabelX()
      setLabelY()
    }
  }

  private def onMouseEntered(event: MouseEvent): Unit = {
    drawLines(event)
  }

  private def onMouseExited(event: MouseEvent): Unit = {
    hideLines()
  }

  private def onMouseMoved(event: MouseEvent): Unit = {
    val bounds = getBounds(chartBackground, chartPane)

    if (bounds.contains(event.getX, event.getY)) {
      // Note: mouse position is relative to the chart, while xAxis works
      // relatively to the background. So adjust.
      Option(xAxis.getValueForDisplay(event.getX - bounds.getMinX)).foreach { xPos =>
        if (!previousXPos.contains(xPos)) {
          previousXPos = Some(xPos)
          drawLines(event, Some(bounds), previousXPos)
        }
      }
    } else {
      hideLines()
    }
  }

  private def onMousePressed(event: MouseEvent): Unit = {
    val bounds = getBounds(chartBackground, chartPane)

    if (bounds.contains(event.getX, event.getY)) {
      // Note: mouse position is relative to the chart, while xAxis works
      // relatively to the background. So adjust.
      Option(xAxis.getValueForDisplay(event.getX - bounds.getMinX)).foreach { xPos =>
        xZoomPos1 = Some(xPos)
      }
    }
  }

  private def ensureMinValues(): Unit = {
    @scala.annotation.tailrec
    def loop(left: Boolean) {
      if (valuesCount - xDropLeft - xDropRight < minValues) {
        if ((left || (xDropRight == 0)) && (xDropLeft > 0)) {
          xDropLeft -= 1
          loop(left = false)
        } else if (xDropRight > 0) {
          xDropRight -= 1
          loop(left = true)
        }
      }
    }
    loop(left = true)
  }

  private def onMouseReleased(event: MouseEvent): Unit = {
    def getDrops(xZoomPos1: String, xZoomPos2: String): Option[(Int, Int)] = {
      val (xZoomFrom, xZoomTo) = if (xZoomPos1 < xZoomPos2) {
        (xZoomPos1, xZoomPos2)
      } else {
        (xZoomPos2, xZoomPos1)
      }

      @scala.annotation.tailrec
      def loop(values: List[(String, Double)], idx: Int, xDropLeft: Option[Int]): Option[(Int, Int)] = {
        values.headOption match {
          case Some((valueX, _)) =>
            xDropLeft match {
              case None =>
                if (xZoomFrom == valueX) {
                  loop(values.tail, idx + 1, Some(idx))
                } else {
                  loop(values.tail, idx + 1, None)
                }

              case Some(left) =>
                if (xZoomTo == valueX) {
                  Some(left, values.tail.length - 1)
                } else {
                  loop(values.tail, idx + 1, xDropLeft)
                }
            }

          case None =>
            xDropLeft.map(left => (left, 0))
        }
      }

      loop(valuesList, 0, None)
    }

    for {
      pos1 <- xZoomPos1
      pos2 <- xZoomPos2
    } {
      if (pos1 != pos2) {
        getDrops(pos1, pos2).foreach { case (dropLeft, dropRight) =>
          if ((dropLeft != xDropLeft) || (dropRight != xDropRight)) {
            xDropLeft = dropLeft
            xDropRight = dropRight
            ensureMinValues()
            setData()
          }
        }
      }
    }

    xZoomPos1 = None
    xZoomPos2 = None
  }

  private def onScroll(event: ScrollEvent): Unit = {
    val bounds = getBounds(chartBackground, chartPane)
    val deltaY = (event.getDeltaY / event.getMultiplierY).toInt

    if ((deltaY != 0) && bounds.contains(event.getX, event.getY)) {
      // Note: mouse position is relative to the chart, while xAxis works
      // relatively to the background. So adjust.
      Option(xAxis.getValueForDisplay(event.getX - bounds.getMinX)).foreach { xPos =>
        val zoomCenterIdx = getValueIndex(xPos)
        val redraw = if (deltaY > 0) {
          // Zoom in
          // Note: keep at least minValues values on screen
          if (valuesCount - xDropLeft - xDropRight > minValues) {
            xDropLeft = math.min(math.max(0, zoomCenterIdx - minValues / 2), zoomCenterIdx - (zoomCenterIdx - xDropLeft) / (2 * deltaY))
            xDropRight = valuesCount - math.max(math.min(valuesCount, zoomCenterIdx + minValues / 2), zoomCenterIdx + (valuesCount - xDropRight - zoomCenterIdx) / (2 * deltaY))

            ensureMinValues()

            true
          } else {
            false
          }
        } else {
          // Zoom out
          if ((xDropLeft > 0) || (xDropRight > 0)) {
            xDropLeft = math.max(0, zoomCenterIdx - (zoomCenterIdx - xDropLeft) * (-3 * deltaY) / 2)
            xDropRight = valuesCount - math.min(valuesCount, zoomCenterIdx + (valuesCount - xDropRight - zoomCenterIdx) * (-3 * deltaY) / 2)
            true
          } else {
            false
          }
        }

        if (redraw) {
          setData()
        }
      }
    }
  }

  private def onMouseDragged(event: MouseEvent): Unit = {
    val bounds = getBounds(chartBackground, chartPane)

    if (bounds.contains(event.getX, event.getY)) {
      // Note: mouse position is relative to the chart, while xAxis works
      // relatively to the background. So adjust.
      Option(xAxis.getValueForDisplay(event.getX - bounds.getMinX)).foreach { xPos =>
        xZoomPos2 = Some(xPos)
      }

      // XXX - grey out zoom zone
    }

    onMouseMoved(event)
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
