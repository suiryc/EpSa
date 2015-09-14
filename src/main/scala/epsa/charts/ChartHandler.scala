package epsa.charts

import epsa.model.InvestmentFund
import java.text.SimpleDateFormat
import javafx.geometry.Bounds
import javafx.scene.Node
import javafx.scene.chart.{CategoryAxis, LineChart, NumberAxis, XYChart}
import javafx.scene.input.{MouseEvent, ScrollEvent}
import javafx.scene.layout.{AnchorPane, Region}
import javafx.scene.paint.Color
import javafx.scene.shape.{Line, Rectangle}
import suiryc.scala.concurrent.Cancellable
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.event.EventHandler._

/**
 * Handles chart for a given investment fund.
 *
 * Creates said chart, and manages associated resources:
 *   - zooming with mouse selection (dragging or scrolling)
 *   - displaying chart data value in label
 *   - draw visible lines to spot chart data value
 */
class ChartHandler(fund: InvestmentFund) {

  /** Chart 'x' axis. */
  private val xAxis = new CategoryAxis()
  xAxis.setLabel("Date")

  /** Date format for 'x' axis. */
  private val dateFormat = new SimpleDateFormat("yyyy-MM-dd")

  /** Chart 'y' axis. */
  private val yAxis = new NumberAxis()
  yAxis.setLabel("VL")
  yAxis.setTickLabelFormatter(new NumberAxis.DefaultFormatter(yAxis, null, "€"))
  yAxis.setForceZeroInRange(false)
  yAxis.setAutoRanging(true)

  /** Chart series. */
  private val series = new XYChart.Series[String, Number]()
  series.setName(fund.name)
  /** Investment fund asset values to display in chart. */
  // TODO - limited to a given number of values for testing
  private val valuesList = fund.values.takeRight(200).map { v =>
    (dateFormat.format(v.date), v.value)
  }
  /** Number of investment fund asset values. */
  private val valuesCount = valuesList.length
  /** Investment fund asset values map. */
  private val valuesMap = valuesList.toMap
  /** How many values to drop on the left side to populate chart. */
  private var xDropLeft = 0
  /** How many values to drop on the right side to populate chart. */
  private var xDropRight = 0
  /** Minimum number of values to display. That is, we cannot zoom more than that. */
  private val minValues = 20
  /** Currently displayed 'x' data value. */
  private var currentXPos: Option[String] = None
  /** Zoom 'x' first selected value. */
  private var xZoomPos1: Option[String] = None
  /** Zoom 'x' second selected value. */
  private var xZoomPos2: Option[String] = None

  /** The chart. */
  private val chart = new LineChart[String, Number](xAxis, yAxis)
  // We don't really need animation
  chart.setAnimated(false)
  chart.getStylesheets.add(getClass.getResource("/css/chart-investment-fund.css").toExternalForm)
  chart.getStyleClass.add("custom-chart")
  chart.setTitle("Valeurs liquidatives")
  chart.setCreateSymbols(false)
  chart.getData.add(series)

  /** Chart background. */
  private val chartBg = chart.lookup(".chart-plot-background").asInstanceOf[Region]
  /** Chart background bounds. */
  private var chartBgBounds: Option[Bounds] = None
  // Listen to chart background changes to invalidate cached bounds value.
  chartBg.boundsInParentProperty.listen {
    chartBgBounds = None
    // Note: since bounds are changing (which is why we reset chartBgBounds)
    // is is important to make sure we are redrawing the reference lines after
    // recomputing it.
    // e.g. listening to the chart (or its parent) resizing does not work as
    // expected, probably due to relying on the bounds value known right
    // before resizing.
    drawReferenceLines()
  }

  /** Vertical line to spot currently display 'x' data value. */
  private val verticalLine = new Line(0, 0, 0, 0)
  verticalLine.setStrokeWidth(0.5)
  verticalLine.setVisible(false)
  // Note: it is important to disable the line, otherwise it will somehow
  // steal mouse pointer and trigger spurious onMouseExited/onMouseEntered
  // events on underlying chart.
  verticalLine.setDisable(true)

  /** Horizontal line to spot currently display 'y' data value. */
  private val horizontalLine = new Line(0, 0, 0, 0)
  horizontalLine.setStrokeWidth(0.5)
  horizontalLine.setVisible(false)
  horizontalLine.setDisable(true)

  /** Vertical line to spot previously selected 'x' data value. */
  private val verticalLineRef = new Line(0, 0, 0, 0)
  verticalLineRef.setStroke(Color.GREY)
  verticalLineRef.setStrokeWidth(0.5)
  verticalLineRef.setVisible(false)
  verticalLineRef.setDisable(true)

  /** Horizontal line to spot previously selected 'y' data value. */
  private val horizontalLineRef = new Line(0, 0, 0, 0)
  horizontalLineRef.setStroke(Color.GREY)
  horizontalLineRef.setStrokeWidth(0.5)
  horizontalLineRef.setVisible(false)
  horizontalLineRef.setDisable(true)

  /** Zoom zone 'highlight' (actually darkens selected zone). */
  private val zoomZone = new Rectangle()
  zoomZone.setFill(Color.BLACK)
  zoomZone.setStyle("-fx-opacity: 0.2;")
  zoomZone.setVisible(false)
  zoomZone.setDisable(true)

  /** Chart data label to display value. */
  private val labelVL = new ChartDataLabel()
  labelVL.getStyleClass.addAll("default-color0", "chart-line-symbol", "chart-series-line")
  labelVL.setStyle("-fx-font-size: 14; -fx-opacity: 0.6;")
  labelVL.setMinSize(Region.USE_PREF_SIZE, Region.USE_PREF_SIZE)
  labelVL.setVisible(false)
  labelVL.setDisable(true)
  /** Label listerning subscription. */
  private var labelVLCancellable: List[Cancellable] = Nil

  setData()

  /** Chart pane. */
  val chartPane = new AnchorPane()
  AnchorPane.setTopAnchor(chart, 0.0)
  AnchorPane.setRightAnchor(chart, 0.0)
  AnchorPane.setBottomAnchor(chart, 0.0)
  AnchorPane.setLeftAnchor(chart, 0.0)
  chartPane.getChildren.addAll(chart, zoomZone, verticalLineRef, horizontalLineRef, verticalLine, horizontalLine, labelVL)
  // Note: it is not a good idea to track mouse from chartBg, since
  // crossing any displayed element (e.g. grid) will trigger exited/entered.
  // Better track mouse on chart, and check whether it is over the graph.
  chart.setOnMouseEntered(onMouseEntered _)
  chart.setOnMouseExited(onMouseExited _)
  chart.setOnMouseMoved(onMouseMoved _)
  chart.setOnMousePressed(onMousePressed _)
  chart.setOnMouseReleased(onMouseReleased _)
  chart.setOnMouseDragged(onMouseDragged _)
  chart.setOnScroll(onScroll _)

  /** Gets 'node' bounds relatively to 'root' node. */
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

  /** Gets, aned caches, chart background bounds. */
  private def getChartBackgroundBounds: Bounds = {
    if (chartBgBounds.isEmpty) {
      chartBgBounds = Some(getBounds(chartBg, chartPane))
    }

    chartBgBounds.get
  }

  /** Calls given function if mouse is inside chart background. */
  private def withMouseInChartBackground[A](x: Double, y: Double)(f: Bounds => A): Option[A] = {
    val bounds = getChartBackgroundBounds

    if (bounds.contains(x, y)) {
      Some(f(bounds))
    } else {
      None
    }
  }

  /** Calls given function if mouse is inside chart background. */
  private def withMouseInChartBackground[A](event: MouseEvent)(f: Bounds => A): Option[A] =
    withMouseInChartBackground(event.getX, event.getY)(f)

  /** Calls given function if mouse is inside chart background. */
  private def withMouseInChartBackground[A](event: ScrollEvent)(f: Bounds => A): Option[A] =
    withMouseInChartBackground(event.getX, event.getY)(f)

  /** Gets index of a given 'x' value. */
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

  /**
   * Populates the chart series with appropriate data.
   *
   * Drops some data left and right (according to currently selected zone
   * if any).
   */
  private def setData(): Unit = {
    val data = valuesList.drop(xDropLeft).dropRight(xDropRight).map { case (valueX, valueY) =>
      new XYChart.Data[String, Number](valueX, valueY)
    }

    // Since we are about to change the chart data, hide lines
    hideLines(clearRef = true)

    // Note: clearing series data while chart is animated triggers an Exception
    // See: http://stackoverflow.com/a/30396889
    series.getData.clear()
    series.getData.setAll(data : _*)
  }

  /** Hides zoom 'highlight' area. */
  private def hideZoomArea(): Unit = {
    if (zoomZone.isVisible) {
      zoomZone.setVisible(false)
      zoomZone.setWidth(0)
      zoomZone.setHeight(0)
    }
    xZoomPos1 = None
    xZoomPos2 = None
  }

  /**
   * Hides visible lines.
   *
   * Includes reference and current data value lines, as well as label.
   */
  private def hideLines(clearRef: Boolean): Unit = {
    hideZoomArea()
    labelVLCancellable.foreach(_.cancel())
    labelVLCancellable = Nil
    labelVL.setVisible(false)
    horizontalLine.setVisible(false)
    verticalLine.setVisible(false)
    // Sometimes (exiting/leaving chart) we don't want to reset the reference value
    if (clearRef) {
      horizontalLineRef.setVisible(false)
      verticalLineRef.setVisible(false)
      labelVL.setDataRef(None)
    }
    // Reset previous position, so that it can be redrawn if we re-enter
    currentXPos = None
  }

  /** Gets chart 'x' value for given position. */
  private def getX(bounds: Bounds, x: Double): Option[String] =
  // Note: x is relative to the chart, while xAxis works
  // relatively to the background. So adjust.
    Option(xAxis.getValueForDisplay(x - bounds.getMinX))

  /** Gets 'x' position for given chart value. */
  private def getX(bounds: Bounds, xPos: String): Double =
  // Note: x is relative to the chart, while xAxis works
  // relatively to the background. So adjust.
    bounds.getMinX + xAxis.getDisplayPosition(xPos)

  /** Gets 'x' and 'y' position for given chart value. */
  def getXY(bounds: Bounds, xPos: String): (Double, Double) = {
    val x = getX(bounds, xPos)
    val y = bounds.getMinY + yAxis.getDisplayPosition(valuesMap(xPos))
    (x, y)
  }

  // Note: in general it is considered bad practice to modify a value from
  // within its listener.
  // Would require to delay all actions on label ? (to prevent concurrency
  // issues between immediate ones - mouse being moved - and delayed ones)

  /**
   * Sets label 'x' default position.
   *
   * Default position is where the middle of the label is the currently
   * displayed 'x' chart value.
   */
  def setLabelX() = if (labelVL.isVisible) {
    val bounds = getChartBackgroundBounds

    currentXPos.map(getXY(bounds, _)) match {
      case Some((x, _)) =>
        labelVL.setTranslateX(x - labelVL.getWidth / 2)

      case None =>
      // No current position to set label on
    }
  }

  /**
   * Sets label 'y' default position.
   *
   * Default position is where the bottom of the label is above the currently
   * displayed 'y' chart value by 10 pixels.
   */
  def setLabelY() = if (labelVL.isVisible) {
    val bounds = getChartBackgroundBounds

    currentXPos.map(getXY(bounds, _)) match {
      case Some((_, y)) =>
        labelVL.setTranslateY(y - labelVL.getHeight - 10)

      case None =>
      // No current position to set label on
    }
  }

  /**
   * Checks label position.
   *
   * Makre sure it remains inside the chart background, and if possible does not
   * 'hide' the currently displayed chart data value.
   */
  def checkLabelPosition() = if (labelVL.isVisible) {
    val bounds = getChartBackgroundBounds

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
      currentXPos.map(getXY(bounds, _)) match {
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

  /** Draws reference value lines. */
  private def drawReferenceLines(xPos: Option[String] = None): Unit = {
    xPos.orElse(labelVL.getDataRef.map(_.x)).foreach { xPos =>
      val bounds = getChartBackgroundBounds
      val (x, y) = getXY(bounds, xPos)

      verticalLineRef.setStartX(x)
      verticalLineRef.setEndX(x)
      verticalLineRef.setStartY(y)
      verticalLineRef.setEndY(bounds.getMaxY)
      verticalLineRef.setVisible(true)
      horizontalLineRef.setStartX(bounds.getMinX)
      horizontalLineRef.setEndX(x)
      horizontalLineRef.setStartY(y)
      horizontalLineRef.setEndY(y)
      horizontalLineRef.setVisible(true)
    }
  }

  /**
   * Draws currently displayed chart value lines.
   *
   * Also updates zoom 'highlight' zone and label.
   */
  private def drawLines(event: MouseEvent, xPos: Option[String] = None): Unit = {
    val bounds = getChartBackgroundBounds

    xPos.orElse(getX(bounds, event.getX)).foreach { xPos =>
      val (x, y) = getXY(bounds, xPos)

      xZoomPos1.foreach { xPos1 =>
        val x1 = getX(bounds, xPos1)
        if (xPos >= xPos1) {
          zoomZone.setX(x1)
          zoomZone.setWidth(x - x1)
        } else {
          // going left: needs to change x position
          zoomZone.setX(x)
          zoomZone.setWidth(x1 - x)
        }
      }

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

      if (labelVLCancellable.isEmpty) {
        // Listen for position and dimension changes to check the label remains inside the chart
        val s1 = labelVL.boundsInParentProperty.listen(checkLabelPosition())
        // Listen to width and height changes to place the label at the right position
        val s2 = labelVL.widthProperty.listen(setLabelX())
        val s3 = labelVL.heightProperty.listen(setLabelY())
        labelVLCancellable = List(s1, s2, s3)
      }

      if (!labelVL.isVisible) {
        labelVL.setVisible(true)
      }

      labelVL.setData(ChartData(xPos, valuesMap(xPos)))
      setLabelX()
      setLabelY()
    }
  }

  /**
   * onMouseEntered listener.
   *
   * Draws lines.
   */
  private def onMouseEntered(event: MouseEvent): Unit = {
    drawLines(event)
  }

  /**
   * onMouseExited listener.
   *
   * Hides lines.
   */
  private def onMouseExited(event: MouseEvent): Unit = {
    hideLines(clearRef = false)
  }

  /**
   * onMouseMoved listener.
   *
   * Draws or hides lines depending on whether mouse is inside chart background.
   */
  private def onMouseMoved(event: MouseEvent): Unit = {
    val bounds = getChartBackgroundBounds

    if (bounds.contains(event.getX, event.getY)) {
      getX(bounds, event.getX).foreach { xPos =>
        if (!currentXPos.contains(xPos)) {
          currentXPos = Some(xPos)
          drawLines(event, currentXPos)
        }
      }
    } else {
      hideLines(clearRef = false)
    }
  }

  /**
   * onMousePressed listener.
   *
   * Remembers selected 'x' value as reference and zoom first selected value.
   * Draws reference value lines.
   */
  private def onMousePressed(event: MouseEvent): Unit = {
    withMouseInChartBackground(event) { bounds =>
      getX(bounds, event.getX).foreach { xPos =>
        val (x, y) = getXY(bounds, xPos)

        xZoomPos1 = Some(xPos)
        labelVL.setDataRef(Some(ChartData(xPos, valuesMap(xPos))))
        drawReferenceLines(xZoomPos1)

        zoomZone.setX(getX(bounds, xPos))
        zoomZone.setWidth(0)
        zoomZone.setY(bounds.getMinY)
        zoomZone.setHeight(bounds.getMaxY - bounds.getMinY)
        zoomZone.setVisible(true)
      }
    }
  }

  /**
   * Ensures we keep the minimum number of values to display.
   *
   * If necessary, lower the number of dropped values left and right.
   */
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

  /**
   * onMouseReleased listener.
   *
   * Determines zoom range from first and second selected 'x' values.
   * Displays selected range values if different from previous one.
   */
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
          if (((dropLeft != xDropLeft) || (dropRight != xDropRight)) &&
            (valuesCount - xDropLeft - xDropRight > minValues))
          {
            xDropLeft = dropLeft
            xDropRight = dropRight
            ensureMinValues()
            setData()
          }
          // else: either we selected the same zone, or we cannot zoom anymore
        }
      }
    }

    hideZoomArea()
  }

  /**
   * onScroll listener.
   *
   * Determines zoom factor from scroll direction.
   * Displays selected range values if different from previous one.
   */
  private def onScroll(event: ScrollEvent): Unit = {
    val deltaY = (event.getDeltaY / event.getMultiplierY).toInt
    if (deltaY != 0) withMouseInChartBackground(event) { bounds =>
      getX(bounds, event.getX).foreach { xPos =>
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

  /**
   * onMouseDragged listener.
   *
   * Remembers zoom second 'x' value.
   * Calls 'onMouseMoved' listening code.
   */
  private def onMouseDragged(event: MouseEvent): Unit = {
    withMouseInChartBackground(event) { bounds =>
      getX(bounds, event.getX).foreach { xPos =>
        xZoomPos2 = Some(xPos)
      }
    }

    onMouseMoved(event)
  }

}
