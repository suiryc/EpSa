package epsa.charts

import javafx.geometry.{Bounds, Orientation}
import javafx.scene.chart.NumberAxis.DefaultFormatter
import javafx.scene.chart.XYChart
import javafx.scene.control.ScrollBar
import javafx.scene.layout.{Priority, VBox}
import scala.math.BigDecimal.RoundingMode
import suiryc.scala.javafx.beans.value.RichObservableValue
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.scene.text.Fonts
import suiryc.scala.math.BigDecimals
import suiryc.scala.util.Cached

class ClippingChartWrapper[A <: ChartMark](override protected val chartHandler: ChartHandler[A]) extends ChartWrapper[A] {

  setupChart()

  /** Chart view scrollbar. */
  private val scrollbar = new ScrollBar
  scrollbar.setOrientation(Orientation.HORIZONTAL)
  scrollbar.setMin(0)
  scrollbar.setMax(100)
  private val scrollbarRange = scrollbar.getMax - scrollbar.getMin
  private val valueXRangeView = Cached { BigDecimals.round(chartBg.getWidth / xZoom) }

  /** Converts from scrollbar units to days. */
  private def fromScrollUnit(v: Double): Double = {
    (chartHandler.valueXRange - valueXRangeView.value) * v / scrollbarRange
  }

  /** Converts from days to scrollbar units. */
  private def toScrollUnit(v: Double): Double = {
    v * scrollbarRange / (chartHandler.valueXRange - valueXRangeView.value)
  }

  /** Wrapped pane. */
  override val wrappedPane: VBox = new VBox()
  // Notes:
  // Try to have the same rendering as a true ScrollPane.
  // Add "scroll-pane" class to parent Pane, and "viewport" to chart pane.
  // Nullify parent padding on scrollbar by moving and resizing it (ScrollPane
  // does resizeRelocate upon children layout).
  wrappedPane.getStyleClass.add("scroll-pane")
  chartPane.getStyleClass.add("viewport")
  wrappedPane.paddingProperty.listen {
    scrollbar.setTranslateX(-wrappedPane.getPadding.getLeft)
  }
  scrollbar.layoutBoundsProperty.listen {
    scrollbar.resize(wrappedPane.getWidth, scrollbar.getHeight)
  }
  VBox.setVgrow(chartPane, Priority.ALWAYS)
  VBox.setVgrow(scrollbar, Priority.NEVER)
  wrappedPane.getChildren.addAll(chartPane, scrollbar)

  // Place controls node at top center of chart view.
  // Notes: follow pane parent to redo things when necessary.
  // Don't do anything if our pane is not visible (has no parent).
  RichObservableValue.listen[AnyRef](
    wrappedPane.parentProperty, chartBg.layoutBoundsProperty, controlsNode.widthProperty
  ) {
    if (Option(wrappedPane.getParent).isDefined) {
      centerControlsNode()
    }
  }

  // Notes:
  // Clearing data when chart width changes is visually more appealing than
  // doing it when refreshing data. In the latter case, the chart is first
  // resized to fit the new width before being redrawn when we change the
  // data. In the former case the chart almost appear static (depending on
  // the range of data displayed) while only the grid appear resized forth
  // and back.
  // Removing the series is not good because it empties the chart.
  chart.widthProperty.listen { _ =>
    chartSeries.getData.clear()
  }
  // Note: we also override refreshChart to invalidate and refresh
  chartBg.widthProperty.listen { _ =>
    valueXRangeView.invalidate()
    refreshScroll()
  }
  scrollbar.valueProperty.listen { _ =>
    refreshView()
  }
  refreshScroll()

  override val chartViewBounds: Cached[Bounds] = Cached { chartPane.getLayoutBounds }
  // Listen to chart background changes to invalidate cached bounds value.
  chartPane.layoutBoundsProperty.listen {
    chartViewBounds.invalidate()
    resizeChart()
  }

  // Follow xAxis and yAxis changes to redraw reference lines.
  // Both lower bound and scale are used to determine the value position.
  RichObservableValue.listen(xAxis.lowerBoundProperty, xAxis.scaleProperty, yAxis.lowerBoundProperty, yAxis.scaleProperty) {
    drawReferenceLines()
    // We also need to redraw marks.
    drawMarks()
  }

  // We see a clipped view of the chart: rely on its bounds when zooming.
  override val chartZoomBounds: Cached[Bounds] = chartBgBounds

  /** Refreshes X axis (e.g. during pane chart resizing). */
  override protected def refreshXAxis(): Unit = {
    // Note: we are called by refreshChart, which we override to call
    // refreshScroll which also update ticks. So there is no need to update
    // ticks here.
  }

  /** Centers chart on requested 'x' index. */
  override def centerOnXIdx(xIdx: Long, track: Boolean = false): Unit = {
    changeView(track) {
      val xMin = math.max(chartHandler.valueXMin.toDouble, xIdx - valueXRangeView.value.toDouble / 2)
      val hvalue = toScrollUnit(xMin - chartHandler.valueXMin)
      scrollbar.setValue(hvalue)
    }
  }

  override def refreshChart(resetData: Boolean, clearRef: Boolean): Unit = {
    // Note: we actually are only called when zoom has changed, or when series
    // data have been changed. So we can invalidate the date range view.
    // We also need to refresh scroll and view.
    valueXRangeView.invalidate()
    super.refreshChart(resetData, clearRef)
    if (resetData) setupYAxisWidth()
    refreshScroll()
  }

  private def setupYAxisWidth(): Unit = {
    val formatter = Option(yAxis.getTickLabelFormatter).getOrElse(new DefaultFormatter(yAxis))
    val font = yAxis.getTickLabelFont
    // Try to determine the maximum Y formatted value width. With the formatter,
    // consider 4 values: raw min and max, and rounded min and max with the axis
    // precision.
    val yExtra = BigDecimal(1) / BigDecimal(10).pow(settings.yPrecision)
    val labelWidth = List(
      Fonts.textWidth(font, formatter.toString(chartHandler.valueYMin)),
      Fonts.textWidth(font, formatter.toString(chartHandler.valueYMin.setScale(0, RoundingMode.FLOOR) - yExtra)),
      Fonts.textWidth(font, formatter.toString(chartHandler.valueYMax)),
      Fonts.textWidth(font, formatter.toString(chartHandler.valueYMax.setScale(0, RoundingMode.CEILING) + yExtra))
    ).max.ceil
    // Axis is composed of (from left to right):
    //  - label
    //  - gap between label and ticks
    //  - tick
    //  - vertical axis line: 1 pixel
    // The parent pane already has padding on the left of the axis.
    yAxis.setPrefWidth(labelWidth + yAxis.getTickLabelGap + yAxis.getTickLength + 1)
  }

  private def refreshView(): Unit = {
    // Note: we need to enforce minimum and maximum value, especially when
    // viewing the whole chart in the view (no scrollbar needed).
    val viewXMin = math.max(chartHandler.valueXMin, math.round(chartHandler.valueXMin + fromScrollUnit(scrollbar.getValue - scrollbar.getMin)))
    val viewXMax = math.min(chartHandler.valueXMax, viewXMin + valueXRangeView.value)
    xAxis.setLowerBound(viewXMin.toDouble)
    xAxis.setUpperBound(viewXMax.toDouble)

    val seriesData = new java.util.LinkedList[XYChart.Data[Number, Number]]()
    @inline def addData(data: (Long, BigDecimal)): Unit = {
      seriesData.add(new XYChart.Data[Number, Number](data._1, data._2))
      ()
    }
    @scala.annotation.tailrec
    def filterData(previous: Option[(Long, BigDecimal)], remaining: Seq[(Long, BigDecimal)]): Unit = {
      if (remaining.nonEmpty) {
        val data = remaining.head
        val dataX = data._1
        if (dataX < viewXMin) {
          filterData(Some(data), remaining.tail)
        } else if (dataX == viewXMin) {
          addData(data)
          filterData(None, remaining.tail)
        } else {
          if (seriesData.isEmpty) previous.foreach(addData)
          addData(data)
          if (dataX < viewXMax) filterData(None, remaining.tail)
        }
      }
    }
    filterData(None, chartHandler.valuesList)

    // Note: clearing series data while chart is animated triggers an Exception
    // See: http://stackoverflow.com/a/30396889
    chartSeries.getData.clear()
    chartSeries.getData.setAll(seriesData)
    // Note:
    // Beware that sometimes the axis scales may not have changed yet, resulting
    // in possible visual glitches when relying on the current values.
    // Especially for markers, we have to make sure the computed position fits
    // in the chart.
    drawMarks()
  }

  private def refreshScroll(): Unit = {
    if (valueXRangeView.value >= chartHandler.valueXRange) {
      // If the view can display more than we have, hide the scrollbar.
      scrollbar.setVisible(false)
    } else {
      // Amount (relatively to scrollbar value range) of viewed content.
      scrollbar.setVisibleAmount(scrollbarRange * valueXRangeView.value / chartHandler.valueXRange)
      // We want a unit scroll (click on arrow) to move by 10% of the range
      // view: it would need 10 clicks to scroll past the current view.
      scrollbar.setUnitIncrement(toScrollUnit(valueXRangeView.value.toDouble) / 10)
      // We want a block scroll (click on bar empty part) to move by 70%
      // of the range view.
      scrollbar.setBlockIncrement(scrollbar.getUnitIncrement * 7)
      // Make sure the scrollbar is visible.
      scrollbar.setVisible(true)
    }
    refreshView()
    // Note: lower/upper bounds have been computed and set upon refreshing view.
    xAxisWrapper.updateTicks(xAxis.getLowerBound, xAxis.getUpperBound, xZoom)
  }

}
