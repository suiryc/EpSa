package epsa.charts

import javafx.geometry.Bounds
import javafx.scene.control.ScrollPane
import suiryc.scala.javafx.beans.value.RichObservableValue
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.geometry.BoundsEx
import suiryc.scala.javafx.scene.control.{ScrollOffsetPosition, Scrolls}
import suiryc.scala.math.BigDecimals.round
import suiryc.scala.util.Cached

class CompleteChartWrapper[A <: ChartMark](override protected val chartHandler: ChartHandler[A]) extends ChartWrapper[A] {

  setupChart()

  /** Wrapped pane. */
  override val wrappedPane = new ScrollPane()
  // Don't display vertical scrollbar (we resize to fit parent)
  wrappedPane.setVbarPolicy(ScrollPane.ScrollBarPolicy.NEVER)
  // Notes:
  // We need to fit the content height so that the chart is automatically
  // resized when necessary.
  // If we did not disable (set to 0) the minimum height of the chart, it could
  // not be resized down below a certain value even to fit the height of the
  // scroll pane, and we would have to manually change the chart pane height
  // (which is equivalent to a forced fitting).
  wrappedPane.setFitToHeight(true)
  wrappedPane.setContent(chartPane)

  // Move yAxis so that it stays on the left of the view, and place controls
  // node at top center of chart view.
  // Note: follow pane parent to redo things when necessary.
  // Don't do anything if our pane is not visible (has no parent).
  RichObservableValue.listen[AnyRef](
    wrappedPane.parentProperty, chart.widthProperty, wrappedPane.widthProperty,
    wrappedPane.hvalueProperty, wrappedPane.viewportBoundsProperty, controlsNode.widthProperty
  ) {
    if (Option(wrappedPane.getParent).isDefined) {
      centerControlsNode()
      yAxis.setTranslateX(chartViewBounds.value.getMinX)
    }
  }

  /** Gets, and caches, chart background viewed bounds. */
  override val chartViewBounds = Cached { BoundsEx.getViewedBounds(wrappedPane) }
  // Listen to chart background changes to invalidate cached bounds value.
  wrappedPane.viewportBoundsProperty.listen {
    chartViewBounds.invalidate()
    resizeChart()
  }
  wrappedPane.hvalueProperty.listen {
    chartViewBounds.invalidate()
  }
  wrappedPane.vvalueProperty.listen {
    chartViewBounds.invalidate()
  }

  // Redraw reference lines (especially horizontal one) so that it always
  // goes to the yAxis.
  yAxis.translateXProperty.listen {
    drawReferenceLines()
  }

  // We see a part of the chart through the ScrollPane: rely on its view bounds
  // when zooming.
  override val chartZoomBounds: Cached[Bounds] = chartViewBounds

  /** Refreshes X axis (e.g. during pane chart resizing). */
  override protected def refreshXAxis(): Unit = {
    xAxisWrapper.updateTicks(chartHandler.valueXMin.toDouble, chartHandler.valueXMax.toDouble, xZoom)
  }

  override protected def minimumChartWidth: Double = {
    val range = chartHandler.valueXMax - chartHandler.valueXMin
    val zoomedRange = xZoom * range
    round(zoomedRange).toDouble
  }

  /** Centers chart on requested 'x' index. */
  override def centerOnXIdx(xIdx: Long, track: Boolean = false): Unit = {
    changeView(track) {
      val hoffset = getX(xIdx)
      val hvalue = Scrolls.computeHValue(wrappedPane, hoffset, ScrollOffsetPosition.Middle)
      wrappedPane.setHvalue(hvalue)
    }
  }

}
