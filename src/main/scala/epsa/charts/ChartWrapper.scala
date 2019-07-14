package epsa.charts

import epsa.Settings.formatNumber
import epsa.charts.LocalDateAxisWrapper.{dateToNumber, numberToDate}
import epsa.util.JFXStyles
import epsa.util.JFXStyles.AnimationHighlighter
import java.time.LocalDate
import javafx.geometry.Bounds
import javafx.scene.Parent
import javafx.scene.chart.{LineChart, NumberAxis, XYChart}
import javafx.scene.input.{MouseButton, MouseEvent, ScrollEvent}
import javafx.scene.layout.{AnchorPane, HBox, Pane, Region}
import javafx.scene.shape.{Line, Rectangle}
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import suiryc.scala.akka.CoreSystem
import suiryc.scala.concurrent.Cancellable
import suiryc.scala.javafx.beans.value.RichObservableValue
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.geometry.BoundsEx
import suiryc.scala.javafx.scene.Nodes
import suiryc.scala.javafx.scene.chart.Axises
import suiryc.scala.math.BigDecimals
import suiryc.scala.math.Ordering._
import suiryc.scala.util.Cached

trait ChartWrapper[A <: ChartMark] {

  import ChartHandler._

  /** The wrapped pane (to include in parent). */
  val wrappedPane: Parent
  /** The chart view bounds. */
  val chartViewBounds: Cached[Bounds]
  /** The chart bounds to take into account when zooming. */
  protected val chartZoomBounds: Cached[Bounds]
  /**
   * Center chart view on given X index.
   *
   * Note that 'chart view' depends on the view mode: it's the chart background
   * when clipping is enabled, or the (scroll)pane view when disabled.
   */
  def centerOnXIdx(xIdx: Long, track: Boolean = false): Unit

  protected val chartHandler: ChartHandler[A]
  /** Refreshes X axis (e.g. during pane chart resizing). */
  protected def refreshXAxis(): Unit
  protected def minimumChartWidth: Double = 0D

  import chartHandler.{meta,
    valuesList, valuesMap, valueXMin, valueXMax, currentXIdx,
    xZoomProperty, setXZoom, xZoomMin, xZoomMax}
  protected val settings: ChartSettings = chartHandler.settings
  protected val controlsNode: HBox = chartHandler.controlsNode
  protected def xZoom: BigDecimal = xZoomProperty.get

  // Note: using a CategoryAxis has many drawbacks.
  // First, it does not properly handle disparities in time scale (dates that
  // are not evenly placed). Then each 'category' is represented by a 'tick'
  // which populates chart with too many nodes when there are a lot (thousands)
  // of values to show: this takes time to draw (clearly visible even if once
  // when generating the chart while caching is used), which happens when
  // first displaying the chart but also resizing it.
  // The 'easy' solution is to use a 'NumberAxis' with some tweaks (tick label
  // formatter, manual ranging, conversions between Number and LocalDate).
  // Another solution would be to implement a whole new 'DateAxis', which would
  // at least allow to handle ticks more finely.
  protected val xAxisWrapper = new LocalDateAxisWrapper(settings)
  /** Chart 'x' axis. */
  protected val xAxis: NumberAxis = xAxisWrapper.axis

  // Resize chart when necessary
  xAxis.widthProperty.listen {
    resizeChart()
  }

  // We want to redraw marks after zoom has been applied. Listening on x axis
  // scale changes appear to do the trick.
  if (meta.marks.nonEmpty) {
    xAxis.scaleProperty.listen {
      drawMarks()
    }
  }

  /** Current markers. */
  protected var markers: Map[LocalDate, Marker] = Map.empty

  private var animationHighlighter: Option[AnimationHighlighter] = None

  /** Chart 'y' axis. */
  protected val yAxis = new NumberAxis()
  if (settings.showYLabel) {
    yAxis.setLabel(settings.yLabel)
  }
  yAxis.setTickLabelFormatter(new NumberAxis.DefaultFormatter(yAxis, null, settings.ySuffix))
  yAxis.setForceZeroInRange(false)
  yAxis.setAutoRanging(true)

  /** Zoom 'x' first selected value. */
  private var xZoomIdx1: Option[Long] = None
  /** Zoom 'x' second selected value. */
  private var xZoomIdx2: Option[Long] = None

  /** Chart series. */
  val chartSeries = new XYChart.Series[Number, Number]()

  /** The chart. */
  val chart = new LineChart[Number, Number](xAxis, yAxis)
  // Note: in charts, each drawn element is actually a child Node. When there
  // are too many, interacting with the chart becomes slower (due to styling
  // being applied etc).
  // Activating caching helps having better performances by only rendering
  // those elements as bitmaps, at the price of higher memory usage.
  // Note: caching only the chart appears to give a performance gain
  // equivalent to caching all children (some of which are re-created when
  // populating the chart anyway).
  chart.setCache(true)
  // We don't really need animation
  chart.setAnimated(false)
  if (settings.showTitle) {
    chart.setTitle(settings.title)
  }
  chart.setCreateSymbols(false)
  chart.setLegendVisible(settings.legendVisible)
  chart.getData.add(chartSeries)

  /** Vertical line to spot currently display 'x' data value. */
  private val verticalLine = new Line(0, 0, 0, 0)
  verticalLine.getStyleClass.add("chart-vertical-current-line")
  hideLine(verticalLine)
  // Noted: it is important to disable the line, otherwise it will somehow
  // steal mouse pointer and trigger spurious onMouseExited/onMouseEntered
  // events on underlying chart.
  // We also make it non managed to prevent odd interactions with its parent.
  verticalLine.setDisable(true)
  verticalLine.setManaged(false)

  /** Horizontal line to spot currently display 'y' data value. */
  private val horizontalLine = new Line(0, 0, 0, 0)
  horizontalLine.getStyleClass.add("chart-horizontal-current-line")
  hideLine(horizontalLine)
  horizontalLine.setDisable(true)
  horizontalLine.setManaged(false)

  /** Vertical line to spot previously selected 'x' data value. */
  private val verticalLineRef = new Line(0, 0, 0, 0)
  verticalLineRef.getStyleClass.add("chart-vertical-reference-line")
  hideLine(verticalLineRef)
  verticalLineRef.setDisable(true)
  verticalLineRef.setManaged(false)

  /** Horizontal line to spot previously selected 'y' data value. */
  private val horizontalLineRef = new Line(0, 0, 0, 0)
  horizontalLineRef.getStyleClass.add("chart-horizontal-reference-line")
  hideLine(horizontalLineRef)
  horizontalLineRef.setDisable(true)
  horizontalLineRef.setManaged(false)

  /** Zoom zone 'highlight' (actually darkens selected zone). */
  private val zoomZone = new Rectangle()
  zoomZone.getStyleClass.add("chart-zoom-zone")
  zoomZone.setVisible(false)
  zoomZone.setDisable(true)

  /** Chart data label to display value. */
  val dataLabel = new ChartDataLabel[Long](
    xLabel = settings.xLabel,
    xFormatter = v => numberToDate(v).format(dateFormatter),
    yLabel = settings.yLabel,
    yFormatter = v => formatNumber(v, settings.ySuffix)
  )
  dataLabel.getStylesheets.add(getClass.getResource("/css/main.css").toExternalForm)
  dataLabel.getStyleClass.add("chart-data-hover")
  dataLabel.setMinSize(Region.USE_PREF_SIZE, Region.USE_PREF_SIZE)
  dataLabel.setVisible(false)
  dataLabel.setDisable(true)
  /** Label listening subscription. */
  private var dataLabelCancellable: List[Cancellable] = Nil

  /** The chart pane (actually contains the chart). */
  val chartPane: Pane = new AnchorPane()
  // Chart has a minimum size by default, which may get in the way of layout.
  // It is prevented to be resized down under a certain value, which triggers
  // side effects: in non-clipping mode, we may not see the whole graph since
  // we voluntarily disable the vertical scrollbar, and in clipping mode it
  // changes the layoutX value negatively which makes the chart slide to the
  // left of the pane, filling the right side with the background color ...
  // Disabling the minimum size prevents the odd effects.
  // Doing it on the parent instead prevents even more glitches when we add
  // markers in the pane.
  //chart.setMinSize(0, 0)
  chartPane.setMinSize(0, 0)
  /** Chart background. */
  protected val chartBg: Region = chart.lookup(".chart-plot-background").asInstanceOf[Region]
  /** The chart background bounds. */
  val chartBgBounds: Cached[Bounds] = Cached { BoundsEx.getBounds(chartBg, chartPane) }

  // Note: it is not a good idea to track mouse from chartBg, since
  // crossing any displayed element (e.g. grid) will trigger exited/entered.
  // Better track mouse on chart, and check whether it is over the graph.
  chart.setOnMouseEntered(onMouseEntered _)
  chart.setOnMouseExited(onMouseExited _)
  chart.setOnMouseMoved(onMouseMoved _)
  chart.setOnMousePressed(onMousePressed _)
  chart.setOnMouseReleased(onMouseReleased _)
  chart.setOnMouseDragged(onMouseDragged _)
  chart.setOnMouseClicked(onMouseClicked _)
  chart.setOnScroll(onScroll _)


  private var changingViewCancellable = Option.empty[Cancellable]

  protected def setupChart(): Unit = {
    // Listen to chart background changes to invalidate cached bounds value.
    chartBg.boundsInParentProperty.listen {
      chartBgBounds.invalidate()
      // Note: since bounds are changing (which is why we reset chartBgBounds)
      // is is important to make sure we are redrawing the reference lines after
      // recomputing it.
      // e.g. listening to the chart (or its parent) resizing does not work as
      // expected, probably due to relying on the bounds value known right
      // before resizing.
      drawReferenceLines()
      // Also redraw markers.
      if (meta.marks.nonEmpty) drawMarks()
    }

    chartPane.getStylesheets.add(getClass.getResource("/css/chart.css").toExternalForm)
    chartPane.getStyleClass.add("custom-chart")
    if (meta.marks.nonEmpty) chartPane.getStyleClass.add("chart-with-markers")
    AnchorPane.setTopAnchor(chart, 0.0)
    AnchorPane.setRightAnchor(chart, 0.0)
    AnchorPane.setBottomAnchor(chart, 0.0)
    AnchorPane.setLeftAnchor(chart, 0.0)
    // Note: it is important to set 'chart' child before other children that
    // should appear above the chart.
    chartPane.getChildren.addAll(chart, zoomZone, verticalLineRef, horizontalLineRef, verticalLine, horizontalLine, dataLabel)
    ()
  }

  def setSeriesName(name: String): Unit = chartSeries.setName(name)

  protected def isInChart(date: LocalDate): Boolean = {
    isInChart(dateToNumber(date))
  }

  protected def isInChart(xIdx: Long): Boolean = {
    (chartHandler.valueXMin <= xIdx) && (xIdx <= chartHandler.valueXMax)
  }

  protected def isInXAxis(date: LocalDate): Boolean = {
    isInXAxis(dateToNumber(date))
  }

  protected def isInXAxis(xIdx: Long): Boolean = {
    (xAxis.getLowerBound <= xIdx) && (xIdx <= xAxis.getUpperBound)
  }

  /** Calls given function if position is inside chart background. */
  private def withPosInChartBackground[B](x: Double, y: Double)(f: => B): Option[B] = {
    if (chartBgBounds.value.contains(x, y)) {
      Some(f)
    } else {
      None
    }
  }

  /** Calls given function if event is inside chart background. */
  private def withEventInChartBackground[B](event: MouseEvent)(f: => B): Option[B] =
    withPosInChartBackground(event.getX, event.getY)(f)

  /** Calls given function if event is inside chart background. */
  private def withEventInChartBackground[B](event: ScrollEvent)(f: => B): Option[B] =
    withPosInChartBackground(event.getX, event.getY)(f)

  /** Resizes chart if necessary. */
  protected def resizeChart(): Unit = {
    // Compare x axis width to actual data range and resize chart (actually
    // the parent anchor pane) if necessary. Also take into account elements
    // around the chart (like y axis, padding etc).
    val widthParent = chartPane.getWidth
    val width = xAxis.getWidth
    val widthExtra = widthParent - width
    val widthExpected = math.max(chartViewBounds.value.getWidth - widthExtra, minimumChartWidth)
    if ((widthParent > 0) && (width > 0) && (width != widthExpected)) {
      val newWidth = widthExpected + widthExtra
      // Notes:
      // Only changing width right now is often ineffective (not applied until
      // something happens inside the pane ?).
      // Not setting min/max and pref also sometimes triggers glitches.
      // Delegating width changes in 'runLater' also trigger glitches (there is
      // some lag between successive resizing actions and actual resizing).
      // Changing width (min/pref/max) right now and requesting layout in
      // 'runLater' appears to solve most glitches (at least on Windows).
      chartPane.setPrefWidth(newWidth)
      chartPane.setMinWidth(newWidth)
      chartPane.setMaxWidth(newWidth)
      chartViewBounds.invalidate()
      JFXSystem.runLater {
        chartPane.requestLayout()
      }
    }
  }

  /**
   * Applies body code, expecting it to change view, and track view changes
   * to re-apply code if requested.
   */
  def changeView(track: Boolean)(body: => Unit): Unit = {
    body

    if (track) {
      // Notes:
      // Tracking is usually needed when caller is also updating series data,
      // since it may trigger other changes after we are called, which would
      // require to re-apply the requested changes ('body' code).
      // To do so, we need to listen to changing properties on the concerned
      // elements: usually the xAxis and the chart parent pane (size/view),
      // e.g. xAxis.width and (for a ScrollPane) wrappedPane.viewportBounds or
      // wrappedPane.hvalue; except when resizing because everything may not
      // have been resized yet, or xAxis 'value to position' function does not
      // return valid values yet.
      // Listening to xAxis.scale (which is used in xAxis 'value to position'
      // function) alone appears to do the trick.
      // Since there may be more than one change, keep listening for a limited
      // amount of time (and cancel previous listening if any).
      changingViewCancellable.foreach(_.cancel)
      val cancellable = xAxis.scaleProperty.listen(body)
      changingViewCancellable = Some(cancellable)
      CoreSystem.scheduler.scheduleOnce(500.milliseconds) {
        cancellable.cancel()
      }
      ()
    }
  }

  protected def centerControlsNode(): Unit = {
    // Don't use cached bounds: these were obtained while zoom is being
    // applied (but content not yet resized actually).
    // Set on pixel edge for sharper icon rendering in any case.
    val bounds = chartViewBounds.recompute()
    controlsNode.setTranslateX(bounds.getMinX + Nodes.pixelEdge((bounds.getWidth - controlsNode.getWidth) / 2))
  }

  /** Highlights requested mark. */
  def highlightMark(date: LocalDate): Unit = {
    meta.marks.get(date).foreach(highlightMark)
  }

  def highlightMark(mark: A): Unit = {
    // Make mark in view if applicable.
    val date = mark.date
    if (!isInXAxis(date) && isInChart(date)) centerOnDate(date)
    // Notes:
    // Even though we could try to highlight an existing marker, we have to deal
    // with view changes (zoom, or especially scrolling in clipping mode) that
    // may reuse markers for other dates.
    // Instead we create a new dedicated marker to work on.
    //
    // With clipping mode, we first need to insert the marker so that it's
    // initial position is (almost) right. Then we need to follow axises
    // changes to keep its position right.
    val xIdx = dateToNumber(mark.date)
    val marker = new Marker(date)
    val icon = marker.icon
    val vertical = marker.verticalLine
    val nodes = List(marker.verticalLine, marker.region)
    val cancellable = RichObservableValue.listen2(marker.region.parentProperty,
      xAxis.lowerBoundProperty, xAxis.scaleProperty, yAxis.lowerBoundProperty, yAxis.scaleProperty)
    { cancellable =>
      if ((marker.region.getParent == null) || !isInXAxis(xIdx)) {
        // Stop when marker gets out of view.
        if (marker.region.getParent != null) chartPane.getChildren.removeAll(nodes:_*)
        cancellable.cancel()
      } else {
        val (x, y) = getXY(xIdx)
        val pixelCenterX = Nodes.pixelCenter(x)
        vertical.setStartX(pixelCenterX)
        vertical.setEndX(pixelCenterX)
        marker.region.setTranslateX(Nodes.pixelCenter(x - icon.bounds.getWidth / 2))
        refreshMarker(marker, y, force = true)
      }
    }
    chartPane.getChildren.addAll(nodes:_*)
    highlightMark(marker, {
      cancellable.cancel()
      chartPane.getChildren.removeAll(nodes:_*)
      ()
    })
  }

  private def highlightMark(marker: Marker, onDone: => Unit): Unit = {
    val viewedBounds = chartViewBounds.value
    val nodes = List(marker.verticalLine, marker.region)

    // Make sure the marker is visible (and scroll to center on it if not).
    val xIdx = dateToNumber(marker.date)
    val x = getX(xIdx)
    // Note: this is for the non-clipping mode. In clipping mode caller already
    // centered view on date if necessary.
    if ((x <= viewedBounds.getMinX) || (x >= viewedBounds.getMaxX)) {
      centerOnXIdx(xIdx)
    }

    animationHighlighter = Some(JFXStyles.highlightAnimation(nodes, animationHighlighter, onDone))
  }

  private def refreshMarker(marker: Marker, y: Double, force: Boolean): Unit = {
    // Check marker and zoom bounds to prevent collision
    val bounds = chartBgBounds.value
    val markBounds = BoundsEx.getBounds(marker.icon.group, chartPane)
    val controlsBounds = controlsNode.getBoundsInParent
    val markerZoomCollision = ((markBounds.getMinX >= controlsBounds.getMinX) && (markBounds.getMinX <= controlsBounds.getMaxX)) ||
      ((markBounds.getMaxX >= controlsBounds.getMinX) && (markBounds.getMaxX <= controlsBounds.getMaxX))
    val translateY = marker.region.getTranslateY
    if (markerZoomCollision) {
      // The marker and zoom bounds are colliding horizontally
      if (force || (translateY < bounds.getMinY)) {
        // Move to bottom (and invert).
        marker.region.setTranslateY(Nodes.pixelCenter(bounds.getMaxY))
        marker.region.setScaleY(-1)
        marker.verticalLine.setStartY(Nodes.pixelCenter(bounds.getMaxY))
        marker.verticalLine.setEndY(Nodes.pixelCenter(y))
      }
    } else {
      // The marker and zoom bounds don't collide horizontally
      if (force || (translateY == 0) || (translateY > bounds.getMinY)) {
        // Move to top (and restore nominal scale).
        marker.region.setTranslateY(bounds.getMinY - 16)
        marker.region.setScaleY(1)
        marker.verticalLine.setStartY(Nodes.pixelCenter(bounds.getMinY))
        marker.verticalLine.setEndY(Nodes.pixelCenter(y))
      }
    }
  }

  /** (Re-)Draws chart marks if any. */
  protected def drawMarks(): Unit = {
    // Notes: removing/adding markers in 'runLater' triggers glitches when
    // resizing chart width (not all nodes are removed at the end, action
    // is often triggered with some delay which gets lengthier the more
    // events there are to process). So do it rigth now.
    // The ideal is to re-use already created markers when possible: it takes
    // less time to move already existing children than to remove and recreate
    // them all. It also prevents many visual glitches, and most of the time
    // we can re-use the marker which is already at the target position.
    // Since we create markers by increasing date, the easiest is also to re-use
    // old markers by increasing date: even when the date does not match, we
    // usually only need to move the old marker a bit.
    //
    // Important: we may be called while layout is being performed, and
    // particularly when the pane children are iterated over. This means we
    // must not remove children right now otherwise we could trigger an error,
    // but instead delay it (runLater).

    var markersOld = markers.values.toList.sortBy(_.date)
    val markersNew = collection.mutable.Set.empty[Marker]
    markers = Map.empty
    def cleanupOldMarkers(): Unit = {
      // We can still reset/hide the marker before removing them
      markersOld.foreach { marker =>
        marker.region.setVisible(false)
        marker.verticalLine.setVisible(false)
        // It is necessary to remove listener otherwise it keeps getting
        // triggered because the zoom node is still there.
        marker.reset()
      }
      if (markersOld.nonEmpty) JFXSystem.runLater {
        chartPane.getChildren.removeAll {
          markersOld.flatMap { marker =>
            List(marker.verticalLine, marker.region)
          }.asJava
        }
        markersOld = Nil
      }
    }
    def newMarker(date: LocalDate): Marker = {
      markersOld.headOption.map { marker =>
        markersOld = markersOld.tail
        // Cancel any listener on the marker before reusing it
        marker.reset()
        marker.date = date
        marker
      }.getOrElse {
        val marker = new Marker(date)
        markersNew.add(marker)
        marker
      }
    }

    case class MarkersBuilding(markers: Map[LocalDate, Marker] = Map.empty, previous: Option[Marker] = None) {
      def add(marker: Marker): MarkersBuilding = copy(
        markers = markers + (marker.date -> marker),
        previous = Some(marker)
      )
    }
    val bounds = chartBgBounds.value
    markers = meta.marks.values.toList.map { mark =>
      val xIdx = dateToNumber(mark.date)
      val (x, y) = getXY(xIdx)
      (mark, xIdx, x, y)
    }.filter { case (_, xIdx, x, y) =>
      // Only keep marks that would appear in the chart.
      // This is needed in clipping mode, because we only view a portion of the
      // X range, and also because when scrolling both axis bounds and scales
      // are impacted and we may not have the right value immediately: we may
      // try to place children outside the current bounds and make grow the
      // pane (and chart) triggering visual glitches.
      // We need to prevent this, and ultimately we will be called again with
      // the right values.
      isInXAxis(xIdx) &&
        (bounds.getMinX <= x) && (x <= bounds.getMaxX) &&
        (bounds.getMinY <= y) && (y <= bounds.getMaxY)
    }.sortBy(_._1.date).foldLeft(MarkersBuilding()) { case (building, (mark, _, x, y)) =>
      val pixelCenterX = Nodes.pixelCenter(x)

      val markComment = mark.comment.map { comment =>
        s"${dateFormatter.format(mark.date)}\n${comment.trim}"
      }
      building.previous.filter { previous =>
        pixelCenterX <= previous.verticalLine.getStartX + 2 * previous.icon.groupBoundsInParent.getWidth / 3
      }.map { previous =>
        // New marker would overlap previous one: re-use previous marker and
        // update its tooltip when applicable.
        previous.setupTooltip {
          val comments = previous.tooltip.map(_.getText).toList ::: markComment.toList
          if (comments.nonEmpty) Some(comments.mkString("\n\n"))
          else None
        }

        building
      }.getOrElse {
        // No overlapping
        val marker = newMarker(mark.date)

        // Note: when using non-plain (e.g. dashed) line style, it is visually
        // nicer to draw from chart border (top) to line point (bottom).
        val vertical = marker.verticalLine
        vertical.setStartX(pixelCenterX)
        vertical.setEndX(pixelCenterX)

        val icon = marker.icon
        val markGroup = icon.group
        // Note: move icon by half its width and center on pixel.
        marker.region.setTranslateX(Nodes.pixelCenter(x - icon.bounds.getWidth / 2))

        refreshMarker(marker, y, force = true)

        marker.setupTooltip(markComment)

        markGroup.setOnMouseEntered { _ =>
          meta.marksHandler(ChartMarkEvent.Entered, mark)
        }
        markGroup.setOnMouseExited { _ =>
          meta.marksHandler(ChartMarkEvent.Exited, mark)
        }

        val cancellable = RichObservableValue.listen(
          marker.region.boundsInParentProperty, controlsNode.boundsInParentProperty
        ) {
          refreshMarker(marker, y, force = false)
        }
        marker.setup(cancellable)

        building.add(marker)
      }
    }.markers

    // Finally remove remaining old markers, and add new ones.
    cleanupOldMarkers()
    if (markersNew.nonEmpty) {
      chartPane.getChildren.addAll {
        markersNew.toList.flatMap { marker =>
          List(marker.verticalLine, marker.region)
        }.asJava
      }

      // At some point in the past, we also had to request layout (later, since
      // we may be doing it right now) otherwise sometimes the marker were not
      // drawn immediately (only the next time something happens in the pane).
      // This does not seem necessary anymore.
      //JFXSystem.runLater {
      //  chartWrapper.chartPane.requestLayout()
      //}
    }

    ()
  }

  /**
   * Refreshes chart.
   *
   * Populates chart series with data when required.
   * Updates ticks and resizes chart if necessary.
   */
  def refreshChart(resetData: Boolean, clearRef: Boolean): Unit = {
    // Since we are about to change the chart data, hide lines
    hideLines(clearRef = clearRef)

    if (resetData) {
      val data = valuesList.map { case (valueX, valueY) =>
        new XYChart.Data[Number, Number](valueX, valueY)
      }

      // Note: clearing series data while chart is animated triggers an Exception
      // See: http://stackoverflow.com/a/30396889
      chartSeries.getData.clear()
      chartSeries.getData.setAll(data: _*)
    }

    refreshXAxis()
    resizeChart()
  }

  /** Apply zoom value and keep xIdx data at given x position in view. */
  def zoomOn(zoom: BigDecimal, x: Double, xIdx: Long): Unit = {
    if (zoom != xZoom) {
      setXZoom(zoom)
      refreshChart(resetData = false, clearRef = false)
      adjustView(x, xIdx)
    }
  }

  /** Adjusts view so that x position shows the xIdx index. */
  def adjustView(x: Double, xIdx: Long, track: Boolean = true): Unit = {
    // Note: we want to keep the zoom 'center' (mouse position) where it is.
    // xOffset = distance between mouse position and center of the zoom view
    // (xIdx - xCenterIdx) * xZoom = xOffset
    // xCenterIdx = xIdx - xOffset / xZoom
    val bounds = chartZoomBounds.value
    val xOffset = x - (bounds.getMinX + bounds.getMaxX) / 2
    val xCenterIdx = BigDecimals.round(xIdx - xOffset / xZoom)
    centerOnXIdx(xCenterIdx, track = track)
  }

  /**
   * Gets current 'x' index at chart view center.
   *
   * Note that 'chart view' depends on the view mode: it's the chart background
   * when clipping is enabled, or the (scroll)pane view when disabled.
   */
  def getCenterXIdx: Long = {
    val viewedBounds = chartViewBounds.value
    getXIdxReal((viewedBounds.getMinX + viewedBounds.getMaxX) / 2)
  }

  /** Centers on requested date. */
  def centerOnDate(date: LocalDate, track: Boolean = false): Unit = {
    changeView(track) {
      val xIdx = dateToNumber(date)
      centerOnXIdx(xIdx)
    }
  }

  /** Hides zoom 'highlight' area. */
  private def hideZoomArea(): Unit = {
    if (zoomZone.isVisible) {
      zoomZone.setVisible(false)
      // Make sure it won't get in the way of layout.
      zoomZone.setWidth(0)
      zoomZone.setX(0)
      zoomZone.setHeight(0)
      zoomZone.setY(0)
    }
    xZoomIdx1 = None
    xZoomIdx2 = None
  }

  private def hideLine(line: Line): Unit = {
    line.setVisible(false)
    // Make sure the line won't get in the way of layout. We actually even make
    // them non managed to prevent any odd interaction with parents.
    line.setStartX(0)
    line.setEndX(0)
    line.setStartY(0)
    line.setEndY(0)
  }

  private def hideReferenceLines(): Unit = {
    hideLine(horizontalLineRef)
    hideLine(verticalLineRef)
  }

  /**
   * Hides visible lines.
   *
   * Includes reference and current data value lines, as well as label.
   */
  private def hideLines(clearRef: Boolean): Unit = {
    hideZoomArea()
    dataLabelCancellable.foreach(_.cancel())
    dataLabelCancellable = Nil
    dataLabel.setVisible(false)
    hideLine(horizontalLine)
    hideLine(verticalLine)
    // Sometimes (exiting/leaving chart) we don't want to reset the reference value
    if (clearRef) setDataReference(None)
    // Reset previous position, so that it can be redrawn if we re-enter
    currentXIdx = None
  }

  /** Gets chart 'x' index for given position. */
  def getXIdxReal(x: Double): Long = {
    // Note: x is relative to the chart, while xAxis works
    // relatively to the background. So adjust.
    math.round(Axises.getValueForDisplay(xAxis, x - chartBgBounds.value.getMinX).doubleValue)
  }

  /** Gets chart 'x' nearest value for given position. */
  def getXIdx(x: Double): Option[Long] = {
    val v = dateToNumber(numberToDate(getXIdxReal(x)))

    // Get nearest 'x' value
    @scala.annotation.tailrec
    def loop(values: Seq[(Long, BigDecimal)], nearestX: Long): Long = values.headOption match {
      case Some((valueX, _)) =>
        val nearest = if (nearestX >= 0) math.abs(v - nearestX) else Long.MaxValue
        val delta = math.abs(v - valueX)
        if (delta < nearest) loop(values.tail, valueX)
        else nearestX

      case None => nearestX
    }

    val found = loop(valuesList, -1)
    if (found >= 0) Some(found) else None
  }

  /** Gets 'x' position for given chart value. */
  protected def getX(xIdx: Long): Double = {
    // Note: x is relative to the chart, while xAxis works
    // relatively to the background. So adjust.
    chartBgBounds.value.getMinX + Axises.getDisplayPosition(xAxis, xIdx)
  }

  /** Gets 'x' and 'y' position for given chart value. */
  private def getXY(xIdx: Long): (Double, Double) = {
    val x = getX(xIdx)
    val y = chartBgBounds.value.getMinY + Axises.getDisplayPosition(yAxis, valuesMap(xIdx))
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
  private def setLabelX(): Unit = if (dataLabel.isVisible) {
    currentXIdx.map(getXY) match {
      case Some((x, _)) =>
        dataLabel.setTranslateX(x - dataLabel.getWidth / 2)

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
  private def setLabelY(): Unit = if (dataLabel.isVisible) {
    currentXIdx.map(getXY) match {
      case Some((_, y)) =>
        dataLabel.setTranslateY(y - dataLabel.getHeight - 10)

      case None =>
      // No current position to set label on
    }
  }

  /**
   * Checks label position.
   *
   * Make sure it remains inside the chart background, and if possible does not
   * 'hide' the currently displayed chart data value.
   */
  private def checkLabelPosition(): Unit = if (dataLabel.isVisible) {
    val bounds = chartBgBounds.value
    val viewedBounds = chartViewBounds.value

    // Note: (x,y) position is relative to top/left
    val x = dataLabel.getTranslateX
    val maxX = bounds.getMaxX - dataLabel.getLayoutX - dataLabel.getWidth
    val maxX2 = viewedBounds.getMaxX - dataLabel.getLayoutX - dataLabel.getWidth
    val minX = bounds.getMinX - dataLabel.getLayoutX
    val minX2 = viewedBounds.getMinX - dataLabel.getLayoutX
    val y = dataLabel.getTranslateY
    val maxY = bounds.getMaxY - dataLabel.getLayoutY - dataLabel.getHeight
    val minY = bounds.getMinY - dataLabel.getLayoutY

    def xOk(x: Double) =
      (dataLabel.getLayoutX + x >= math.max(bounds.getMinX, viewedBounds.getMinX)) &&
        (dataLabel.getLayoutX + x + dataLabel.getWidth <= math.min(bounds.getMaxX, viewedBounds.getMaxX))

    def yOk(y: Double) =
      (dataLabel.getLayoutY + y >= math.max(bounds.getMinY, viewedBounds.getMinY)) &&
        (dataLabel.getLayoutY + y + dataLabel.getHeight <= math.min(bounds.getMaxY, viewedBounds.getMaxY))

    // Note: we need to check all (but at least one) value we would set is ok
    // in order to prevent stack overflows (which could happen if chart or view
    // size is smaller than label).
    if (x < minX) dataLabel.setTranslateX(minX)
    if ((x > maxX) && xOk(maxX)) dataLabel.setTranslateX(maxX)
    if ((x > maxX2) && xOk(maxX2)) dataLabel.setTranslateX(maxX2)
    if ((x < minX2) && xOk(minX2)) dataLabel.setTranslateX(minX2)

    if (y < minY) {
      // top end of label is going beyond chart
      currentXIdx.map(getXY) match {
        case Some((_, currentY)) =>
          // We don't want to go above the chart top, but would like not to
          // display the label in front of the chart point, unless that make
          // it go beyond the chart bottom.
          if ((bounds.getMinY + dataLabel.getHeight < currentY) || (currentY + 10 + dataLabel.getHeight > bounds.getMaxY) || !yOk(currentY + 10)) {
            // We still remain above the displayed chart point, or would go
            // beyond the chart bottom by displaying the label underneath it.
            // So just go at the top of the chart.
            dataLabel.setTranslateY(minY)
          } else {
            // chart point will be under the label, move it underneath
            dataLabel.setTranslateY(currentY + 10)
          }

        case None =>
          dataLabel.setTranslateY(minY)
      }
    }
    if ((y > maxY) && yOk(maxY)) dataLabel.setTranslateY(maxY)
  }

  private def drawLines(vertical: Line, horizontal: Line, xIdx: Long): (Double, Double) = {
    val bounds = chartBgBounds.value
    val (x, y) = getXY(xIdx)

    // Note: when using non-plain (e.g. dashed) line style, it is visually
    // nicer to draw from axis (left or bottom) to line point (right or top).
    if ((bounds.getMinX <= x) && (x <= bounds.getMaxX)) {
      val xPixel = Nodes.pixelCenter(x)
      val yPixel = Nodes.pixelCenter(y)
      vertical.setStartX(xPixel)
      vertical.setEndX(xPixel)
      vertical.setStartY(Nodes.pixelCenter(bounds.getMaxY))
      vertical.setEndY(yPixel)
      vertical.setVisible(true)
      horizontal.setStartX(Nodes.pixelCenter(bounds.getMinX) + yAxis.getTranslateX)
      horizontal.setEndX(xPixel)
      horizontal.setStartY(yPixel)
      horizontal.setEndY(yPixel)
      horizontal.setVisible(true)
    } else {
      // Hide lines if they are outside the chart: this matters in clipping
      // view mode.
      hideLine(vertical)
      hideLine(horizontal)
    }

    (x, y)
  }

  // Triggers data reference change through the chart handler.
  // Ultimately calls back updateDataReference (in all wrappers).
  private def setDataReference(dataRef: Option[ChartData[Long]]): Unit = {
    chartHandler.setDataReference(dataRef)
  }

  def updateDataReference(dataRef: Option[ChartData[Long]]): Unit = {
    dataLabel.setDataRef(dataRef)
    if (dataRef.isDefined) drawReferenceLines()
    else hideReferenceLines()
  }

  /** Draws reference value lines. */
  def drawReferenceLines(): Unit = {
    dataLabel.getDataRef.map(_.x).foreach { xIdx =>
      drawLines(verticalLineRef, horizontalLineRef, xIdx)
    }
  }

  /**
   * Draws currently displayed chart value lines.
   *
   * Also updates zoom 'highlight' zone and label.
   */
  private def drawLines(event: MouseEvent, xIdx: Option[Long]): Unit = {
    xIdx.orElse(getXIdx(event.getX)).foreach { xIdx =>
      val (x, _) = drawLines(verticalLine, horizontalLine, xIdx)

      xZoomIdx1.foreach { xIdx1 =>
        val x1 = getX(xIdx1)
        // For rectangles, position shall start on pixel edge.
        // Provided x and x1 (reference and current data positions), the
        // rectangle width shall be computed taking into account the actual
        // pixels center on which lines are drawn.
        if (xIdx >= xIdx1) {
          zoomZone.setX(Nodes.pixelEdge(x1))
          zoomZone.setWidth(Nodes.pixelCenter(x) - Nodes.pixelCenter(x1) + 1)
        } else {
          // going left: needs to change x position
          zoomZone.setX(Nodes.pixelEdge(x))
          zoomZone.setWidth(Nodes.pixelCenter(x1) - Nodes.pixelCenter(x) + 1)
        }
      }

      if (dataLabelCancellable.isEmpty) {
        // Listen for position and dimension changes to check the label remains inside the chart
        val s1 = dataLabel.boundsInParentProperty.listen(checkLabelPosition())
        // Listen to width and height changes to place the label at the right position
        val s2 = dataLabel.widthProperty.listen(setLabelX())
        val s3 = dataLabel.heightProperty.listen(setLabelY())
        dataLabelCancellable = List(s1, s2, s3)
      }

      if (!dataLabel.isVisible) {
        dataLabel.setVisible(true)
      }

      dataLabel.setData(ChartData[Long](xIdx, valuesMap(xIdx)))
      setLabelX()
      setLabelY()
    }
  }

  /**
   * onMouseEntered listener.
   *
   * Behaviour shared with onMouseMoved.
   */
  private def onMouseEntered(event: MouseEvent): Unit = {
    onMouseMoved(event)
  }

  /**
   * onMouseExited listener.
   *
   * Hides lines.
   */
  private def onMouseExited(event: MouseEvent): Unit = {
    hideLines(clearRef = false)
    meta.mouseHandler(ChartEvent.Exited, event, null)
  }

  /**
   * onMouseMoved listener.
   *
   * Draws or hides lines depending on whether mouse is inside chart background.
   */
  private def onMouseMoved(event: MouseEvent): Unit = {
    val bounds = chartBgBounds.value

    if (bounds.contains(event.getX, event.getY)) {
      getXIdx(event.getX).foreach { xIdx =>
        if (!currentXIdx.contains(xIdx)) {
          currentXIdx = Some(xIdx)
          drawLines(event, currentXIdx)
          val data = ChartSeriesData(numberToDate(xIdx), valuesMap(xIdx))
          meta.mouseHandler(ChartEvent.Moved, event, data)
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
    if (event.getButton == MouseButton.PRIMARY) {
      withEventInChartBackground(event) {
        getXIdx(event.getX).foreach { xIdx =>
          val bounds = chartBgBounds.value
          xZoomIdx1 = Some(xIdx)
          setDataReference(Some(ChartData(xIdx, valuesMap(xIdx))))

          zoomZone.setX(Nodes.pixelEdge(getX(xIdx)))
          zoomZone.setWidth(0)
          zoomZone.setY(bounds.getMinY)
          zoomZone.setHeight(bounds.getMaxY - bounds.getMinY)
          zoomZone.setVisible(true)
        }
      }
      ()
    }
  }

  /**
   * onMouseReleased listener.
   *
   * Determines zoom range from first and second selected 'x' values.
   * Displays selected range values if different from previous one.
   */
  private def onMouseReleased(event: MouseEvent): Unit = {
    if (event.getButton == MouseButton.PRIMARY) {
      for {
        xIdx1 <- xZoomIdx1
        xIdx2 <- xZoomIdx2
      } {
        hideLines(clearRef = true)

        val range = math.abs(xIdx2 - xIdx1)
        val zoom = if (range > 0) {
          val viewedBounds = chartViewBounds.value
          val zoom0 = BigDecimal(math.round(viewedBounds.getWidth)) / range
          if (zoom0 > xZoomMax) xZoomMax
          else zoom0
        } else xZoom

        if (zoom != xZoom) {
          setXZoom(zoom)
          // Center zoomed area on center of selection.
          // If target zoom is higher than maximum zoom, selected range (once
          // zoomed), which is smaller than view, will appear centered.
          // If target zoom is lower than maximum zoom, selected range will
          // span the entire view.
          refreshChart(resetData = false, clearRef = true)
          centerOnXIdx((xIdx1 + xIdx2) / 2, track = true)
        }
        // else: either we selected the same zone, or we cannot zoom anymore
      }

      hideZoomArea()
    }
  }

  /**
   * onScroll listener.
   *
   * Determines zoom factor from scroll direction.
   * Displays selected range values if different from previous one.
   */
  private def onScroll(event: ScrollEvent): Unit = {
    val deltaY = (event.getDeltaY / event.getMultiplierY).toInt
    if (deltaY != 0) withEventInChartBackground(event) {
      getXIdx(event.getX).foreach { xIdx =>
        val zoom0 =
          if (deltaY > 0) xZoom * (2 * deltaY)
          else xZoom / (-2 * deltaY)

        // Notes:
        // Actual minimal zoom depends on data dates range and viewed bounds.
        val viewedBounds = chartViewBounds.value
        val range = valueXMax - valueXMin
        val widthParent = chartPane.getWidth
        val width = xAxis.getWidth
        val widthExtra = widthParent - width
        val widthMin = viewedBounds.getWidth - widthExtra
        val actualZoomMin = BigDecimal(widthMin) / range

        // Stay within zoom limits
        @scala.annotation.tailrec
        def getZoom(zoom: BigDecimal): BigDecimal = {
          if (zoom > xZoomMax) xZoomMax
          else if ((zoom * 2 < actualZoomMin) && (actualZoomMin < xZoomMax)) getZoom(zoom * 2)
          else if (zoom < xZoomMin) xZoomMin
          else zoom
        }

        val zoom = getZoom(zoom0)
        zoomOn(zoom, event.getX, xIdx)
      }
    }
    ()
  }

  /**
   * onMouseDragged listener.
   *
   * Remembers zoom second 'x' value.
   * Calls 'onMouseMoved' listening code.
   */
  private def onMouseDragged(event: MouseEvent): Unit = {
    if (event.getButton == MouseButton.PRIMARY) {
      withEventInChartBackground(event) {
        getXIdx(event.getX).foreach { xIdx =>
          xZoomIdx2 = Some(xIdx)
        }
      }
    }

    // Note: when dragging mouse, JavaFX does not trigger 'moving' events,
    // do it ourself.
    onMouseMoved(event)
  }

  /**
   * onMouseClicked listener.
   *
   * Call backs user-defined event handler.
   */
  private def onMouseClicked(event: MouseEvent): Unit = {
    // We want real clicks (and filter dragged mouse 'click')
    if (event.isStillSincePress) {
      withEventInChartBackground(event) {
        getXIdx(event.getX).foreach { xIdx =>
          val data = ChartSeriesData(numberToDate(xIdx), valuesMap(xIdx))
          if (event.getButton == MouseButton.PRIMARY)
            meta.mouseHandler(ChartEvent.Clicked, event, data)
          else if (event.getButton == MouseButton.SECONDARY)
            meta.mouseHandler(ChartEvent.RightClicked, event, data)
        }
      }
    }
    ()
  }

}
