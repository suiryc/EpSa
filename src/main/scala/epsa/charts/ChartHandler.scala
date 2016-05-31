package epsa.charts

import epsa.Settings.scalePercents
import epsa.controllers.{Form, Images}
import epsa.util.JFXStyles
import epsa.util.JFXStyles.AnimationHighlighter
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import javafx.beans.property.{ObjectProperty, SimpleObjectProperty}
import javafx.event.ActionEvent
import javafx.geometry.Bounds
import javafx.scene.chart.{LineChart, NumberAxis, XYChart}
import javafx.scene.control._
import javafx.scene.image.ImageView
import javafx.scene.input.{MouseButton, MouseEvent, ScrollEvent}
import javafx.scene.layout.{AnchorPane, Region}
import javafx.scene.shape.{Line, Rectangle}
import scala.collection.JavaConversions._
import scala.concurrent.duration._
import suiryc.scala.concurrent.Cancellable
import suiryc.scala.javafx.beans.value.RichObservableValue
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.javafx.event.EventHandler._
import suiryc.scala.javafx.geometry.BoundsEx
import suiryc.scala.math.BigDecimals._

// TODO: way to limit y range to currently viewed min/max; and way to reset to auto range

trait ChartSeriesData {
  val date: LocalDate
  val value: BigDecimal
}

object ChartSeriesData {
  def apply(date0: LocalDate, value0: BigDecimal): ChartSeriesData =
    new ChartSeriesData {
      override val date = date0
      override val value = value0
    }
}

trait ChartMark {
  val date: LocalDate
  val comment: Option[String]
}

object ChartMarkEvent extends Enumeration {
  val Entered = Value
  val Exited = Value
}

object ChartEvent extends Enumeration {
  val Moved = Value
  val Clicked = Value
  val RightClicked = Value
  val Exited = Value
}

case class ChartMeta[A <: ChartMark](
  marks: Map[LocalDate, A] = Map.empty[LocalDate, A],
  marksHandler: (ChartMarkEvent.Value, A) => Unit = { (_: ChartMarkEvent.Value, _: A) => },
  mouseHandler: (ChartEvent.Value, MouseEvent, ChartSeriesData) => Unit = { (_: ChartEvent.Value, _: MouseEvent, _: ChartSeriesData) => }
)

case class ChartSettings(
  title: String,
  showTitle: Boolean,
  xLabel: String,
  showXLabel: Boolean,
  yLabel: String,
  showYLabel: Boolean,
  ySuffix: String,
  legendVisible: Boolean
)

object ChartSettings {
  val hidden: ChartSettings =
    ChartSettings(
      title = "",
      showTitle = false,
      xLabel = "",
      showXLabel = false,
      yLabel = "",
      showYLabel = false,
      ySuffix = "",
      legendVisible = false
    )
}

/**
 * Handles chart for a given series.
 *
 * Creates said chart, and manages associated resources:
 *   - zooming with mouse selection (dragging or scrolling)
 *   - displaying chart data value in label
 *   - draw visible lines to spot chart data value
 */
class ChartHandler[A <: ChartMark](
  seriesName: String,
  seriesValues: Seq[ChartSeriesData],
  meta: ChartMeta[A] = ChartMeta[A](),
  settings: ChartSettings
) {

  import ChartHandler._

  // Note: using a CategoryAxis has many drawbacks.
  // First, it does not properly handle disparities in time scale (dates that
  // are not evenly placed). Then each 'category' is represented by a 'tick'
  // which populates chart with too many nodes when there are a lot (thousands)
  // of NAVs to show: this takes time to draw (clearly visible even if once
  // when generating the chart while caching is used), which happens when
  // first displaying the chart but also resizing it.
  // The 'easy' solution is to use a 'NumberAxis' with some tweaks (tick label
  // formatter, manual ranging, conversions between Number and LocalDate).
  // Another solution would be to implement a whole new 'DateAxis', which would
  // at least allow to handle ticks more finely.
  // TODO: implement a LocalDateAxis ? (see NumberAxis source code
  //   and https://pixelduke.wordpress.com/2013/09/06/dateaxis-for-javafx/
  //   and http://myjavafx.blogspot.fr/2013/09/javafx-charts-display-date-values-on.html
  //   and https://apache.googlesource.com/incubator-geode/+/sga2/jvsdfx-mm/src/main/java/com/pivotal/javafx/scene/chart/DateAxis.java)
  private val xAxisWrapper = new LocalDateAxisWrapper(settings)
  import xAxisWrapper.{dateToNumber, numberToDate}
  /** Chart 'x' axis. */
  private val xAxis = xAxisWrapper.axis

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
  private var markers: Map[LocalDate, Marker] = Map.empty

  private var animationHighlighter: Option[AnimationHighlighter] = None

  /** Date format for 'x' axis. */
  private val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  /** Chart 'y' axis. */
  private val yAxis = new NumberAxis()
  if (settings.showYLabel) {
    yAxis.setLabel(settings.yLabel)
  }
  yAxis.setTickLabelFormatter(new NumberAxis.DefaultFormatter(yAxis, null, settings.ySuffix))
  yAxis.setForceZeroInRange(false)
  yAxis.setAutoRanging(true)

  /** Chart series. */
  private val series = new XYChart.Series[Number, Number]()
  series.setName(seriesName)
  /** Series values to display in chart. */
  private var valuesList = seriesValues.map { v =>
    (dateToNumber(v.date), v.value)
  }.sortBy(_._1)
  /** Series values map. */
  private var valuesMap = valuesList.toMap
  /** Currently displayed 'x' data value. */
  private var currentXPos: Option[Long] = None
  /** Zoom factor. */
  private val xZoomProperty: ObjectProperty[BigDecimal] = new SimpleObjectProperty[BigDecimal]()
  private def xZoom: BigDecimal = xZoomProperty.get
  private def xZoom_=(v: BigDecimal) = xZoomProperty.set(v)
  /** Minimal zoom factor. */
  private val xZoomMin: BigDecimal = BigDecimal(1) / 16
  /** Maximal zoom factor. */
  // Note: with 'high' zooming (8 being high already), rendering (or even
  // simple interaction) becomes really slow. This is partly due to chart grid
  // lines being dashed (default in JavaFX 8). So to keep decent performance
  // at zoom level 8, we override the CSS to use plain grid lines. The slow
  // interaction is still visible but more bearable.
  private val xZoomMax: BigDecimal = BigDecimal(8)
  /** Zoom 'x' first selected value. */
  private var xZoomPos1: Option[Long] = None
  /** Zoom 'x' second selected value. */
  private var xZoomPos2: Option[Long] = None

  /** The chart. */
  private val chart = new LineChart[Number, Number](xAxis, yAxis)
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
    // Also redraw markers.
    if (meta.marks.nonEmpty) drawMarks()
  }

  // Display current zoom value, and allow user to change to a predefined set
  // of values.
  private val zoomNode = new Hyperlink()
  zoomNode.getStyleClass.add("chart-zoom-level")
  zoomNode.setGraphic(new ImageView(Images.iconMagnifierZoom))
  // Don't allow focus
  zoomNode.setFocusTraversable(false)
  xZoomProperty.listen { v =>
    zoomNode.setText(Form.formatAmount(scalePercents(v * 100), "%"))
  }
  xZoom = BigDecimal(1)
  private val contextMenu = new ContextMenu()
  private def loop(zoom: BigDecimal): Unit =
    if (zoom <= xZoomMax) {
      val menuItem = new MenuItem(Form.formatAmount(scalePercents(zoom * 100), "%"))
      menuItem.setOnAction { (_: ActionEvent) =>
        // Stay on view center after zooming
        val viewedBounds = getChartBackgroundViewedBounds()
        val x = viewedBounds.getMinX + viewedBounds.getWidth / 2
        val xPos = getX(getChartBackgroundBounds, viewedBounds.getMinX + viewedBounds.getWidth / 2)
        zoomOn(zoom, x, xPos)
      }
      contextMenu.getItems.add(menuItem)
      loop(zoom * 2)
    }
  loop(xZoomMin)
  zoomNode.setOnMouseReleased { (event: MouseEvent) =>
    // Reset visited state
    zoomNode.setVisited(false)
    // Show context menu where is mouse
    contextMenu.show(zoomNode, event.getScreenX, event.getScreenY)
  }

  /** Vertical line to spot currently display 'x' data value. */
  private val verticalLine = new Line(0, 0, 0, 0)
  verticalLine.getStyleClass.add("chart-vertical-current-line")
  verticalLine.setVisible(false)
  // Note: it is important to disable the line, otherwise it will somehow
  // steal mouse pointer and trigger spurious onMouseExited/onMouseEntered
  // events on underlying chart.
  verticalLine.setDisable(true)

  /** Horizontal line to spot currently display 'y' data value. */
  private val horizontalLine = new Line(0, 0, 0, 0)
  horizontalLine.getStyleClass.add("chart-horizontal-current-line")
  horizontalLine.setVisible(false)
  horizontalLine.setDisable(true)

  /** Vertical line to spot previously selected 'x' data value. */
  private val verticalLineRef = new Line(0, 0, 0, 0)
  verticalLineRef.getStyleClass.add("chart-vertical-reference-line")
  verticalLineRef.setVisible(false)
  verticalLineRef.setDisable(true)

  /** Horizontal line to spot previously selected 'y' data value. */
  private val horizontalLineRef = new Line(0, 0, 0, 0)
  horizontalLineRef.getStyleClass.add("chart-horizontal-reference-line")
  horizontalLineRef.setVisible(false)
  horizontalLineRef.setDisable(true)

  /** Zoom zone 'highlight' (actually darkens selected zone). */
  private val zoomZone = new Rectangle()
  zoomZone.getStyleClass.add("chart-zoom-zone")
  zoomZone.setVisible(false)
  zoomZone.setDisable(true)

  /** Chart data label to display value. */
  private val labelNAV = new ChartDataLabel[Long](
    xLabel = settings.xLabel,
    xFormatter = v => numberToDate(v).format(dateFormatter),
    yLabel = settings.yLabel,
    ySuffix = settings.ySuffix
  )
  labelNAV.getStylesheets.add(getClass.getResource("/css/main.css").toExternalForm)
  labelNAV.getStyleClass.add("chart-data-hover")
  labelNAV.setMinSize(Region.USE_PREF_SIZE, Region.USE_PREF_SIZE)
  labelNAV.setVisible(false)
  labelNAV.setDisable(true)
  /** Label listening subscription. */
  private var labelVLCancellable: List[Cancellable] = Nil

  /** Chart pane. */
  private val anchorPane = new AnchorPane()
  anchorPane.getStylesheets.add(getClass.getResource("/css/chart.css").toExternalForm)
  anchorPane.getStyleClass.add("custom-chart")
  if (meta.marks.nonEmpty) anchorPane.getStyleClass.add("chart-with-markers")
  val chartPane = new ScrollPane()
  // Don't display vertical scrollbar (we resize to fit parent)
  chartPane.setVbarPolicy(ScrollPane.ScrollBarPolicy.NEVER)
  // Note: scrollpane content is sometimes not resized below a 'random' value
  // when resizing down (but still triggered in multiple steps by certain
  // actions on the nodes later) even when 'fitting to width/height'.
  // However forcing the content size to match the viewport appears to do
  // the trick.
  chartPane.viewportBoundsProperty.listen { bounds =>
    anchorPane.setPrefHeight(bounds.getHeight)
    anchorPane.setMinHeight(bounds.getHeight)
    anchorPane.setMaxHeight(bounds.getHeight)
  }
  chartPane.setContent(anchorPane)
  AnchorPane.setTopAnchor(chart, 0.0)
  AnchorPane.setRightAnchor(chart, 0.0)
  AnchorPane.setBottomAnchor(chart, 0.0)
  AnchorPane.setLeftAnchor(chart, 0.0)
  anchorPane.getChildren.addAll(chart, zoomNode, zoomZone, verticalLineRef, horizontalLineRef, verticalLine, horizontalLine, labelNAV)
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

  // Place zoom node at top center of view
  RichObservableValue.listen(
    List(chart.widthProperty, chartPane.widthProperty, chartPane.hvalueProperty, zoomNode.widthProperty),
    {
      // Don't use cached bounds as these were get while zoom is being applied
      // (but content not yet resized actually).
      // Set on pixel edge for harper icon rendering in any case
      val bounds = getChartBackgroundViewedBounds(cached = false)
      zoomNode.setTranslateX(pixelEdge(bounds.getMinX + bounds.getWidth / 2 - zoomNode.getWidth / 2))
    }
  )

  /** Chart background viewed bounds. */
  private var chartBgViewedBounds: Option[Bounds] = None
  // Listen to chart background changes to invalidate cached bounds value.
  chartPane.viewportBoundsProperty.listen {
    chartBgViewedBounds = None
    resizeChart()
  }
  chartPane.hvalueProperty.listen {
    chartBgViewedBounds = None
  }
  chartPane.vvalueProperty.listen {
    chartBgViewedBounds = None
  }

  refreshView(resetData = true)

  // Display the latest NAV at first
  // Note: since scrollpane content (the chart) will change a bit later, we set
  // the H value now, which will be changed when content changes (new value will
  // be proportional to content size change) which is when we reset it to the
  // max again. Cancel our listener shortly after setting it in case the content
  // is smaller than parent (in which case the H value won't change).
  chartPane.setHvalue(chartPane.getHmax)
  private val cancellable = chartPane.hvalueProperty.listen2 { (cancellable, v) =>
    chartPane.setHvalue(chartPane.getHmax)
    cancellable.cancel()
  }
  epsa.Main.Akka.system.scheduler.scheduleOnce(500.milliseconds) {
    cancellable.cancel()
  }(epsa.Main.Akka.dispatcher)

  /** Gets, and caches, chart background bounds. */
  private def getChartBackgroundBounds: Bounds = {
    if (chartBgBounds.isEmpty) {
      chartBgBounds = Some(BoundsEx.getBounds(chartBg, anchorPane))
    }

    chartBgBounds.get
  }

  /** Gets, and caches, chart background viewed bounds. */
  private def getChartBackgroundViewedBounds(cached: Boolean = true): Bounds = {
    if (chartBgViewedBounds.isEmpty || !cached) {
      chartBgViewedBounds = Some(BoundsEx.getViewedBounds(chartPane))
    }

    chartBgViewedBounds.get
  }

  /** Calls given function if mouse is inside chart background. */
  private def withMouseInChartBackground[B](x: Double, y: Double)(f: Bounds => B): Option[B] = {
    val bounds = getChartBackgroundBounds

    if (bounds.contains(x, y)) {
      Some(f(bounds))
    } else {
      None
    }
  }

  /** Calls given function if mouse is inside chart background. */
  private def withMouseInChartBackground[B](event: MouseEvent)(f: Bounds => B): Option[B] =
    withMouseInChartBackground(event.getX, event.getY)(f)

  /** Calls given function if mouse is inside chart background. */
  private def withMouseInChartBackground[B](event: ScrollEvent)(f: Bounds => B): Option[B] =
    withMouseInChartBackground(event.getX, event.getY)(f)

  /** Resizes chart if necessary. */
  private def resizeChart(xViewOffset: Option[Long] = None) = {
    // Compare x axis width to actual data range and resize chart (actually
    // the parent anchor pane) if necessary. Also take into account elements
    // around the chart (like y axis, padding etc).
    val viewedBounds = getChartBackgroundViewedBounds()
    val data = series.getData.map(v => math.round(v.getXValue.doubleValue))
    val range =
      if (data.isEmpty) 0
      else data.max - data.min
    val zoomedRange = xZoom * range
    val widthParent = anchorPane.getWidth
    val width = xAxis.getWidth
    val widthExtra = widthParent - width
    val widthExpected = math.max(viewedBounds.getWidth - widthExtra, round(zoomedRange))
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
      anchorPane.setPrefWidth(newWidth)
      anchorPane.setMinWidth(newWidth)
      anchorPane.setMaxWidth(newWidth)
      chartBgViewedBounds = None
      xViewOffset.foreach { offset =>
        // Notes:
        // Once chart has been fully resized inside scrollpane (and scrollbar
        // ready) we wish to scroll to the specified position.
        // Listening to xAxis width or hvalue changes does not do the trick:
        // either everything has not been resized yet, or xAxis 'value to position'
        // function does not return valid values yet.
        // Waiting for xAxis.scale (which is used in xAxis 'value to position'
        // function) appears to do the trick (at least on Windows).
        xAxis.scaleProperty.listen2 { cancellable =>
          cancellable.cancel()
          scrollTo(offset)
        }
      }
      JFXSystem.runLater {
        anchorPane.requestLayout()
      }
    }
  }

  /** Scroll to request offset (to get it on left of view). */
  private def scrollTo(offset: Long): Unit = {
    // Note: since computed offset may be lower than chart minimum value,
    // make sure the requested offset (which takes into account nodes at
    // the left of the chart) is at least 0.
    val hoffset = math.max(0, getX(getChartBackgroundBounds, offset))
    val hvalue = BoundsEx.computeHValue(chartPane, hoffset)
    chartPane.setHvalue(hvalue)
  }

  /** Highlights requested mark. */
  def highlightMark(date: LocalDate): Unit = {
    markers.get(date).foreach { marker =>
      val viewedBounds = getChartBackgroundViewedBounds()
      val nodes = List(marker.verticalLine, marker.region)

      // Make sure the marker is visible (and scroll to center on it if not).
      val xPos = xAxisWrapper.dateToNumber(date)
      val x = getX(getChartBackgroundBounds, xPos)
      if ((x <= viewedBounds.getMinX) || (x >= viewedBounds.getMaxX)) {
        computeViewOffset(viewedBounds.getMinX + viewedBounds.getWidth / 2, Some(xPos)).foreach(scrollTo)
      }

      animationHighlighter = Some(JFXStyles.highlightAnimation(nodes, animationHighlighter))
    }
  }

  /** (Re-)Draws chart marks if any. */
  private def drawMarks(): Unit = {
    // Notes: removing/adding markers in 'runLater' triggers glitches when
    // resizing chart width (not all nodes are removed at the end, action
    // is often triggered with some delay which gets lengthier the more
    // events there are to process).
    // Doing it right now still has the glitch that the marker (SVG
    // path) 'disappears' while chart is resized.

    // First remove current markers
    val remove = markers.values.flatMap { marker =>
      // It is necessary to remove listener once marker is removed, otherwise
      // it keeps getting triggered because the zoom node is still there.
      marker.cancellable.cancel()
      List(marker.verticalLine, marker.region)
    }
    markers = Map.empty
    anchorPane.getChildren.removeAll(remove)

    val bounds = getChartBackgroundBounds
    markers = meta.marks.map { case (date, mark) =>
      val xPos = xAxisWrapper.dateToNumber(mark.date)
      val (x, y) = getXY(bounds, xPos)

      // Note: when using non-plain (e.g. dashed) line style, it is visually
      // nicer to draw from chart border (top) to line point (bottom).
      val vertical = new Line()
      vertical.getStyleClass.add("chart-marker-line")
      vertical.setStartX(pixelCenter(x))
      vertical.setEndX(pixelCenter(x))
      vertical.setStartY(pixelCenter(bounds.getMinY))
      vertical.setEndY(pixelCenter(y))
      vertical.setVisible(true)
      vertical.setDisable(true)

      val markRegion = new Region
      markRegion.setFocusTraversable(false)
      markRegion.getStyleClass.setAll("chart-marker")
      markRegion.setMaxWidth(Region.USE_PREF_SIZE)
      markRegion.setMaxHeight(Region.USE_PREF_SIZE)
      // Note: set layout because 'translate' is used in CSS to center shape.
      // Setting it on pixel edge since it is a 'box'. (even though CSS may
      // have to offset by 0.5 if region width is an even number).
      markRegion.setLayoutX(pixelEdge(x))

      mark.comment.foreach { comment =>
        Tooltip.install(markRegion, new Tooltip(s"${dateFormatter.format(date)}\n$comment"))
      }

      markRegion.setOnMouseEntered { (_: MouseEvent) =>
        meta.marksHandler(ChartMarkEvent.Entered, mark)
      }
      markRegion.setOnMouseExited { (_: MouseEvent) =>
        meta.marksHandler(ChartMarkEvent.Exited, mark)
      }

      // Check marker and zoom bounds to prevent collision
      // Notes:
      // Change Y 'layout' as 'translate' is used in CSS.
      // Don't change Y 'scale' when moving mark to bottom of chart as
      // it is used in CSS (which ultimately overrides what the code
      // may set). Instead use styling to do it through a pseudo class
      // (using a class style may have some glitches, e.g. the SVG path
      // appearing unscaled for an instant the first time we change the
      // style class).
      val cancellable = RichObservableValue.listen(
        List(markRegion.boundsInParentProperty, zoomNode.boundsInParentProperty),
        {
          val markBounds = markRegion.getBoundsInParent
          val zoomBounds = zoomNode.getBoundsInParent
          if (((markBounds.getMinX >= zoomBounds.getMinX) && (markBounds.getMinX <= zoomBounds.getMaxX)) ||
              ((markBounds.getMaxX >= zoomBounds.getMinX) && (markBounds.getMaxX <= zoomBounds.getMaxX))) {
            // The marker and zoom bounds are colliding horizontally
            if (markRegion.getLayoutY == 0) {
              // If the marker is at the top of the chart (first collision),
              // move it at the bottom, and invert it (pointing up).
              // Adjust vertical line accordingly.
              markRegion.setLayoutY(pixelCenter(bounds.getMaxY))
              JFXStyles.togglePseudoClass(markRegion, "inverted", set = true)
              vertical.setStartY(pixelCenter(bounds.getMaxY))
              vertical.setEndY(pixelCenter(y))
            }
          } else {
            // The marker and zoom bounds don't collide horizontally
            if (markRegion.getLayoutY != 0) {
              // If the marker is at the bottom of the chart (previous collision),
              // revert it to the top (pointing down).
              // Adjust vertical line accordingly.
              markRegion.setLayoutY(0)
              JFXStyles.togglePseudoClass(markRegion, "inverted", set = false)
              vertical.setStartY(pixelCenter(bounds.getMinY))
              vertical.setEndY(pixelCenter(y))
            }
          }
        }
      )

      date -> Marker(markRegion, vertical, cancellable)
    }
    val add = markers.values.flatMap { marker =>
      List(marker.verticalLine, marker.region)
    }
    anchorPane.getChildren.addAll(add)

    // Requesting layout (in 'runLater') triggers marker drawing (which
    // otherwise sometimes is done next time something happens in pane).
    JFXSystem.runLater {
      anchorPane.requestLayout()
    }
  }

  def updateSeries(seriesValues: Seq[ChartSeriesData]): Unit = {
    valuesMap ++= seriesValues.map { v =>
      dateToNumber(v.date) -> v.value
    }.toMap
    valuesList = valuesMap.toList.sortBy(_._1)
    val viewedBounds = getChartBackgroundViewedBounds()
    val offset = getX(getChartBackgroundBounds, viewedBounds.getMinX)
    refreshView(resetData = true, offset)
  }

  /**
   * Refreshes chart.
   *
   * Populates chart series with data when required.
   * Updates ticks and resizes chart if necessary.
   */
  private def refreshView(resetData: Boolean, xViewOffset: Option[Long] = None): Unit = {
    // Since we are about to change the chart data, hide lines
    hideLines(clearRef = true)

    if (resetData) {
      val data = valuesList.map { case (valueX, valueY) =>
        new XYChart.Data[Number, Number](valueX, valueY)
      }

      // Note: clearing series data while chart is animated triggers an Exception
      // See: http://stackoverflow.com/a/30396889
      series.getData.clear()
      series.getData.setAll(data: _*)
    }

    xAxisWrapper.updateTicks(series, xZoom)
    resizeChart(xViewOffset)
  }

  /** Apply zoom value and keep xPos data at given x position in view. */
  private def zoomOn(zoom: BigDecimal, x: Double, xPos: Option[Long]): Unit = {
    if (zoom != xZoom) {
      xZoom = zoom
      // Note: we want to keep the zoom 'center' (mouse position) where it is.
      val xViewOffset = computeViewOffset(x, xPos)
      refreshView(resetData = false, xViewOffset)
    }
  }

  /** Computes the 'x' position at the left of the viewed area so that the given 'x' position appears at the given offset. */
  private def computeViewOffset(x: Double, xPos: Option[Long]): Option[Long] = {
    // xOffset = distance between view (left) bound and mouse position
    // xViewOffset = first 'x' value to show in chart view
    // (xPos - xViewOffset) * zoom = xOffset
    // => xViewOffset = xPos - xOffset / zoom
    val viewedBounds = getChartBackgroundViewedBounds()
    xPos.map { pos =>
      val xOffset = x - viewedBounds.getMinX
      round(pos - xOffset / xZoom)
    }
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
    labelNAV.setVisible(false)
    horizontalLine.setVisible(false)
    verticalLine.setVisible(false)
    // Sometimes (exiting/leaving chart) we don't want to reset the reference value
    if (clearRef) {
      horizontalLineRef.setVisible(false)
      verticalLineRef.setVisible(false)
      labelNAV.setDataRef(None)
    }
    // Reset previous position, so that it can be redrawn if we re-enter
    currentXPos = None
  }

  /**
   * Computes value to center on a pixel.
   *
   * See 'Coordinate System' section in JavaFX Node JavaDoc.<br>
   * The center of a pixel is at 0.5 past its index; e.g. top-left pixel center
   * is at x=0.5 and y=0.5 (and not x=0 y=0).
   * This function helps to get the center of pixel provided its index. This
   * is useful to draw sharp 1-pixel wide lines, otherwise antialiasing is used
   * to draw it 2-pixels wide.
   *
   * @param v pixel x or y index
   * @return pixel center
   */
  private def pixelCenter(v: Double): Double =
  // Note: add a small delta before flooring to try to prevent going to
  // previous pixel due to 'double' rounding errors.
    math.floor(v + 0.0001) + 0.5

  /**
   * Computes pixel (left/top) edge.
   *
   * @param v pixel x or y edge
   * @return pixel edge
   */
  private def pixelEdge(v: Double): Double =
    pixelCenter(v) - 0.5

  /** Gets chart 'x' value for given position. */
  private def getX(bounds: Bounds, x: Double): Option[Long] = {
    // Note: x is relative to the chart, while xAxis works
    // relatively to the background. So adjust.
    val v = dateToNumber(numberToDate(xAxis.getValueForDisplay(x - bounds.getMinX)))

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
  private def getX(bounds: Bounds, xPos: Long): Double =
  // Note: x is relative to the chart, while xAxis works
  // relatively to the background. So adjust.
    bounds.getMinX + xAxis.getDisplayPosition(xPos)

  /** Gets 'x' and 'y' position for given chart value. */
  private def getXY(bounds: Bounds, xPos: Long): (Double, Double) = {
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
  private def setLabelX() = if (labelNAV.isVisible) {
    val bounds = getChartBackgroundBounds

    currentXPos.map(getXY(bounds, _)) match {
      case Some((x, _)) =>
        labelNAV.setTranslateX(x - labelNAV.getWidth / 2)

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
  private def setLabelY() = if (labelNAV.isVisible) {
    val bounds = getChartBackgroundBounds

    currentXPos.map(getXY(bounds, _)) match {
      case Some((_, y)) =>
        labelNAV.setTranslateY(y - labelNAV.getHeight - 10)

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
  private def checkLabelPosition() = if (labelNAV.isVisible) {
    val bounds = getChartBackgroundBounds
    val viewedBounds = getChartBackgroundViewedBounds()

    // Note: (x,y) position is relative to top/left
    val x = labelNAV.getTranslateX
    val maxX = bounds.getMaxX - labelNAV.getLayoutX - labelNAV.getWidth
    val maxX2 = viewedBounds.getMaxX - labelNAV.getLayoutX - labelNAV.getWidth
    val minX = bounds.getMinX - labelNAV.getLayoutX
    val minX2 = viewedBounds.getMinX - labelNAV.getLayoutX
    val y = labelNAV.getTranslateY
    val maxY = bounds.getMaxY - labelNAV.getLayoutY - labelNAV.getHeight
    val minY = bounds.getMinY - labelNAV.getLayoutY

    def xOk(x: Double) =
      (labelNAV.getLayoutX + x >= math.max(bounds.getMinX, viewedBounds.getMinX)) &&
        (labelNAV.getLayoutX + x + labelNAV.getWidth <= math.min(bounds.getMaxX, viewedBounds.getMaxX))

    def yOk(y: Double) =
      (labelNAV.getLayoutY + y >= math.max(bounds.getMinY, viewedBounds.getMinY)) &&
        (labelNAV.getLayoutY + y + labelNAV.getHeight <= math.min(bounds.getMaxY, viewedBounds.getMaxY))

    // Note: we need to check all (but at least one) value we would set is ok
    // in order to prevent stack overflows (which could happen if chart or view
    // size is smaller than label).
    if (x < minX) labelNAV.setTranslateX(minX)
    if ((x > maxX) && xOk(maxX)) labelNAV.setTranslateX(maxX)
    if ((x > maxX2) && xOk(maxX2)) labelNAV.setTranslateX(maxX2)
    if ((x < minX2) && xOk(minX2)) labelNAV.setTranslateX(minX2)

    if (y < minY) {
      // top end of label is going beyond chart
      currentXPos.map(getXY(bounds, _)) match {
        case Some((_, currentY)) =>
          // We don't want to go above the chart top, but would like not to
          // display the label in front of the chart point, unless that make
          // it go beyond the chart bottom.
          if ((bounds.getMinY + labelNAV.getHeight < currentY) || (currentY + 10 + labelNAV.getHeight > bounds.getMaxY) || !yOk(currentY + 10)) {
            // We still remain above the displayed chart point, or would go
            // beyond the chart bottom by displaying the label underneath it.
            // So just go at the top of the chart.
            labelNAV.setTranslateY(minY)
          } else {
            // chart point will be under the label, move it underneath
            labelNAV.setTranslateY(currentY + 10)
          }

        case None =>
          labelNAV.setTranslateY(minY)
      }
    }
    if ((y > maxY) && yOk(maxY)) labelNAV.setTranslateY(maxY)
  }

  private def drawLines(vertical: Line, horizontal: Line, xPos: Long): (Double, Double) = {
    val bounds = getChartBackgroundBounds
    val (x, y) = getXY(bounds, xPos)

    // Note: when using non-plain (e.g. dashed) line style, it is visually
    // nicer to draw from axis (left or bottom) to line point (right or top).
    vertical.setStartX(pixelCenter(x))
    vertical.setEndX(pixelCenter(x))
    vertical.setStartY(pixelCenter(bounds.getMaxY))
    vertical.setEndY(pixelCenter(y))
    vertical.setVisible(true)
    horizontal.setStartX(pixelCenter(bounds.getMinX))
    horizontal.setEndX(pixelCenter(x))
    horizontal.setStartY(pixelCenter(y))
    horizontal.setEndY(pixelCenter(y))
    horizontal.setVisible(true)

    (x, y)
  }

  /** Draws reference value lines. */
  private def drawReferenceLines(xPos: Option[Long] = None): Unit = {
    xPos.orElse(labelNAV.getDataRef.map(_.x)).foreach { xPos =>
      drawLines(verticalLineRef, horizontalLineRef, xPos)
    }
  }

  /**
   * Draws currently displayed chart value lines.
   *
   * Also updates zoom 'highlight' zone and label.
   */
  private def drawLines(event: MouseEvent, xPos: Option[Long] = None): Unit = {
    val bounds = getChartBackgroundBounds

    xPos.orElse(getX(bounds, event.getX)).foreach { xPos =>
      val (x, _) = drawLines(verticalLine, horizontalLine, xPos)

      xZoomPos1.foreach { xPos1 =>
        val x1 = getX(bounds, xPos1)
        // For rectangles, position shall start on pixel edge.
        // Provided x and x1 (reference and current data positions), the
        // rectangle width shall be computed taking into account the actual
        // pixels center on which lines are drawn.
        if (xPos >= xPos1) {
          zoomZone.setX(pixelEdge(x1))
          zoomZone.setWidth(pixelCenter(x) - pixelCenter(x1) + 1)
        } else {
          // going left: needs to change x position
          zoomZone.setX(pixelEdge(x))
          zoomZone.setWidth(pixelCenter(x1) - pixelCenter(x) + 1)
        }
      }

      if (labelVLCancellable.isEmpty) {
        // Listen for position and dimension changes to check the label remains inside the chart
        val s1 = labelNAV.boundsInParentProperty.listen(checkLabelPosition())
        // Listen to width and height changes to place the label at the right position
        val s2 = labelNAV.widthProperty.listen(setLabelX())
        val s3 = labelNAV.heightProperty.listen(setLabelY())
        labelVLCancellable = List(s1, s2, s3)
      }

      if (!labelNAV.isVisible) {
        labelNAV.setVisible(true)
      }

      labelNAV.setData(ChartData[Long](xPos, valuesMap(xPos)))
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
    val bounds = getChartBackgroundBounds

    if (bounds.contains(event.getX, event.getY)) {
      getX(bounds, event.getX).foreach { xPos =>
        if (!currentXPos.contains(xPos)) {
          currentXPos = Some(xPos)
          drawLines(event, currentXPos)
          val data = ChartSeriesData(xAxisWrapper.numberToDate(xPos), valuesMap(xPos))
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
      withMouseInChartBackground(event) { bounds =>
        getX(bounds, event.getX).foreach { xPos =>
          xZoomPos1 = Some(xPos)
          labelNAV.setDataRef(Some(ChartData(xPos, valuesMap(xPos))))
          drawReferenceLines(xZoomPos1)

          zoomZone.setX(pixelEdge(getX(bounds, xPos)))
          zoomZone.setWidth(0)
          zoomZone.setY(bounds.getMinY)
          zoomZone.setHeight(bounds.getMaxY - bounds.getMinY)
          zoomZone.setVisible(true)
        }
      }
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
        pos1 <- xZoomPos1
        pos2 <- xZoomPos2
      } {
        hideLines(clearRef = true)

        val range = math.abs(pos2 - pos1)
        val zoom = if (range > 0) {
          val viewedBounds = getChartBackgroundViewedBounds()
          val zoom0 = BigDecimal(math.round(viewedBounds.getWidth)) / range
          if (zoom0 > xZoomMax) xZoomMax
          else zoom0
        } else xZoom

        if (zoom != xZoom) {
          val viewedBounds = getChartBackgroundViewedBounds()
          xZoom = zoom
          // When target zoom is higher than maximum zoom, selected range (once
          // zoomed) is smaller than view. So compute the actual left data index
          // that makes the selected range centered.
          // If target zoom is lower than maximum zoom, the computed left index
          // matches the lower end of the selected range.
          //
          // number of visible data = viewWidth / zoom
          // center of zoom area = (pos1 + pos2) / 2
          // data on the left of the view = center - half visible data
          //  = (pos1 + pos2) / 2 - (viewWidth / zoom) / 2
          //  = (pos1 + pos2 - viewWidth / zoom) / 2
          val offset = round((pos1 + pos2 - viewedBounds.getWidth / xZoom) / 2)
          refreshView(resetData = false, xViewOffset = Some(offset))
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
    if (deltaY != 0) withMouseInChartBackground(event) { bounds =>
      getX(bounds, event.getX).foreach { xPos =>
        val zoom0 =
          if (deltaY > 0) xZoom * (2 * deltaY)
          else xZoom / (-2 * deltaY)

        // Note: actual minimal zoom depends on data dates range and viewed bounds
        val viewedBounds = getChartBackgroundViewedBounds()
        val data = series.getData.map(v => math.round(v.getXValue.doubleValue))
        val range =
          if (data.isEmpty) 0
          else data.max - data.min
        val widthParent = anchorPane.getWidth
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
        zoomOn(zoom, event.getX, Some(xPos))
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
    if (event.getButton == MouseButton.PRIMARY) {
      withMouseInChartBackground(event) { bounds =>
        getX(bounds, event.getX).foreach { xPos =>
          xZoomPos2 = Some(xPos)
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
      withMouseInChartBackground(event) { bounds =>
        getX(bounds, event.getX).foreach { xPos =>
          val data = ChartSeriesData(xAxisWrapper.numberToDate(xPos), valuesMap(xPos))
          if (event.getButton == MouseButton.PRIMARY)
            meta.mouseHandler(ChartEvent.Clicked, event, data)
          else if (event.getButton == MouseButton.SECONDARY)
            meta.mouseHandler(ChartEvent.RightClicked, event, data)
        }
      }
    }
  }

}

object ChartHandler {
  case class Marker(region: Region, verticalLine: Line, cancellable: Cancellable)
}
