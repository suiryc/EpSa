package epsa.charts

import epsa.Main
import epsa.Settings.formatNumber
import epsa.util.Icons
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import javafx.beans.property.{ObjectProperty, SimpleObjectProperty}
import javafx.geometry.{BoundingBox, Pos}
import javafx.scene.control._
import javafx.scene.input.MouseEvent
import javafx.scene.layout.{AnchorPane, HBox, Region}
import javafx.scene.shape.Line
import suiryc.scala.concurrent.Cancellable
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.geometry.BoundsEx
import suiryc.scala.javafx.scene.Graphics
import suiryc.scala.util.Cached


trait ChartSeriesData {
  val date: LocalDate
  val value: BigDecimal
}

object ChartSeriesData {
  def apply(date: LocalDate, value: BigDecimal): ChartSeriesData = {
    val date0 = date
    val value0 = value
    new ChartSeriesData {
      override val date: LocalDate = date0
      override val value: BigDecimal = value0
    }
  }
}

trait ChartMark {
  val date: LocalDate
  val comment: Option[String]
}

object ChartMark {
  def apply(date: LocalDate, comment: Option[String]): ChartMark = {
    val date0 = date
    val comment0 = comment
    new ChartMark {
      override val date: LocalDate = date0
      override val comment: Option[String] = comment0
    }
  }
}

object ChartMarkEvent extends Enumeration {
  val Entered, Exited = Value
}

object ChartEvent extends Enumeration {
  val Moved, Clicked, RightClicked, Exited = Value
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
  yPrecision: Int,
  legendVisible: Boolean,
  clipping: Boolean
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
      yPrecision = 2,
      legendVisible = false,
      clipping = true
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
  protected[charts] val seriesName: String,
  seriesValues: Seq[ChartSeriesData],
  protected[charts] val meta: ChartMeta[A] = ChartMeta[A](),
  protected[charts] val settings: ChartSettings
) {

  import Main.settings.scalePercents
  import LocalDateAxisWrapper.dateToNumber

  /** Series values to display in chart. */
  private[charts] var valuesList = Seq.empty[(Long, BigDecimal)]
  /** Series values map. */
  private[charts] var valuesMap = Map.empty[Long, BigDecimal]
  private[charts] var valueXMin: Long = 0L
  private[charts] var valueXMax: Long = 0L
  private[charts] def valueXRange: Long = valueXMax - valueXMin
  private[charts] var valueYMin: BigDecimal = BigDecimal(0L)
  private[charts] var valueYMax: BigDecimal = BigDecimal(0L)
  updateValues(seriesValues, replace = true)

  def hasValues: Boolean = valuesList.nonEmpty

  /** Currently displayed 'x' data value. */
  private[charts] var currentXIdx: Option[Long] = None
  /** Zoom factor. */
  private[charts] val xZoomProperty: ObjectProperty[BigDecimal] = new SimpleObjectProperty[BigDecimal]()
  def setXZoom(v: BigDecimal): Unit = xZoomProperty.set(v)
  /** Minimal zoom factor. */
  private[charts] val xZoomMin: BigDecimal = BigDecimal(1) / 16
  /** Maximal zoom factor. */
  // Note: with 'high' zooming (8 being high already), rendering (or even
  // simple interaction) becomes really slow. This is partly due to chart grid
  // lines being dashed (default in JavaFX 8). So to keep decent performance
  // at zoom level 8, we override the CSS to use plain grid lines. The slow
  // interaction is still visible but more bearable.
  private[charts] val xZoomMax: BigDecimal = BigDecimal(8)

  private[charts] val controlsNode = new HBox()
  controlsNode.getStyleClass.add("chart-controls")
  controlsNode.setAlignment(Pos.CENTER)
  controlsNode.setSpacing(5)

  // Display current zoom value, and allow user to change to a predefined set
  // of values.
  private val zoomNode = new Hyperlink()
  zoomNode.getStyleClass.add("chart-zoom-level")
  zoomNode.setGraphic(Icons.searchPlus().pane)
  // Don't allow focus
  zoomNode.setFocusTraversable(false)
  controlsNode.getChildren.add(zoomNode)
  xZoomProperty.listen { v =>
    zoomNode.setText(formatNumber(scalePercents(v * 100), "%"))
  }
  setXZoom(BigDecimal(1))
  private val contextMenu = new ContextMenu()
  @scala.annotation.tailrec
  private def loop(zoom: BigDecimal): Unit =
    if (zoom <= xZoomMax) {
      val menuItem = new MenuItem(formatNumber(scalePercents(zoom * 100), "%"))
      menuItem.setOnAction { _ =>
        // Stay on view center after zooming
        val viewedBounds = chartWrapper.chartViewBounds.value
        val x = (viewedBounds.getMinX + viewedBounds.getMaxX) / 2
        val xIdx = chartWrapper.getXIdxReal(x)
        chartWrapper.zoomOn(zoom, x, xIdx)
      }
      contextMenu.getItems.add(menuItem)
      loop(zoom * 2)
    }
  loop(xZoomMin)
  zoomNode.setOnMouseReleased { event =>
    // Reset visited state
    zoomNode.setVisited(false)
    // Show context menu where is mouse
    contextMenu.show(zoomNode, event.getScreenX, event.getScreenY)
  }

  private val chartWrapperClipping = Cached[ChartWrapper[A]] { new ClippingChartWrapper[A](this) }
  private val chartWrapperComplete = Cached[ChartWrapper[A]] { new CompleteChartWrapper[A](this) }
  private def chartWrappers: List[ChartWrapper[A]] = chartWrapperClipping.option.toList ::: chartWrapperComplete.option.toList
  private var chartWrapper: ChartWrapper[A] = _

  private var clipping: Boolean = !settings.clipping
  private var clippingIcon: Graphics.SVGGroup = _
  def setClipping(clipping: Boolean): Unit = {
    if (clipping != this.clipping) {
      Option(clippingIcon).foreach(icon => controlsNode.getChildren.remove(icon.pane))
      this.clipping = clipping
      clippingIcon = if (clipping) Icons.expand() else Icons.compress()
      clippingIcon.pane.getStyleClass.add("chart-mode")
      controlsNode.getChildren.add(clippingIcon.pane)
      clippingIcon.pane.setOnMouseClicked { _ =>
        setClipping(!clipping)
      }
      switchChartWrapper()
    }
  }

  // The 'chart' pane (as seen by caller). Contains the actual chart.
  val chartPane = new AnchorPane

  private def switchChartWrapper(): Unit = {
    val boundsParent = new BoundingBox(chartPane.getWidth / 2, chartPane.getHeight / 2, 0, 0)
    val previousWrapper = Option(chartWrapper)
    val xIdxOpt = previousWrapper.flatMap { chartWrapper =>
      // Determine the x index shown in the middle of the view.
      val boundsChart = BoundsEx.parentToLocal(chartWrapper.chart, chartPane, boundsParent)
      val xIdxOpt = if (chartWrapper.chartBgBounds.value.contains(boundsChart.getMinX, boundsChart.getMinY)) {
        chartWrapper.getXIdx(boundsChart.getMinX)
      } else {
        None
      }
      // Properly remove children before setting up the chart pane.
      chartPane.getChildren.remove(chartWrapper.wrappedPane)
      chartWrapper.chartPane.getChildren.removeAll(controlsNode)
      xIdxOpt
    }
    val cachedChart = if (clipping) chartWrapperClipping else chartWrapperComplete
    val cached = cachedChart.option.isDefined
    chartWrapper = cachedChart.value
    chartPane.getChildren.addAll(chartWrapper.wrappedPane)
    AnchorPane.setTopAnchor(chartWrapper.wrappedPane, 0.0)
    AnchorPane.setRightAnchor(chartWrapper.wrappedPane, 0.0)
    AnchorPane.setBottomAnchor(chartWrapper.wrappedPane, 0.0)
    AnchorPane.setLeftAnchor(chartWrapper.wrappedPane, 0.0)
    chartWrapper.chartPane.getChildren.addAll(controlsNode)

    // The first time we build the chart pane, we need to fully set it up.
    // The next time we only need a refresh (e.g. to apply zoom changes).
    if (!cached) {
      updateSeries(Seq.empty, keepCenter = false)
      // Set initial information when applicable.
      previousWrapper.foreach { wrapper =>
        chartWrapper.chartSeries.setName(wrapper.chartSeries.getName)
        chartWrapper.updateDataReference(wrapper.dataLabel.getDataRef)
      }
    } else {
      chartWrapper.refreshChart(resetData = false, clearRef = false)
    }
    // Then we adjust the view to what was previously seen (in the other mode).
    // We need to track: the first time the chart is being built, but later the
    // chart may be refreshed when zoom changed.
    xIdxOpt.foreach { xIdx =>
      chartWrapper.changeView(track = true) {
        val boundsChart = BoundsEx.parentToLocal(chartWrapper.chart, chartPane, boundsParent)
        chartWrapper.adjustView(boundsChart.getMinX, xIdx, track = false)
      }
    }
  }
  setClipping(settings.clipping)

  def setSeriesName(name: String): Unit = {
    chartWrappers.foreach(_.setSeriesName(name))
  }

  private def updateValues(seriesValues: Seq[ChartSeriesData], replace: Boolean): Unit = {
    def newPoint(v: ChartSeriesData): (Long, BigDecimal) = {
      val xIdx = dateToNumber(v.date)
      val valueY = v.value
      valueXMin = math.min(valueXMin, xIdx)
      valueXMax = math.max(valueXMax, xIdx)
      if (valueY < valueYMin) valueYMin = valueY
      if (valueY > valueYMax) valueYMax = valueY
      (xIdx, v.value)
    }

    if (replace || valuesList.isEmpty) {
      if (seriesValues.nonEmpty) {
        valueXMin = Long.MaxValue
        valueXMax = Long.MinValue
        valueYMin = BigDecimal(Long.MaxValue)
        valueYMax = BigDecimal(Long.MinValue)
      } else {
        valueXMin = 0L
        valueXMax = 0L
        valueYMin = BigDecimal(0L)
        valueYMax = BigDecimal(0L)
      }
      valuesList = seriesValues.map(newPoint).sortBy(_._1)
      valuesMap = valuesList.toMap
    } else {
      valuesMap ++= seriesValues.map(newPoint).toMap
      valuesList = valuesMap.toList.sortBy(_._1)
    }
  }

  def updateSeries(seriesValues: Seq[ChartSeriesData], replace: Boolean = false, keepCenter: Boolean = true): Unit = {
    val center = if (keepCenter) Some(chartWrapper.getCenterXIdx) else None
    updateValues(seriesValues, replace)
    chartWrappers.foreach(_.refreshChart(resetData = true, clearRef = replace))
    center.foreach { center =>
      chartWrapper.centerOnXIdx(center, track = true)
    }
  }

  def setDataReference(dataRef: Option[ChartData[Long]]): Unit = {
    chartWrappers.foreach(_.updateDataReference(dataRef))
  }

  /** Highlights requested mark. */
  def highlightMark(date: LocalDate): Unit = {
    chartWrapper.highlightMark(date)
  }

  def highlightMark(mark: A): Unit = {
    chartWrapper.highlightMark(mark)
  }

  def centerOnDate(date: LocalDate, track: Boolean = false): Unit = {
    chartWrapper.centerOnDate(date, track = track)
  }

}

object ChartHandler {

  /** Date format for 'x' axis. */
  val dateFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  class Marker(var date: LocalDate) {
    val icon: Graphics.SVGGroup = Icons.mapPin()
    val region: Region = icon.pane
    region.setFocusTraversable(false)
    region.getStyleClass.setAll("chart-marker")

    val verticalLine = new Line()
    verticalLine.getStyleClass.add("chart-marker-line")
    verticalLine.setVisible(true)
    verticalLine.setDisable(true)

    var tooltip = Option.empty[Tooltip]

    def setupTooltip(text: Option[String]): Unit = {
      // Remove any installed tooltip before installing a new one.
      // This is necessary when re-using an existing marker.
      resetTooltip()
      text.foreach { s =>
        val tt = new Tooltip(s)
        tooltip = Some(tt)
        Tooltip.install(icon.group, tt)
      }
    }

    private def resetTooltip(): Unit = {
      tooltip.foreach(Tooltip.uninstall(icon.group, _))
      tooltip = None
    }

    private var cancellable = Option.empty[Cancellable]

    def setup(cancellable: Cancellable): Unit = {
      resetCancellable()
      this.cancellable = Some(cancellable)
    }

    private def resetCancellable(): Unit = {
      cancellable.foreach(_.cancel())
      cancellable = None
    }

    def reset(): Unit = {
      resetCancellable()
      resetTooltip()
    }

  }

}
