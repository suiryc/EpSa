package epsa.charts

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import javafx.scene.chart.{NumberAxis, XYChart}
import javafx.util.StringConverter
import suiryc.scala.math.BigDecimals._

class LocalDateAxisWrapper(settings: ChartSettings) {

  /** Date format for axis. */
  val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  /** Actual (wrapped) axis. */
  val axis = new NumberAxis()
  if (settings.showXLabel) {
    axis.setLabel(settings.xLabel)
  }
  axis.setTickLabelFormatter(new StringConverter[Number]() {
    override def fromString(v: String): Number =
      dateToNumber(LocalDate.parse(v, dateFormatter))
    override def toString(v: Number): String =
      numberToDate(v).format(dateFormatter)
  })
  axis.setTickLabelRotation(90)
  axis.setForceZeroInRange(false)
  axis.setAutoRanging(false)

  def updateTicks(series: XYChart.Series[Number, Number], zoom: BigDecimal): Unit = {
    val data = series.getData
    if (!data.isEmpty) {
      import scala.collection.JavaConversions._
      val values = data.map(v => math.round(v.getXValue.doubleValue))
      val min = values.min
      val max = values.max
      axis.setLowerBound(min.doubleValue)
      axis.setUpperBound(max.doubleValue)
      // TODO: change tick label format/rotation if needed
      // Default tick unit (1 pixel = 1 day): 8 weeks (56 days = 56 pixels)
      // Scale tick unit with zoom, minimum value (for high zooming): 7 days
      val tickUnit = math.max(7, round(7 * 4 * 2 / zoom)).toInt
      axis.setTickUnit(tickUnit)
      // If tick unit is beyond 7 days, display 5 (the default) minor ticks,
      // otherwise display one minor tick for each day.
      val minorTickCount =
        if (tickUnit > 7) 5
        else tickUnit
      axis.setMinorTickCount(minorTickCount)
    } else {
      axis.setLowerBound(0)
      axis.setUpperBound(0)
    }
  }

  def dateToNumber(v: LocalDate): Long = v.toEpochDay

  def numberToDate(v: Long): LocalDate = LocalDate.ofEpochDay(v)

  def numberToDate(v: Number): LocalDate = LocalDate.ofEpochDay(math.round(v.doubleValue))

}
