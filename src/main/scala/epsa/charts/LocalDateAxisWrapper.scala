package epsa.charts

import java.time.LocalDate
import javafx.scene.chart.NumberAxis
import javafx.util.StringConverter
import suiryc.scala.math.BigDecimals._

object LocalDateAxisWrapper {

  def dateToNumber(v: LocalDate): Long = v.toEpochDay

  def numberToDate(v: Long): LocalDate = LocalDate.ofEpochDay(v)

  def numberToDate(v: Number): LocalDate = numberToDate(math.round(v.doubleValue))

}

/** Wraps a standard NumberAxis to more easily handle LocalDate values. */
class LocalDateAxisWrapper(settings: ChartSettings) {

  import LocalDateAxisWrapper._

  /** Actual (wrapped) axis. */
  val axis = new NumberAxis()
  if (settings.showXLabel) {
    axis.setLabel(settings.xLabel)
  }
  axis.setTickLabelFormatter(new StringConverter[Number]() {
    override def fromString(v: String): Number =
      dateToNumber(LocalDate.parse(v, ChartHandler.dateFormatter))
    override def toString(v: Number): String =
      numberToDate(v).format(ChartHandler.dateFormatter)
  })
  axis.setTickLabelRotation(90)
  axis.setForceZeroInRange(false)
  axis.setAutoRanging(false)

  // Note: series data are expected to be sorted by caller.
  def updateTicks(valueMin: Double, valueMax: Double, zoom: BigDecimal): Unit = {
    if (valueMax > valueMin) {
      axis.setLowerBound(valueMin)
      axis.setUpperBound(valueMax)
      // Default tick unit (1 pixel = 1 day): 8 weeks (56 days = 56 pixels)
      // Scale tick unit with zoom, minimum value (for high zooming): 7 days
      val tickUnit = math.max(7, round(7 * 4 * 2 / zoom)).toInt
      axis.setTickUnit(tickUnit.toDouble)
      // If tick unit is beyond 7 days, display 5 (the default) minor ticks,
      // otherwise display one minor tick for each day.
      val minorTickCount =
        if (tickUnit > 7) 5
        else tickUnit
      axis.setMinorTickCount(minorTickCount)
    } else {
      axis.setLowerBound(valueMin)
      axis.setUpperBound(valueMin)
    }
  }

}
