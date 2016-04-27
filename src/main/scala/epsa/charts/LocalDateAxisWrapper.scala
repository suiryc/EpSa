package epsa.charts

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import javafx.scene.chart.{NumberAxis, XYChart}
import javafx.util.StringConverter

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

  def updateTicks(series: XYChart.Series[Number, Number]): Unit = {
    import scala.collection.JavaConversions._
    val values = series.getData.map(v => math.round(v.getXValue.doubleValue))
    val min = values.min
    val max = values.max
    axis.setLowerBound(min.doubleValue)
    axis.setUpperBound(max.doubleValue)
    // TODO: change tick label format/rotation if needed
    axis.setTickUnit(7 * 5)
  }

  def dateToNumber(v: LocalDate): Long = v.toEpochDay

  def numberToDate(v: Long): LocalDate = LocalDate.ofEpochDay(v)

  def numberToDate(v: Number): LocalDate = LocalDate.ofEpochDay(math.round(v.doubleValue))

}
