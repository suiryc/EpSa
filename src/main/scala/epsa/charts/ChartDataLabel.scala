package epsa.charts

import epsa.util.JFXStyles
import javafx.scene.layout.VBox
import javafx.scene.text.{Text, TextFlow}

/** Chart data. */
case class ChartData[A](x: A, y: BigDecimal)

/**
 * Chart data 'label'.
 *
 * Simply assembles vertically a text for the current visible data, and an
 * optional text flow (to apply different styles) for an optional reference
 * data.
 */
class ChartDataLabel[A](
  xLabel: String,
  xFormatter: A => String,
  yLabel: String,
  yFormatter: BigDecimal => String
) extends VBox {

  /** Reference data. */
  private var refData: Option[ChartData[A]] = None

  /** Text of current data. */
  private val dataText = new Text()
  /** Text (optional) for reference data. */
  private val refText = new TextFlow()
  /** Reference data label. */
  private val refTextLabel = new Text("Δ: ")
  /** Reference data variation value. */
  private val refTextValue = new Text()
  /** Reference data data. */
  private val refTextDate = new Text()

  // First populate the reference data text flow
  refText.getChildren.addAll(refTextLabel, refTextValue, refTextDate)
  // And add the data text to our children
  getChildren.addAll(dataText)

  def getDataRef: Option[ChartData[A]] =
    refData

  /**
   * Changes reference data.
   *
   * Takes care of adding/removing the reference data text flow when necessary.
   */
  def setDataRef(data: Option[ChartData[A]]): Unit = {
    if (refData.isDefined && data.isEmpty) {
      getChildren.setAll(dataText)
    } else if (refData.isEmpty && data.isDefined) {
      getChildren.setAll(dataText, refText)
    }
    refData = data
    refData.foreach(setData)
  }

  /**
   * Changes current data.
   *
   * Updates data text, and reference one if necessary.
   * If reference is present, variation value is colored accordingly: red if
   * current data is lower, green if greater, black if equal.
   */
  def setData(data: ChartData[A]): Unit = {
    dataText.setText(s"$xLabel: ${xFormatter(data.x)}\n$yLabel: ${yFormatter(data.y)}")
    refData.foreach { refData =>
      val delta: BigDecimal = if (refData.y.compare(java.math.BigDecimal.ZERO) != 0) {
        (data.y - refData.y) * 100 / refData.y
      } else {
        0
      }
      refTextValue.setText(f"$delta%+.2f%%")
      refTextDate.setText(s" (${xFormatter(refData.x)})")
      if (delta > 0) {
        JFXStyles.togglePositive(refTextValue)
      } else if (delta < 0) {
        JFXStyles.toggleNegative(refTextValue)
      } else {
        JFXStyles.toggleNeutral(refTextValue)
      }
    }
  }

}
