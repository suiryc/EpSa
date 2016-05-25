package epsa.model

import epsa.I18N.Strings
import epsa.controllers.{ColoredCell, FormatCell, Images}
import epsa.util.JFXStyles
import java.time.LocalDate
import java.util.Comparator
import javafx.beans.property.{SimpleObjectProperty, SimpleStringProperty}
import javafx.scene.control.{ContentDisplay, Label, TableColumn, Tooltip}
import javafx.scene.image.ImageView
import javafx.scene.layout.Region
import scala.collection.immutable.ListMap
import suiryc.scala.javafx.util.Callback
import suiryc.scala.math.Ordering.localDateOrdering
import suiryc.scala.util.Comparators

/** Asset field settings. */
trait AssetField[A] {
  /** Field key. */
  val key: String
  /** Field name in table. */
  val tableLabel: String
  /** Field name in details pane. */
  val detailsLabel: String
  /** Field comment if any. */
  val comment: (AssetDetails) => Option[String] = { _ => None }
  /** How to format field value. */
  val format: (AssetDetails, Boolean) => String

  /** The details pane label (where value is displayed). */
  // Note: the label is cached so that all tabs (savings on various dates)
  // can display their respective details.
  lazy val detailsValue = AssetField.detailsLabels.get(key) match {
    case Some(label) =>
      label

    case None =>
      val label = new Label
      label.setMinWidth(Region.USE_PREF_SIZE)
      label.setMinHeight(Region.USE_PREF_SIZE)
      // Display graphic on the right side
      label.setContentDisplay(ContentDisplay.RIGHT)
      AssetField.detailsLabels += key -> label
      label
  }

  /** The table column. */
  val column: TableColumn[AssetDetails, A]

  def updateDetailsValue(assetDetailsOpt: Option[AssetDetails]): Unit = {
    detailsValue.setText(assetDetailsOpt.map(format(_, true)).orNull)
    assetDetailsOpt.flatMap(comment) match {
      case Some(text) =>
        detailsValue.setTooltip(new Tooltip(text))
        detailsValue.setGraphic(AssetField.tooltipHint)

      case None =>
        detailsValue.setTooltip(null)
        detailsValue.setGraphic(null)
    }
  }
}

/** Asset field with formatted text value to display. */
case class AssetTextField(key: String, tableLabel: String, detailsLabel: String,
  format: (AssetDetails, Boolean) => String,
  override val comment: (AssetDetails) => Option[String] = { _ => None }
) extends AssetField[String] {
  val column = new TableColumn[AssetDetails, String](tableLabel)
  column.setCellValueFactory(Callback { data =>
    new SimpleStringProperty(format(data.getValue, false))
  })
}

/** Asset field with date to display. */
case class AssetDateField(key: String, tableLabel: String, detailsLabel: String,
  format: (AssetDetails, Boolean) => String,
  value: (AssetDetails) => Option[LocalDate]
) extends AssetField[AssetDetails] {
  val column = new TableColumn[AssetDetails, AssetDetails](tableLabel)
  column.setCellValueFactory(Callback { data =>
    new SimpleObjectProperty(data.getValue)
  })
  column.setCellFactory(Callback { new FormatCell[AssetDetails, AssetDetails](v => format(v, false)) })
  column.setComparator(AssetField.dateComparator(value))
}

/** Asset field with amount to display. */
case class AssetAmountField(key: String, tableLabel: String, detailsLabel: String,
  format: (AssetDetails, Boolean) => String,
  value: (AssetDetails) => Option[BigDecimal]
) extends AssetField[AssetDetails] {
  val column = new TableColumn[AssetDetails, AssetDetails](tableLabel)
  column.setCellValueFactory(Callback { data =>
    new SimpleObjectProperty(data.getValue)
  })
  column.setCellFactory(Callback { new FormatCell[AssetDetails, AssetDetails](v => format(v, false)) })
  column.setComparator(AssetField.amountComparator(value))
}

/** Asset field with (colored) amount to display. */
case class AssetColoredAmountField(key: String, tableLabel: String, detailsLabel: String,
  format: (AssetDetails, Boolean) => String,
  value: (AssetDetails) => Option[BigDecimal]
) extends AssetField[AssetDetails] {
  val column = new TableColumn[AssetDetails, AssetDetails](tableLabel)
  column.setCellValueFactory(Callback { data =>
    new SimpleObjectProperty(data.getValue)
  })
  val value0 = value
  column.setCellFactory(Callback {
    new FormatCell[AssetDetails, AssetDetails](v => format(v, false)) with ColoredCell[AssetDetails] {
      def value(v: AssetDetails) = value0(v)
    }
  })
  column.setComparator(AssetField.amountComparator(value))

  override def updateDetailsValue(assetDetailsOpt: Option[AssetDetails]): Unit = {
    super.updateDetailsValue(assetDetailsOpt)
    assetDetailsOpt.flatMap(value).find(_ != 0) match {
      case Some(v) =>
        if (v > 0) JFXStyles.togglePositive(detailsValue)
        else JFXStyles.toggleNegative(detailsValue)

      case None =>
        JFXStyles.toggleNeutral(detailsValue)
    }
  }
}

object AssetField {

  val KEY_SCHEME = "scheme"
  val KEY_FUND = "fund"
  val KEY_AVAILABILITY = "availability"
  val KEY_UNITS = "units"
  val KEY_VWAP = "vwap"
  val KEY_DATE = "date"
  val KEY_NAV = "nav"
  val KEY_INVESTED_AMOUNT = "investedAmount"
  val KEY_GROSS_AMOUNT = "grossAmount"
  val KEY_GROSS_GAIN = "grossGain"
  val KEY_GROSS_GAIN_PCT = "grossGainPct"

  private var detailsLabels = Map.empty[String, Label]

  // Asset fields.
  // Note: declare the fields through a def so that changing language applies
  // upon reloading view.
  // Order here is the one the fields will appear in the asset details pane
  // and table columns.
  def fields() = List(
    AssetTextField(KEY_SCHEME, Strings.scheme, Strings.schemeColon, AssetField.formatScheme, AssetField.schemeComment),
    AssetTextField(KEY_FUND, Strings.fund, Strings.fundColon, AssetField.formatFund, AssetField.fundComment),
    AssetDateField(KEY_AVAILABILITY, Strings.availability, Strings.availabilityColon, AssetField.formatAvailability, AssetField.availability),
    AssetAmountField(KEY_UNITS, Strings.units, Strings.unitsColon, AssetField.formatUnits, AssetField.units),
    AssetAmountField(KEY_VWAP, Strings.vwap, Strings.vwapColon, AssetField.formatVWAP, AssetField.vwap),
    AssetAmountField(KEY_NAV, Strings.nav, Strings.navColon, AssetField.formatNAV, AssetField.nav),
    AssetDateField(KEY_DATE, Strings.date, Strings.dateColon, AssetField.formatDate, AssetField.date),
    AssetAmountField(KEY_INVESTED_AMOUNT, Strings.invested, Strings.investedAmountColon, AssetField.formatInvestedAmount, AssetField.investedAmount),
    AssetAmountField(KEY_GROSS_AMOUNT, Strings.gross, Strings.grossAmountColon, AssetField.formatGrossAmount, AssetField.grossAmount),
    AssetColoredAmountField(KEY_GROSS_GAIN, Strings.gross, Strings.grossGainColon, AssetField.formatGrossGain, AssetField.grossGain),
    AssetColoredAmountField(KEY_GROSS_GAIN_PCT, Strings.grossPct, Strings.grossGainPctColon, AssetField.formatGrossGainPct, AssetField.grossGainPct)
  ).map { field =>
    field.key -> field
  }.foldLeft(ListMap.empty[String, AssetField[_]])(_ + _)

  val bigDecimalComparator = Comparators.optionComparator[BigDecimal]
  val localDateComparator = Comparators.optionComparator[LocalDate]

  def amountComparator(value: AssetDetails => Option[BigDecimal]): Comparator[AssetDetails] = {
    new Comparator[AssetDetails] {
      override def compare(o1: AssetDetails, o2: AssetDetails): Int =
        bigDecimalComparator.compare(value(o1), value(o2))
    }
  }
  def dateComparator(value: AssetDetails => Option[LocalDate]): Comparator[AssetDetails] = {
    new Comparator[AssetDetails] {
      override def compare(o1: AssetDetails, o2: AssetDetails): Int =
        localDateComparator.compare(value(o1), value(o2))
    }
  }

  def formatScheme(details: AssetDetails, long: Boolean) = details.scheme.name
  def schemeComment(details: AssetDetails) = details.scheme.comment
  def formatFund(details: AssetDetails, long: Boolean) = details.fund.name
  def fundComment(details: AssetDetails) = details.fund.comment
  def formatAvailability(details: AssetDetails, long: Boolean) = details.formatAvailability(long)
  def availability(details: AssetDetails) = details.availability
  def formatUnits(details: AssetDetails, long: Boolean) = details.formatUnits
  def units(details: AssetDetails) = Some(details.units)
  def formatVWAP(details: AssetDetails, long: Boolean) = details.formatVWAP
  def vwap(details: AssetDetails) = Some(details.vwap)
  def formatDate(details: AssetDetails, long: Boolean) = details.formatDate
  def date(details: AssetDetails) = details.date
  def formatNAV(details: AssetDetails, long: Boolean) = details.formatNAV
  def nav(details: AssetDetails) = details.nav
  def formatInvestedAmount(details: AssetDetails, long: Boolean) = details.formatInvestedAmount
  def investedAmount(details: AssetDetails) = Some(details.investedAmount)
  def formatGrossAmount(details: AssetDetails, long: Boolean) = details.formatGrossAmount
  def grossAmount(details: AssetDetails) = details.grossAmount
  def formatGrossGain(details: AssetDetails, long: Boolean) = details.formatGrossGain
  def grossGain(details: AssetDetails) = details.grossGain
  def formatGrossGainPct(details: AssetDetails, long: Boolean) = details.formatGrossGainPct
  def grossGainPct(details: AssetDetails) = details.grossGainPct

  // Note: there need to be distinct ImageView instances to display an image
  // more than once.
  def tooltipHint = new ImageView(Images.iconInformationBalloon)

}
