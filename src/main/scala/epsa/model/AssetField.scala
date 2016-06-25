package epsa.model

import epsa.I18N.Strings
import epsa.controllers.{ColoredCell, FormatCell, Images, WarningCell}
import epsa.util.JFXStyles
import java.time.LocalDate
import java.util.Comparator
import javafx.beans.property.{SimpleObjectProperty, SimpleStringProperty}
import javafx.scene.control.{ContentDisplay, Label, TableColumn, Tooltip}
import javafx.scene.image.ImageView
import javafx.scene.layout.{HBox, Region}
import scala.collection.immutable.ListMap
import suiryc.scala.javafx.util.Callback
import suiryc.scala.math.Ordering.localDateOrdering
import suiryc.scala.util.Comparators

/** Asset field settings. */
trait AssetField[A] {
  /** Field key. */
  val key: String
  /** Column index. */
  val columnIdx: Int
  /** Field name in table. */
  val tableLabel: String
  /** Field name in details pane. */
  val detailsLabel: String
  /** Field comment if any. */
  val comment: (AssetDetails) => Option[String] = { _ => None }
  /** Field warning if any. */
  val warning: (AssetDetails) => Option[String] = { _ => None }
  /** How to format field value. */
  val format: (AssetDetails) => String

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
    detailsValue.setText(assetDetailsOpt.map(format).orNull)
    setupGraphicNode(
      buildGraphicNode(assetDetailsOpt, comment, AssetField.tooltipHint),
      buildGraphicNode(assetDetailsOpt, warning, AssetField.tooltipWarning)
    )
  }

  protected def buildGraphicNode(assetDetailsOpt: Option[AssetDetails], get: (AssetDetails) => Option[String], create: => ImageView): Option[ImageView] = {
    assetDetailsOpt.flatMap(get).map { v =>
      val node = create
      Tooltip.install(node, new Tooltip(v))
      node
    }
  }

  protected def setupGraphicNode(nodes: Option[ImageView]*): Unit = {
    if (nodes.isEmpty) detailsValue.setGraphic(null)
    else {
      val hbox = new HBox()
      hbox.setSpacing(5)
      hbox.getChildren.setAll(nodes.toList.flatten:_*)
      detailsValue.setGraphic(hbox)
    }
  }

}

/** Asset field with formatted text value to display. */
case class AssetTextField(key: String, columnIdx: Int, tableLabel: String, detailsLabel: String,
  format: (AssetDetails) => String,
  override val comment: (AssetDetails) => Option[String] = { _ => None }
) extends AssetField[String] {
  val column = new TableColumn[AssetDetails, String](tableLabel)
  column.setCellValueFactory(Callback { data =>
    new SimpleStringProperty(format(data.getValue))
  })
}

/** Asset field with date to display. */
case class AssetDateField(key: String, columnIdx: Int, tableLabel: String, detailsLabel: String,
  format: (AssetDetails) => String,
  value: (AssetDetails) => Option[LocalDate]
) extends AssetField[AssetDetails] {
  val column = new TableColumn[AssetDetails, AssetDetails](tableLabel)
  column.setCellValueFactory(Callback { data =>
    new SimpleObjectProperty(data.getValue)
  })
  column.setCellFactory(Callback { new FormatCell[AssetDetails, AssetDetails](format) })
  column.setComparator(AssetField.dateComparator(value))
}

/** Asset field with number to display. */
case class AssetNumberField(key: String, columnIdx: Int, tableLabel: String, detailsLabel: String,
  format: (AssetDetails) => String,
  value: (AssetDetails) => Option[BigDecimal],
  override val warning: (AssetDetails) => Option[String] = { _ => None }
) extends AssetField[AssetDetails] {
  val column = new TableColumn[AssetDetails, AssetDetails](tableLabel)
  column.setCellValueFactory(Callback { data =>
    new SimpleObjectProperty(data.getValue)
  })
  val warning0 = warning
  column.setCellFactory(Callback {
    new FormatCell[AssetDetails, AssetDetails](format) with WarningCell[AssetDetails] {
      getStyleClass.add(JFXStyles.CLASS_VALUE_NUMBER)
      override def warning(v: AssetDetails) = warning0(v)
    }
  })
  column.setComparator(AssetField.numberComparator(value))
}

/** Asset field with (colored) number to display. */
case class AssetColoredNumberField(key: String, columnIdx: Int, tableLabel: String, detailsLabel: String,
  format: (AssetDetails) => String,
  value: (AssetDetails) => Option[BigDecimal],
  override val warning: (AssetDetails) => Option[String] = { _ => None }
) extends AssetField[AssetDetails] {
  val column = new TableColumn[AssetDetails, AssetDetails](tableLabel)
  column.setCellValueFactory(Callback { data =>
    new SimpleObjectProperty(data.getValue)
  })
  val value0 = value
  val warning0 = warning
  column.setCellFactory(Callback {
    new FormatCell[AssetDetails, AssetDetails](format) with ColoredCell[AssetDetails] with WarningCell[AssetDetails] {
      getStyleClass.add(JFXStyles.CLASS_VALUE_NUMBER)
      override def value(v: AssetDetails) = value0(v)
      override def warning(v: AssetDetails) = warning0(v)
    }
  })
  column.setComparator(AssetField.numberComparator(value))

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
  val KEY_LEVIES_AMOUNT = "leviesAmount"
  val KEY_NET_AMOUNT = "netAmount"
  val KEY_GROSS_GAIN = "grossGain"
  val KEY_GROSS_GAIN_PCT = "grossGainPct"
  val KEY_NET_GAIN = "netGain"
  val KEY_NET_GAIN_PCT = "netGainPct"

  private var detailsLabels = Map.empty[String, Label]

  // Asset fields.
  // Note: declare the fields through a def so that changing language applies
  // upon reloading view.
  // Order here is the one the fields will appear in the asset details pane
  // and table columns.
  def fields() = List(
    AssetTextField(KEY_SCHEME, 0, Strings.scheme, Strings.schemeColon,
      AssetField.formatScheme, AssetField.schemeComment),
    AssetTextField(KEY_FUND, 0, Strings.fund, Strings.fundColon,
      AssetField.formatFund, AssetField.fundComment),
    AssetDateField(KEY_AVAILABILITY, 0, Strings.availability, Strings.availabilityColon,
      AssetField.formatAvailability, AssetField.availability),
    AssetNumberField(KEY_UNITS, 0, Strings.units, Strings.unitsColon,
      AssetField.formatUnits, AssetField.units),
    AssetNumberField(KEY_VWAP, 0, Strings.vwap, Strings.vwapColon,
      AssetField.formatVWAP, AssetField.vwap),
    AssetNumberField(KEY_NAV, 0, Strings.nav, Strings.navColon,
      AssetField.formatNAV, AssetField.nav),
    AssetDateField(KEY_DATE, 0, Strings.date, Strings.dateColon,
      AssetField.formatDate, AssetField.date),
    AssetNumberField(KEY_INVESTED_AMOUNT, 1, Strings.invested, Strings.investedAmountColon,
      AssetField.formatInvestedAmount, AssetField.investedAmount),
    AssetNumberField(KEY_GROSS_AMOUNT, 1, Strings.gross, Strings.grossAmountColon,
      AssetField.formatGrossAmount, AssetField.grossAmount, AssetField.grossAmountWarning),
    AssetNumberField(KEY_LEVIES_AMOUNT, 1, Strings.levies, Strings.leviesAmountColon,
      AssetField.formatLeviesAmount, AssetField.leviesAmount, AssetField.leviesWarning),
    AssetNumberField(KEY_NET_AMOUNT, 1, Strings.net, Strings.netAmountColon,
      AssetField.formatNetAmount, AssetField.netAmount, AssetField.leviesWarning),
    AssetColoredNumberField(KEY_GROSS_GAIN, 1, Strings.gross, Strings.grossGainColon,
      AssetField.formatGrossGain, AssetField.grossGain, AssetField.grossAmountWarning),
    AssetColoredNumberField(KEY_GROSS_GAIN_PCT, 1, Strings.grossPct, Strings.grossGainPctColon,
      AssetField.formatGrossGainPct, AssetField.grossGainPct, AssetField.grossAmountWarning),
    AssetColoredNumberField(KEY_NET_GAIN, 1, Strings.net, Strings.netGainColon,
      AssetField.formatNetGain, AssetField.netGain, AssetField.leviesWarning),
    AssetColoredNumberField(KEY_NET_GAIN_PCT, 1, Strings.netPct, Strings.netGainPctColon,
      AssetField.formatNetGainPct, AssetField.netGainPct, AssetField.leviesWarning)
  ).map { field =>
    field.key -> field
  }.foldLeft(ListMap.empty[String, AssetField[_]])(_ + _)

  val bigDecimalComparator = Comparators.optionComparator[BigDecimal]
  val localDateComparator = Comparators.optionComparator[LocalDate]

  def numberComparator(value: AssetDetails => Option[BigDecimal]): Comparator[AssetDetails] = {
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

  def formatScheme(details: AssetDetails) = details.scheme.name
  def schemeComment(details: AssetDetails) = details.scheme.comment
  def formatFund(details: AssetDetails) = details.fund.name
  def fundComment(details: AssetDetails) = details.fund.comment
  def formatAvailability(details: AssetDetails) = details.formatAvailability
  def availability(details: AssetDetails) = details.availability
  def formatUnits(details: AssetDetails) = details.formatUnits
  def units(details: AssetDetails) = Some(details.units)
  def formatVWAP(details: AssetDetails) = details.formatVWAP
  def vwap(details: AssetDetails) = Some(details.vwap)
  def formatDate(details: AssetDetails) = details.formatDate
  def date(details: AssetDetails) = details.date
  def formatNAV(details: AssetDetails) = details.formatNAV
  def nav(details: AssetDetails) = details.nav
  def formatInvestedAmount(details: AssetDetails) = details.formatInvestedAmount
  def investedAmount(details: AssetDetails) = Some(details.investedAmount)
  def formatGrossAmount(details: AssetDetails) = details.formatGrossAmount
  def grossAmount(details: AssetDetails) = details.grossAmount
  def grossAmountWarning(details: AssetDetails) = Option(details.grossAmountWarning.mkString("\n")).filterNot(_.isEmpty)
  def leviesWarning(details: AssetDetails) = Option {
    (details.grossAmountWarning ::: details.leviesWarning).distinct.mkString("\n")
  }.filterNot(_.isEmpty)
  def formatLeviesAmount(details: AssetDetails) = details.formatLeviesAmount
  def leviesAmount(details: AssetDetails) = details.leviesAmount
  def formatNetAmount(details: AssetDetails) = details.formatNetAmount
  def netAmount(details: AssetDetails) = details.netAmount
  def formatGrossGain(details: AssetDetails) = details.formatGrossGain
  def grossGain(details: AssetDetails) = details.grossGain
  def formatGrossGainPct(details: AssetDetails) = details.formatGrossGainPct
  def grossGainPct(details: AssetDetails) = details.grossGainPct
  def formatNetGain(details: AssetDetails) = details.formatNetGain
  def netGain(details: AssetDetails) = details.netGain
  def formatNetGainPct(details: AssetDetails) = details.formatNetGainPct
  def netGainPct(details: AssetDetails) = details.netGainPct

  // Note: there need to be distinct ImageView instances to display an image
  // more than once.
  def tooltipHint = new ImageView(Images.iconInformationBalloon)
  def tooltipWarning = new ImageView(Images.iconExclamationRed)

}
