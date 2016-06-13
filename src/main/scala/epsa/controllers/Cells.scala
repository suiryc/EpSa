package epsa.controllers

import epsa.I18N.Strings
import epsa.model.Savings
import epsa.util.JFXStyles
import java.time.{LocalDate, Month}
import java.time.format.TextStyle
import java.util.Locale
import javafx.scene.control.{Cell, ContentDisplay, ListCell, Tooltip}
import javafx.scene.image.ImageView
import suiryc.scala.javafx.scene.control.{CellWithSeparator, ListCellEx, TableCellEx}
import suiryc.scala.util.I18NLocale

class SchemeCell extends ListCell[Option[Savings.Scheme]] with CellWithSeparator[Savings.Scheme] {
  override protected def itemText(item: Savings.Scheme) = item.name
}

class FundCell extends ListCell[Option[Savings.Fund]] with CellWithSeparator[Savings.Fund] {
  override protected def itemText(item: Savings.Fund) = item.name
}

class SchemeAndFundCell extends ListCell[Option[SchemeAndFund]] with CellWithSeparator[SchemeAndFund] {
  override protected def itemText(item: SchemeAndFund) = s"${item.fund.name} / ${item.scheme.name}"
}

class AvailabilityListCell(baseOpt: Option[LocalDate]) extends ListCellEx[Option[LocalDate]] {
  override protected def itemText(item: Option[LocalDate]) = Form.formatAvailability(item, baseOpt)
}

class UnavailabilityPeriodCell extends ListCellEx[Savings.UnavailabilityPeriod] {
  override protected def itemText(item: Savings.UnavailabilityPeriod) = item.id
}

class MonthListCell extends ListCellEx[Option[Month]] {
  override protected def itemText(item: Option[Month]) =
    item.map { month =>
      val name = month.getDisplayName(TextStyle.FULL, Locale.getDefault)
      "%02d - %s".format(month.getValue, name.capitalize)
    }.getOrElse(Strings.na)
}

class FormatCell[A, B](format: B => String) extends TableCellEx[A, B] {
  override protected def itemText(item: B) = format(item)
}

trait ColoredCell[A] extends Cell[A] {
  def value(a: A): Option[BigDecimal]
  override protected def updateItem(item: A, empty: Boolean) {
    super.updateItem(item, empty)
    (if (empty) None else Some(item)).flatMap(value).find(_ != 0) match {
      case Some(v) =>
        if (v > 0) JFXStyles.togglePositive(this)
        else JFXStyles.toggleNegative(this)

      case None =>
        JFXStyles.toggleNeutral(this)
    }
  }
}

trait WarningCell[A] extends Cell[A] {
  // Display graphic on the right side
  setContentDisplay(ContentDisplay.RIGHT)

  def warning(a: A): Option[String]
  override protected def updateItem(item: A, empty: Boolean) {
    super.updateItem(item, empty)
    (if (empty) None else Some(item)).flatMap(warning) match {
      case Some(v) =>
        setTooltip(new Tooltip(v))
        setGraphic(new ImageView(Images.iconExclamationRed))

      case None =>
        setTooltip(null)
        setGraphic(null)
    }
  }
}

class I18NLocaleCell extends ListCellEx[I18NLocale] {
  override protected def itemText(item: I18NLocale) = item.displayName
}
