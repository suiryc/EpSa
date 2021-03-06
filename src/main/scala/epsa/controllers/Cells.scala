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

class SchemeCell extends ListCell[Option[Savings.Scheme]] with CellWithSeparator[Savings.Scheme] {
  override protected def itemText(item: Savings.Scheme): String = item.name
}

class FundCell extends ListCell[Option[Savings.Fund]] with CellWithSeparator[Savings.Fund] {
  override protected def itemText(item: Savings.Fund): String = item.name
}

class SchemeAndFundCell extends ListCell[Option[SchemeAndFund]] with CellWithSeparator[SchemeAndFund] {
  override protected def itemText(item: SchemeAndFund) = s"${item.fund.name} / ${item.scheme.name}"
}

class AvailabilityListCell(baseOpt: Option[LocalDate]) extends ListCellEx[Option[LocalDate]] {
  override protected def itemText(item: Option[LocalDate]): String = Form.formatAvailability(item, baseOpt)
}

class UnavailabilityPeriodCell extends ListCellEx[Savings.UnavailabilityPeriod] {
  override protected def itemText(item: Savings.UnavailabilityPeriod): String = item.id
}

class MonthListCell extends ListCellEx[Option[Month]] {
  override protected def itemText(item: Option[Month]): String =
    item.map { month =>
      val name = month.getDisplayName(TextStyle.FULL, Locale.getDefault)
      "%02d - %s".format(month.getValue, name.capitalize)
    }.getOrElse(Strings.na)
}

class FormatCell[A, B](format: B => String) extends TableCellEx[A, B] {
  override protected def itemText(item: B): String = format(item)
}

trait ColoredCell[A] extends Cell[A] {
  def value(a: A): Option[BigDecimal]
  override protected def updateItem(item: A, empty: Boolean): Unit = {
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
  // Display graphic on the right side (may be overridden in CSS)
  setContentDisplay(ContentDisplay.RIGHT)

  def warning(a: A): Option[String]
  override protected def updateItem(item: A, empty: Boolean): Unit = {
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
