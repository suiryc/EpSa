package epsa.controllers

import epsa.model.Savings
import epsa.util.JFXStyles
import java.time.LocalDate
import javafx.scene.control.{Cell, ListCell}
import suiryc.scala.javafx.scene.control.{CellWithSeparator, ListCellEx, TableCellEx}
import suiryc.scala.util.I18NLocale

class SchemeCell extends ListCellEx[Savings.Scheme] {
  override protected def itemText(item: Savings.Scheme) = item.name
}

class FundCell extends ListCellEx[Savings.Fund] {
  override protected def itemText(item: Savings.Fund) = item.name
}

class SchemeAndFundCell extends ListCell[Option[SchemeAndFund]] with CellWithSeparator[SchemeAndFund] {
  override protected def itemText(item: SchemeAndFund) = s"${item.fund.name} / ${item.scheme.name}"
}

class AvailabilityListCell(baseOpt: Option[LocalDate]) extends ListCellEx[Option[LocalDate]] {
  override protected def itemText(item: Option[LocalDate]) = Form.formatAvailability(item, baseOpt, long = false)
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

class I18NLocaleCell extends ListCellEx[I18NLocale] {
  override protected def itemText(item: I18NLocale) = item.displayName
}
