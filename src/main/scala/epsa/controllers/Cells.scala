package epsa.controllers

import epsa.model.Savings
import java.time.LocalDate
import javafx.scene.control.ListCell
import suiryc.scala.javafx.scene.control.{CellWithSeparator, ListCellEx, TableCellEx}
import suiryc.scala.util.I18NLocale

class SchemeCell extends ListCellEx[Savings.Scheme] {
  override def itemText(item: Savings.Scheme) = item.name
}

class FundCell extends ListCellEx[Savings.Fund] {
  override def itemText(item: Savings.Fund) = item.name
}

class SchemeAndFundCell extends ListCell[Option[SchemeAndFund]] with CellWithSeparator[SchemeAndFund] {
  override def itemText(item: SchemeAndFund) = s"${item.fund.name} / ${item.scheme.name}"
}

class AvailabilityListCell(baseOpt: Option[LocalDate]) extends ListCellEx[Option[LocalDate]] {
  override def itemText(item: Option[LocalDate]) = Form.formatAvailability(item, baseOpt, long = false)
}

class AvailabilityTableCell[A] extends TableCellEx[A, Option[LocalDate]] {
  override def itemText(item: Option[LocalDate]) = Form.formatAvailability(item, date = None, long = false)
}

class AmountCell[A](currency: String) extends TableCellEx[A, BigDecimal] {
  override def itemText(item: BigDecimal) = Form.formatAmount(item, currency)
}

class I18NLocaleCell extends ListCellEx[I18NLocale] {
  override def itemText(item: I18NLocale) = item.displayName
}
