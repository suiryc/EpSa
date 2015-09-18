package epsa.controllers

import epsa.I18N
import epsa.model.Savings
import java.time.LocalDate
import javafx.scene.control.{ListCell, TableCell}
import suiryc.scala.util.I18NLocale

class SchemeCell
  extends ListCell[Savings.Scheme]
{

  override protected def updateItem(item: Savings.Scheme, empty: Boolean) {
    super.updateItem(item, empty)
    if (empty) setText(null)
    else setText(item.name)
  }

}

class FundCell
  extends ListCell[Savings.Fund]
{

  override protected def updateItem(item: Savings.Fund, empty: Boolean) {
    super.updateItem(item, empty)
    if (empty) setText(null)
    else setText(item.name)
  }

}

class AvailabilityCell[A]
  extends TableCell[A, Option[LocalDate]]
{

  override protected def updateItem(item: Option[LocalDate], empty: Boolean) {
    super.updateItem(item, empty)
    if (empty) setText(null)
    else setText(Form.formatAvailability(item))
  }

}

class AmountCell[A]
  extends TableCell[A, BigDecimal]
{

  override protected def updateItem(item: BigDecimal, empty: Boolean) {
    super.updateItem(item, empty)
    if (empty) setText(null)
    else setText(Form.formatAmount(item))
  }

}

class I18NLocaleCell
  extends ListCell[I18NLocale]
{

  override protected def updateItem(item: I18NLocale, empty: Boolean) {
    super.updateItem(item, empty)
    if (empty) setText(null)
    else setText(item.displayName)
  }

}
