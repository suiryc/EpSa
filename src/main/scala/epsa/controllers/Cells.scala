package epsa.controllers

import epsa.I18N
import epsa.model.Savings
import javafx.scene.control.{ListCell, TableCell}

class FundCell
  extends ListCell[Savings.Fund]
{

  override protected def updateItem(item: Savings.Fund, empty: Boolean) {
    super.updateItem(item, empty)
    if (empty) setText(null)
    else setText(item.name)
  }

}

class SchemeCell
  extends ListCell[Savings.Scheme]
{

  override protected def updateItem(item: Savings.Scheme, empty: Boolean) {
    super.updateItem(item, empty)
    if (empty) setText(null)
    else setText(item.name)
  }

}

class I18NLocaleCell
  extends ListCell[I18N.I18NLocale]
{

  override protected def updateItem(item: I18N.I18NLocale, empty: Boolean) {
    super.updateItem(item, empty)
    if (empty) setText(null)
    else setText(item.displayName)
  }

}

class AmountCell[A]
  extends TableCell[A, BigDecimal]
{

  override protected def updateItem(item: BigDecimal, empty: Boolean) {
    super.updateItem(item, empty)
    // TODO - currency as setting ?
    if (empty) setText(null)
    else setText(s"$item €")
  }

}
