package epsa.controllers

import epsa.model.Savings
import java.time.LocalDate
import javafx.beans.property.{BooleanProperty, SimpleBooleanProperty}
import javafx.scene.control.{Cell, ListCell, TableCell}
import javafx.scene.control.cell.CheckBoxListCell
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.util.Callback
import suiryc.scala.util.I18NLocale

// TODO: move CellEx/ListCellEx/TableCellEx/CheckBoxListCellEx in suiryc-scala-javafx ?

/** Cell extension that knows how to update cell text. */
trait CellEx[A] extends Cell[A] {

  protected def itemText(item: A): String

  override protected def updateItem(item: A, empty: Boolean) {
    super.updateItem(item, empty)
    if (empty) setText(null)
    else setText(itemText(item))
  }

}

/** ListCell extension with CellEx. */
trait ListCellEx[A] extends ListCell[A] with CellEx[A]

/** TableCell extension with CellEx. */
trait TableCellEx[A, B] extends TableCell[A, B] with CellEx[B]

class SchemeCell extends ListCellEx[Savings.Scheme] {
  override def itemText(item: Savings.Scheme) = item.name
}

class FundCell extends ListCellEx[Savings.Fund] {
  override def itemText(item: Savings.Fund) = item.name
}

class SchemeAndFundCell extends ListCellEx[SchemeAndFund] {
  override def itemText(item: SchemeAndFund) = s"${item.fund.name} / ${item.scheme.name}"
}

class AvailabilityListCell(baseOpt: Option[LocalDate]) extends ListCellEx[Option[LocalDate]] {
  override def itemText(item: Option[LocalDate]) = Form.formatAvailability(item, baseOpt, long = false)
}

class AvailabilityTableCell[A] extends TableCellEx[A, Option[LocalDate]] {
  override def itemText(item: Option[LocalDate]) = Form.formatAvailability(item, date = None, long = false)
}

class AmountCell[A] extends TableCellEx[A, BigDecimal] {
  override def itemText(item: BigDecimal) = Form.formatAmount(item)
}

class I18NLocaleCell extends ListCellEx[I18NLocale] {
  override def itemText(item: I18NLocale) = item.displayName
}

/**
 * CheckBox ListCell extension.
 *
 * Automatically updates Cell according to content by setting text, checkbox
 * selection and cell disabling if value is locked.
 *
 * @tparam A cell data type
 */
abstract class CheckBoxListCellEx[A] extends CheckBoxListCell[A] {

  import CheckBoxListCellEx._

  protected def getInfo(item: A): CellInfo

  protected def setLocked(locked: Boolean): Unit

  protected def statusChanged(oldValue: Boolean, newValue: Boolean): Unit

  setSelectedStateCallback(Callback { item =>
    // We are supposed to have an item
    Option(item).map { _ =>
      val info = getInfo(item)
      val property = info.observable
      property.set(info.checked)
      property.listen { (_, v0, v1) =>
        statusChanged(v0, v1)
      }
      property
    }.getOrElse(new SimpleBooleanProperty())
  })

  override protected def updateItem(item: A, empty: Boolean) {
    super.updateItem(item, empty)
    if (empty) setText(null)
    else {
      val info = getInfo(item)
      setText(info.text)
      setDisabled(info.locked)
      setLocked(info.locked)
    }
  }

}

object CheckBoxListCellEx {
  case class CellInfo(text: String, observable: BooleanProperty, checked: Boolean, locked: Boolean)
}
