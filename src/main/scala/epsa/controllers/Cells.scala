package epsa.controllers

import epsa.model.Savings
import java.time.LocalDate
import javafx.beans.property.{BooleanProperty, SimpleBooleanProperty}
import javafx.scene.control.{ListCell, TableCell}
import javafx.scene.control.cell.CheckBoxListCell
import suiryc.scala.javafx.beans.value.RichObservableValue._
import suiryc.scala.javafx.util.Callback
import suiryc.scala.util.I18NLocale

// TODO: use a common 'ListCellEx'/'TableCellEx' class/trait with 'itemText: String' function to implement

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

class SchemeAndFundCell
  extends ListCell[SchemeAndFund]
{

  override protected def updateItem(item: SchemeAndFund, empty: Boolean) {
    super.updateItem(item, empty)
    if (empty) setText(null)
    else setText(s"${item.fund.name} / ${item.scheme.name}")
  }

}

class AvailabilityListCell(baseOpt: Option[LocalDate])
  extends ListCell[Option[LocalDate]]
{

  override protected def updateItem(item: Option[LocalDate], empty: Boolean) {
    super.updateItem(item, empty)
    if (empty) setText(null)
    else setText(Form.formatAvailability(item, baseOpt, long = false))
  }

}

class AvailabilityTableCell[A]
  extends TableCell[A, Option[LocalDate]]
{

  override protected def updateItem(item: Option[LocalDate], empty: Boolean) {
    super.updateItem(item, empty)
    if (empty) setText(null)
    else setText(Form.formatAvailability(item, date = None, long = false))
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

/**
 * CheckBox ListCell extension.
 *
 * Automatically updates Cell according to content by setting text, checkbox
 * selection and cell disabling if value is locked.
 *
 * @tparam A cell data type
 */
// TODO: move in suiryc-scala-javafx ?
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
