package epsa.controllers

import epsa.I18N
import epsa.model.Savings
import java.time.LocalDate
import javafx.css.PseudoClass
import javafx.scene.Node
import javafx.scene.control.{Control, Tooltip}
import javafx.scene.image.ImageView

object Form {

  private val errorClass = PseudoClass.getPseudoClass("error")

  private val imageButtonClass = "image-button"

  def setStyleError(node: Node, set: Boolean): Unit =
  // See: http://stackoverflow.com/a/24231728
    setPseudoClass(node, errorClass, set)

  private def setPseudoClass(node: Node, pseudoClass: PseudoClass, set: Boolean): Unit =
    node.pseudoClassStateChanged(pseudoClass, set)

  def toggleError(node: Control, set: Boolean, msgOpt: Option[String] = None): Unit = {
    setStyleError(node, set)
    node.setTooltip(msgOpt.map(new Tooltip(_)).orNull)
  }

  def setStyleImageButton(node: Node, set: Boolean): Unit = {
    if (set && !node.getStyleClass.contains(imageButtonClass)) node.getStyleClass.add(imageButtonClass)
    else if (!set) node.getStyleClass.remove(imageButtonClass)
  }

  /**
   * Toggles image button status.
   *
   * When set, sets 'image-button' style class and have node opaque.
   * Otherwise unsets 'image-button' style class and have node 60% transparent.
   * Also installs/uninstalls tooltip message.
   */
  def toggleImageButton(node: ImageView, set: Boolean, msgOpt: Option[String] = None): Unit = {
    // Note: do not disable node otherwise tooltip won't work
    setStyleImageButton(node, set)
    if (set) node.setOpacity(1.0)
    else node.setOpacity(0.4)

    msgOpt match {
      case Some(msg) =>
        Tooltip.install(node, new Tooltip(msg))

      case None =>
        // Note: uninstall takes a Tooltip but does not use it (and we did not
        // keep the installed tooltip if any).
        Tooltip.uninstall(node, null)
    }
  }

  def formatAvailability(availability: Option[LocalDate], date: Option[LocalDate], long: Boolean): String =
    availability.map { avail =>
      Savings.resolveAvailablity(availability, date) match {
        case Some(_) =>
          avail.toString

        case None =>
          // Actually available
          I18N.getResources.getString("available") +
            (if (long) s" ($avail)" else "")
      }
    }.getOrElse(I18N.getResources.getString("available"))

  def formatAmount(amount: BigDecimal, currency: String): String =
    s"$amount $currency"

}

object AssetActionKind extends Enumeration {
  val Payment = Value
  val Transfer = Value
  val Refund = Value
}

case class SchemeAndFund(scheme: Savings.Scheme, fund: Savings.Fund) extends Ordered[SchemeAndFund] {

  def compare(other: SchemeAndFund): Int = {
    val c1 = scheme.name.compare(other.scheme.name)
    if (c1 != 0) c1
    else fund.name.compare(other.fund.name)
  }

}
