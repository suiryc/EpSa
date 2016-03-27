package epsa.controllers

import epsa.I18N
import epsa.model.Savings
import java.time.LocalDate
import javafx.css.PseudoClass
import javafx.scene.Node
import javafx.scene.control.{Control, Tooltip}
import javafx.scene.image.ImageView

object Form {

  private val warningClass = PseudoClass.getPseudoClass("warning")

  private val errorClass = PseudoClass.getPseudoClass("error")

  private val imageButtonClass = "image-button"

  private def setPseudoClass(node: Node, pseudoClass: PseudoClass, set: Boolean): Unit =
  // See: http://stackoverflow.com/a/24231728
    node.pseudoClassStateChanged(pseudoClass, set)

  def toggleStyles(node: Control, msgOpt: Option[String], styles: Style*): Unit = {
    // Apply all style changes
    // Group by style and check whether at least one instance is set
    styles.groupBy(_.pseudoClass).mapValues(_.exists(_.set)).foreach {
      case (pseudoClass, set) => setPseudoClass(node, pseudoClass, set)
    }
    // Get the first enabled style provided message, or the default one.
    val opt = styles.find(_.set).map(_.msg).orElse(msgOpt)
    node.setTooltip(opt.filterNot(_.isEmpty).map(new Tooltip(_)).orNull)
  }

  def toggleWarning(node: Control, set: Boolean, msgOpt: Option[String] = None): Unit = {
    setPseudoClass(node, warningClass, set)
    node.setTooltip(msgOpt.map(new Tooltip(_)).orNull)
  }

  def toggleError(node: Control, set: Boolean, msgOpt: Option[String] = None): Unit = {
    setPseudoClass(node, errorClass, set)
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
      Savings.resolveAvailability(availability, date) match {
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

  trait Style {
    val pseudoClass: PseudoClass
    val set: Boolean
    val msg: String
  }

  case class ErrorStyle(set: Boolean, msg: String = "") extends Style {
    override val pseudoClass = errorClass
  }

  case class WarningStyle(set: Boolean, msg: String = "") extends Style {
    override val pseudoClass = warningClass
  }

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
