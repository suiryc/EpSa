package epsa.util

import javafx.animation.{KeyFrame, Timeline}
import javafx.css.PseudoClass
import javafx.event.ActionEvent
import javafx.scene.Node
import javafx.scene.control.{Control, Tooltip}
import javafx.scene.image.ImageView
import javafx.util.{Duration => jfxDuration}
import suiryc.scala.javafx.event.EventHandler._

object JFXStyles {

  private val warningClass = PseudoClass.getPseudoClass("warning")

  private val errorClass = PseudoClass.getPseudoClass("error")

  private val animationHighlightClass = PseudoClass.getPseudoClass("animation-highlight")

  private val positiveClass = PseudoClass.getPseudoClass("value-positive")

  private val negativeClass = PseudoClass.getPseudoClass("value-negative")

  private val imageButtonClass = "image-button"

  private def setPseudoClass(node: Node, pseudoClass: PseudoClass, set: Boolean): Unit =
  // See: http://stackoverflow.com/a/24231728
    node.pseudoClassStateChanged(pseudoClass, set)

  def togglePseudoClass(node: Node, pseudoClass: String, set: Boolean): Unit =
    node.pseudoClassStateChanged(PseudoClass.getPseudoClass(pseudoClass), set)

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

  def toggleAnimationHighlight(node: Node, set: Boolean): Unit = {
    setPseudoClass(node, animationHighlightClass, set)
  }

  def togglePositive(node: Node): Unit = {
    setPseudoClass(node, negativeClass, set = false)
    setPseudoClass(node, positiveClass, set = true)
  }

  def toggleNegative(node: Node): Unit = {
    setPseudoClass(node, positiveClass, set = false)
    setPseudoClass(node, negativeClass, set = true)
  }

  def toggleNeutral(node: Node): Unit = {
    setPseudoClass(node, positiveClass, set = false)
    setPseudoClass(node, negativeClass, set = false)
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

  def highlightAnimation(nodes: List[Node], animationHighlighter: Option[AnimationHighlighter]): AnimationHighlighter = {
    def toggle(set: Boolean): Unit =
      nodes.foreach { node =>
        toggleAnimationHighlight(node, set = set)
      }

    animationHighlighter.foreach(_.stop())
    val timeline = new Timeline(
      new KeyFrame(jfxDuration.seconds(0.5), { _: ActionEvent => toggle(set = true) }),
      new KeyFrame(jfxDuration.seconds(1.0), { _: ActionEvent => toggle(set = false) })
    )
    timeline.setCycleCount(3)
    AnimationHighlighter(nodes, timeline, () => toggle(set = false))
  }

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

  case class AnimationHighlighter(rows: List[Node], timeline: Timeline, onStop: () => Unit) {
    timeline.play()
    def stop(): Unit = {
      timeline.stop()
      onStop()
    }
  }

}
