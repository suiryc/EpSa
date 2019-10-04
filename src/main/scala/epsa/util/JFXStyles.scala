package epsa.util

import javafx.animation.{KeyFrame, Timeline}
import javafx.css.PseudoClass
import javafx.event.ActionEvent
import javafx.scene.{Node, Scene}
import javafx.scene.control.{Control, Tooltip}
import javafx.scene.image.ImageView
import javafx.util.{Duration => jfxDuration}
import suiryc.scala.javafx.css.Styleables
import suiryc.scala.javafx.scene.{Styles, StylesFeat}

object JFXStyles extends StylesFeat {

  val CLASS_HEADER = "header"
  val CLASS_NO_SELECT = "no-select"
  val CLASS_VALUE_NUMBER = "value-number"

  private val animationHighlightClass = PseudoClass.getPseudoClass("animation-highlight")

  private val positiveClass = PseudoClass.getPseudoClass("value-positive")

  private val negativeClass = PseudoClass.getPseudoClass("value-negative")

  private val imageButtonClass = "image-button"

  override def addStylesheet(scene: Scene): Unit = {
    super.addStylesheet(scene)
    scene.getStylesheets.add(getClass.getResource("/css/form.css").toExternalForm)
    ()
  }

  def toggleStyles(node: Control, msgOpt: Option[String], styles: Style*): Unit = {
    // Apply all style changes
    // Group by style and check whether at least one instance is set
    styles.groupBy(_.pseudoClass).view.mapValues(_.exists(_.active)).foreach {
      case (pseudoClass, set) => togglePseudoClass(node, pseudoClass, set)
    }
    // Get the first enabled style provided message, or the default one.
    val opt = styles.find(_.active).map(_.msg).orElse(msgOpt)
    node.setTooltip(opt.filterNot(_.isEmpty).map(new Tooltip(_)).orNull)
  }

  def toggleAnimationHighlight(node: Node, active: Boolean): Unit = {
    togglePseudoClass(node, animationHighlightClass, active)
  }

  def togglePositive(node: Node): Unit = {
    togglePseudoClass(node, negativeClass, active = false)
    togglePseudoClass(node, positiveClass, active = true)
  }

  def toggleNegative(node: Node): Unit = {
    togglePseudoClass(node, positiveClass, active = false)
    togglePseudoClass(node, negativeClass, active = true)
  }

  def toggleNeutral(node: Node): Unit = {
    togglePseudoClass(node, positiveClass, active = false)
    togglePseudoClass(node, negativeClass, active = false)
  }

  def toggleImageButtonStyle(node: Node, active: Boolean): Unit = {
    Styleables.toggleStyleClass(node, imageButtonClass, set = active)
  }

  /**
   * Toggles image button status.
   *
   * When activated, sets 'image-button' style class and have node opaque.
   * Otherwise unsets 'image-button' style class and have node 60% transparent.
   * Also installs/uninstalls tooltip message.
   */
  def toggleImageButton(node: ImageView, active: Boolean, msgOpt: Option[String] = None): Unit = {
    // Note: do not disable node otherwise tooltip won't work
    toggleImageButtonStyle(node, active)
    if (active) node.setOpacity(1.0)
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

  def highlightAnimation(nodes: List[Node], animationHighlighter: Option[AnimationHighlighter], onDone: => Unit = {}): AnimationHighlighter = {
    def toggle(active: Boolean): Unit =
      nodes.foreach { node =>
        toggleAnimationHighlight(node, active)
      }

    animationHighlighter.foreach(_.stop())
    val timeline = new Timeline(
      new KeyFrame(jfxDuration.seconds(0.5), { _: ActionEvent => toggle(active = true) }),
      new KeyFrame(jfxDuration.seconds(1.0), { _: ActionEvent => toggle(active = false) })
    )
    timeline.setCycleCount(3)
    timeline.setOnFinished { _ =>
      onDone
    }
    AnimationHighlighter(nodes, timeline, () => {
      toggle(active = false)
      onDone
    })
  }

  trait Style {
    val pseudoClass: PseudoClass
    val active: Boolean
    val msg: String
  }

  case class ErrorStyle(active: Boolean, msg: String = "") extends Style {
    override val pseudoClass: PseudoClass = Styles.errorClass
  }

  case class WarningStyle(active: Boolean, msg: String = "") extends Style {
    override val pseudoClass: PseudoClass = Styles.warningClass
  }

  case class AnimationHighlighter(rows: List[Node], timeline: Timeline, onStop: () => Unit) {
    timeline.play()
    def stop(): Unit = {
      timeline.stop()
      onStop()
    }
  }

}
