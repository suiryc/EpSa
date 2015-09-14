package epsa.controllers

import javafx.css.PseudoClass
import javafx.scene.Node
import javafx.scene.control.Tooltip
import javafx.scene.image.ImageView

object Form {

  private val errorClass = PseudoClass.getPseudoClass("error")

  private val imageButtonClass = "image-button"

  def setStyleError(node: Node, set: Boolean): Unit =
  // See: http://stackoverflow.com/a/24231728
    setPseudoClass(node, errorClass, set)

  def setStyleImageButton(node: Node, set: Boolean): Unit = {
    if (set && !node.getStyleClass.contains(imageButtonClass)) node.getStyleClass.add(imageButtonClass)
    else if (!set) node.getStyleClass.remove(imageButtonClass)
  }

  private def setPseudoClass(node: Node, pseudoClass: PseudoClass, set: Boolean): Unit =
    node.pseudoClassStateChanged(pseudoClass, set)

  /**
   * Toggles image button status.
   *
   * When set, sets 'image-button' style class and have node opaque.
   * Otherwise unsets 'image-button' style class and have node 60% transparent.
   * Also installs/uninstalls tooltip message.
   */
  def toggleImageButton(node: ImageView, set: Boolean, msgOpt: Option[String] = None): Unit = {
    // Note: do not disable node otherwise tooltip won't work
    if (set) {
      Form.setStyleImageButton(node, set = true)
      node.setOpacity(1.0)
    }
    else {
      Form.setStyleImageButton(node, set = false)
      node.setOpacity(0.4)
    }

    msgOpt match {
      case Some(msg) =>
        Tooltip.install(node, new Tooltip(msg))

      case None =>
        // Note: uninstall takes a Tooltip but does not use it (and we did not
        // keep the installed tooltip if any).
        Tooltip.uninstall(node, null)
    }
  }

}
