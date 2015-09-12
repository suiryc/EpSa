package epsa.controllers

import javafx.css.PseudoClass
import javafx.scene.Node

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

}
