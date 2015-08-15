package epsa.controllers

import javafx.css.PseudoClass
import javafx.scene.Node

object Form {

  val errorClass = PseudoClass.getPseudoClass("error")

  def setStyleError(node: Node, set: Boolean): Unit =
  // See: http://stackoverflow.com/a/24231728
    node.pseudoClassStateChanged(errorClass, set)

}
