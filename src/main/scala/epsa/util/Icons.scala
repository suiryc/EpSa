package epsa.util

import suiryc.scala.javafx.scene.Graphics._

object Icons extends IconBuilders {

  private val PREFIX = "icon-"

  // Font Awesome 5 SVG icons.
  // Font Awesome by Dave Gandy - http://fontawesome.io
  // License: http://fontawesome.io/license

  // https://fontawesome.com/icons/map-pin?style=solid
  val mapPin: Builder = iconBuilder(s"${PREFIX}map-pin") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgWidth = 288, svgHeight = 512, targetSvgSize = targetSvgSize, targetSize = targetSvgSize, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M112 316.94v156.69l22.02 33.02c4.75 7.12 15.22 7.12 19.97 0L176 473.63V316.94c-10.39 1.92-21.06 3.06-32 3.06s-21.61-1.14-32-3.06zM144 0C64.47 0 0 64.47 0 144s64.47 144 144 144 144-64.47 144-144S223.53 0 144 0zm0 76c-37.5 0-68 30.5-68 68 0 6.62-5.38 12-12 12s-12-5.38-12-12c0-50.73 41.28-92 92-92 6.62 0 12 5.38 12 12s-5.38 12-12 12z")
    )
  }

  // https://fontawesome.com/icons/search-plus?style=solid
  val searchPlus: Builder = iconBuilder(s"${PREFIX}search-plus") { (styleClass, targetSvgSize) =>
    val params = SVGGroupParams(svgSize = 512, svgTranslate = 16, targetSvgSize = targetSvgSize, targetSize = targetSvgSize, styleClass = styleClass)
    SVGGroup(params,
      svgPath("M304 192v32c0 6.6-5.4 12-12 12h-56v56c0 6.6-5.4 12-12 12h-32c-6.6 0-12-5.4-12-12v-56h-56c-6.6 0-12-5.4-12-12v-32c0-6.6 5.4-12 12-12h56v-56c0-6.6 5.4-12 12-12h32c6.6 0 12 5.4 12 12v56h56c6.6 0 12 5.4 12 12zm201 284.7L476.7 505c-9.4 9.4-24.6 9.4-33.9 0L343 405.3c-4.5-4.5-7-10.6-7-17V372c-35.3 27.6-79.7 44-128 44C93.1 416 0 322.9 0 208S93.1 0 208 0s208 93.1 208 208c0 48.3-16.4 92.7-44 128h16.3c6.4 0 12.5 2.5 17 7l99.7 99.7c9.3 9.4 9.3 24.6 0 34zM344 208c0-75.2-60.8-136-136-136S72 132.8 72 208s60.8 136 136 136 136-60.8 136-136z")
    )
  }

}
