package beautycontest.Models

import org.scalajs.dom.html
import scalatags.JsDom.all.div

object inGameItems {

  // className are css classes defined
  def newDivOfClass(className: String):html.Div = {
    val toR = div().render
    toR.className = className
    if (!className.contains("container"))
      toR.textContent = className
    toR
  }

}