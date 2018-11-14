package beautycontest

import org.scalajs.dom

object Helpers {

  def rround2(x: Float): Double = (math rint x * 100) / 100

  def deleteChildrenOf(s: String): Unit = {
    val myNode = dom.document.getElementById(s)
    while (myNode.firstChild != null) {
      myNode.removeChild(myNode.firstChild)
    }
  }

}
