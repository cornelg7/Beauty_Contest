package beautycontest.Views

import beautycontest.Models.inGameItems
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.html.Input
import scalatags.JsDom.all._

import scala.scalajs.js.annotation.JSExport

@JSExport
class inGameSimulation {

  @JSExport
  def main(): Unit = {
    val where = dom.document.getElementsByTagName("BODY")(0)
    val gamecontainer = inGameItems.newDivOfClass("gamecontainer")
    val upcontainer = inGameItems.newDivOfClass("upcontainer")
    val leftcontainer = inGameItems.newDivOfClass("leftcontainer")
    val roundinfo = inGameItems.newDivOfClass("roundinfo")
    val gameboard = inGameItems.newDivOfClass("gameboard")
    val buttons = inGameItems.newDivOfClass("buttons")
    val rightcontainer = inGameItems.newDivOfClass("rightcontainer")
    val gameinfo = inGameItems.newDivOfClass("gameinfo")
    val highscore = inGameItems.newDivOfClass("highscore")
    val roundhistory = inGameItems.newDivOfClass("roundhistory")

    where.appendChild(gamecontainer)
    gamecontainer.appendChild(upcontainer)
    upcontainer.appendChild(leftcontainer)
    leftcontainer.appendChild(roundinfo)
    leftcontainer.appendChild(gameboard)
    leftcontainer.appendChild(buttons)
    upcontainer.appendChild(rightcontainer)
    rightcontainer.appendChild(gameinfo)
    rightcontainer.appendChild(highscore)
    gamecontainer.appendChild(roundhistory)

  }
}
