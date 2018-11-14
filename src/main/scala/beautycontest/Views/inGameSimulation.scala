package beautycontest.Views

import beautycontest.Controlers.{Game, Player}
import beautycontest.Helpers
import beautycontest.Models.inGameItems
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.html.{Div, Input}
import org.scalajs.dom.raw.Node
import scalatags.JsDom.all._

import scala.scalajs.js.annotation.JSExport

@JSExport
class inGameSimulation(currentGame: Game) {

  def prepare(): Unit = {
    addInGameTemplate()
    updateGameInfo()
   // updateRoundInfo()
    addButtons()
    fillGameBoard()
  }

  val where: Node = dom.document.getElementById("game")
  val gamecontainer: Div = inGameItems.newDivOfClass("gamecontainer")
  val upcontainer: Div = inGameItems.newDivOfClass("upcontainer")
  val leftcontainer: Div = inGameItems.newDivOfClass("leftcontainer")
  val roundinfo: Div = inGameItems.newDivOfClass("roundinfo")
  val gameboard: Div = inGameItems.newDivOfClass("gameboard")
  val buttons: Div = inGameItems.newDivOfClass("buttons")
  val rightcontainer: Div = inGameItems.newDivOfClass("rightcontainer")
  val gameinfo: Div = inGameItems.newDivOfClass("gameinfo")
  val highscore: Div = inGameItems.newDivOfClass("highscore")
  val roundhistory: Div = inGameItems.newDivOfClass("roundhistory")

  case class playerAndCard(player: Player, cardContainer: Div, cardHeader: Div, cardBody: Div)
  var playerCards: List[playerAndCard] = List()


  def fillGameBoard(): Unit = {
      // for each player, create a card and save it in playerCards
    currentGame.playerList.foreach(x => {
      val elem = playerAndCard(x,
        inGameItems.newDivOfClass("gameboard-item"),
        inGameItems.newDivOfClass("gameboard-item-header"),
        inGameItems.newDivOfClass("gameboard-item-body")
      )
      gameboard.appendChild(elem.cardContainer)
      elem.cardContainer.appendChild(elem.cardHeader)
      elem.cardContainer.appendChild(elem.cardBody)
      elem.cardHeader.innerHTML = elem.player.uid + ". " + elem.player.name
      elem.cardBody.innerHTML = elem.player.uid + ""
      playerCards = elem :: playerCards
    })
  }

  def updateGameInfo(): Unit = {
    val pToShow = Helpers.rround2(currentGame.p)
    gameinfo.innerHTML = "Game info:  " + currentGame.numberOfPlayers + " players," +
      currentGame.numberOfRounds + " rounds, p = " + pToShow
  }

  def updatePlayerCards(): Unit = {
    val infoListMap = currentGame.previousRoundList.head.infoList.map{x => x.player -> (x.choice, x.win) }.toMap
    playerCards.foreach(x => {
      x.cardBody.innerHTML = "" + infoListMap(x.player)._1 // player x's choice
      x.cardContainer.style.background = "purple" // reset the green
      if (infoListMap(x.player)._2) {  // if player x won
        x.cardContainer.style.background = "lime"
      }
    })
  }

  def updateRoundInfo(): Unit = {
    var sumToShow: Int = 0
    var avgToShow, wToShow: Double = 0.toDouble
    try {
      updatePlayerCards()
      sumToShow = currentGame.previousRoundList.head.sumNumber
      avgToShow = Helpers.rround2(currentGame.previousRoundList.head.avgNumber)
      wToShow = Helpers.rround2(currentGame.previousRoundList.head.winNumber)
    } catch {case e: Exception => println("catch error in updateRoundInfo " + e)}
    roundinfo.innerHTML = "Round info:  #" + currentGame.currentRoundNumber + ", sum: " +
      sumToShow + ", avg: " + avgToShow + ", w: " + wToShow
  }

  def edbuging(): Unit = {
    gameboard.appendChild(inGameItems.newDivOfClass("gameboard-item"))
  }

  def addButtons(): Unit = {
    // go to next round
    val nextRoundButton = input(
      `type`:="button",
      value:="Next round"
    ).render

    // jump to end of the game
    val jumpToEndButton = input(
      `type`:="button",
      value:="Jump to end"
    ).render

    // terminate this simulation
    val backButton = input(
      `type`:="button",
      value:="End"
    ).render

    // debug button
    val debugButton = input(
      `type`:="button",
      value:="......."
    ).render

    // add onclick events
    nextRoundButton.onclick = (e: dom.MouseEvent) => currentGame.nextRound()
    jumpToEndButton.onclick = (e: dom.MouseEvent) => currentGame.jumpToEnd()
    backButton.onclick = (e: dom.MouseEvent) => currentGame.goBack()
    debugButton.onclick = (e: dom.MouseEvent) => edbuging()

    buttons.appendChild(nextRoundButton)
    buttons.appendChild(jumpToEndButton)
    buttons.appendChild(backButton)
    buttons.appendChild(debugButton)

  }

  @JSExport
  def addInGameTemplate(): Unit = {
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
