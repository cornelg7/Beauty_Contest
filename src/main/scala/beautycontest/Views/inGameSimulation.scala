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
    fillScoreBoard()
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

  case class scoreBoardEntry(place: Int, player: Player, roundsWon: Int, rowContainer: Div,
                             rowPlace: Div, rowName: Div, rowScore: Div)
  var scoreBoardEntries: List[scoreBoardEntry] = List()
  var scoreBoardHeader: scoreBoardEntry = scoreBoardEntry(0, null, 0,
    inGameItems.newDivOfClass("highscore-row"),
    inGameItems.newDivOfClass("highscore-row-place"),
    inGameItems.newDivOfClass("highscore-row-name"),
    inGameItems.newDivOfClass("highscore-row-score"))


  def fillScoreBoard(): Unit = {
      // fill header
    scoreBoardHeader.rowPlace.innerHTML = "##"
    scoreBoardHeader.rowName.innerHTML = "Player Name"
    scoreBoardHeader.rowScore.innerHTML = "Score"
    scoreBoardHeader.rowContainer.appendChild(scoreBoardHeader.rowPlace)
    scoreBoardHeader.rowContainer.appendChild(scoreBoardHeader.rowName)
    scoreBoardHeader.rowContainer.appendChild(scoreBoardHeader.rowScore)
    highscore.appendChild(scoreBoardHeader.rowContainer)

      // the rest..
    var i: Int = 1
    // for each player, create a card and save it in playerCards
    currentGame.playerList.foreach(x => {
      val elem = scoreBoardEntry(i, x, 0,
        inGameItems.newDivOfClass("highscore-row"),
        inGameItems.newDivOfClass("highscore-row-place"),
        inGameItems.newDivOfClass("highscore-row-name"),
        inGameItems.newDivOfClass("highscore-row-score")
      )
      highscore.appendChild(elem.rowContainer)
      elem.rowContainer.appendChild(elem.rowPlace)
      elem.rowContainer.appendChild(elem.rowName)
      elem.rowContainer.appendChild(elem.rowScore)
      elem.rowPlace.innerHTML = "#" + i
      elem.rowName.innerHTML = elem.player.uid + ". " + elem.player.name
      elem.rowScore.innerHTML = "" + elem.roundsWon

      scoreBoardEntries = elem :: scoreBoardEntries
      i = i + 1
    })
  }

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

  def updateScoreBoard(): Unit = {
    val infoListMap = currentGame.previousRoundList.head.infoList.map{x => x.player -> (x.choice, x.win) }.toMap
    scoreBoardEntries = scoreBoardEntries.map(x => {
      if (infoListMap(x.player)._2) {  // if player x won
        x.rowScore.innerHTML = "" + (x.roundsWon + 1)
        x.copy(roundsWon = x.roundsWon + 1)
      }
      else
        x
    })
      // remove all children from highscore, sort scoreBoardEntries by roundswon desc, add back to highscore
    while (highscore.firstChild != null) {
      highscore.removeChild(highscore.firstChild)
    }
    highscore.appendChild(scoreBoardHeader.rowContainer)
    scoreBoardEntries = scoreBoardEntries.sortBy(x => -x.roundsWon)
    var i = 1
    scoreBoardEntries.foreach(x => {
      x.rowPlace.innerHTML = "#" + i
      highscore.appendChild(x.rowContainer)
      i = i + 1
    })
  }

  def updateRoundInfo(): Unit = {
    var sumToShow: Int = 0
    var avgToShow, wToShow: Double = 0.toDouble
    try {
      updatePlayerCards()
      updateScoreBoard()
      sumToShow = currentGame.previousRoundList.head.sumNumber
      avgToShow = Helpers.rround2(currentGame.previousRoundList.head.avgNumber)
      wToShow = Helpers.rround2(currentGame.previousRoundList.head.winNumber)
    } catch {case e: Exception => println("catch error in updateRoundInfo " + e)}
    roundinfo.innerHTML = "Round info:  #" + currentGame.currentRoundNumber + ", sum: " +
      sumToShow + ", avg: " + avgToShow + ", w: " + wToShow
  }

  def edbuging(): Unit = {
    //gameboard.appendChild(inGameItems.newDivOfClass("gameboard-item"))
    highscore.appendChild(inGameItems.newDivOfClass("highscore-row"))
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
