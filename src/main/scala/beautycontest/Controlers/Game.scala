package beautycontest.Controlers

import beautycontest.Helpers
import beautycontest.Views.{basicUI, inGameSimulation}
import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

class Game(val p: Float, val numberOfPlayers: Int, val numberOfRounds: Int, val playerList: List[Player], val teamList: List[List[Player]] = List()) {

    // public for all players:
  var previousRoundList:List[Round] = List[Round]()
  var currentRoundNumber = 0
  var inGameUI = new inGameSimulation(this)

  case class Score(player: Player, roundsWon: Int)
  var scoreBoard: List[Score] = fillUpScoreBoard()

    // this is called by basicUI
 // prepGame()

  def jumpToEnd(): Unit = {
    //noinspection LoopVariableNotUpdated
    while (currentRoundNumber < numberOfRounds) nextRoundWithoutPrint()
  }

  def nextRoundWithoutPrint(): Unit = {
    if (currentRoundNumber >= numberOfRounds)
      return
    currentRoundNumber = currentRoundNumber + 1
    val currentRound = new Round(p, playerList)
    currentRound.startRound()
    //printWhatHappened(currentRound)
    updateScoreBoard(currentRound)
    previousRoundList = currentRound :: previousRoundList
    inGameUI.updateRoundInfo()
  }

  def nextRound(): Unit = {
    if (currentRoundNumber >= numberOfRounds)
      return
    currentRoundNumber = currentRoundNumber + 1
    val currentRound = new Round(p, playerList)
    currentRound.startRound()
    //printWhatHappened(currentRound)
    updateScoreBoard(currentRound)
    previousRoundList = currentRound :: previousRoundList
    inGameUI.updateRoundInfo()
  }

  def updateScoreBoard(r: Round): Unit = {
    val winners = r.infoList.filter(_.win)
      // for all players x, if x is a winner, increment its score
    scoreBoard = scoreBoard.map(x => if (winners.exists(y => y.player.equals(x.player))) x.copy(roundsWon = x.roundsWon + 1) else x);
  }

  def printScoreBoard(): Unit = {
    val whereToOutput = dom.document.getElementById("output")
    var s:String = "<br>"
    scoreBoard.sortBy(x => x.roundsWon) // @TODO: fix sorting
    scoreBoard.foreach(x => s = s + "&emsp;Player " + x.player.name +
      " with id " + x.player.uid + " won " + x.roundsWon + " rounds.<br>")
    s = s + "<br>"
    whereToOutput.innerHTML = s + whereToOutput.innerHTML
  }

  def printWhatHappened(r: Round): Unit = {
    val whereToOutput = dom.document.getElementById("output")
    var s:String = "<br>ROUND  " + currentRoundNumber + ": <br>" +
      "&emsp;Win number:  " + r.winNumber + "<br>" +
      "&emsp;Winners: <br>"
    val winners = r.infoList.filter(_.win)
    winners.foreach(x => s = s + "&emsp;&emsp;Player " + x.player.uid +
      " with name " + x.player.name + " and choice " + x.choice + "<br><br>"
    )
    whereToOutput.innerHTML = s + whereToOutput.innerHTML

      // after last round, print scoreboard
//    if (currentRoundNumber == numberOfRounds)
//      printScoreBoard()
  }

  def broadcastThisToPlayers(): Unit = {
    playerList.foreach(x => x.myGame = this)
  }

  def prepGame(): Unit = {

    addUIDs()

    broadcastThisToPlayers()

    Helpers.deleteChildrenOf("input")

    inGameUI = new inGameSimulation(this)
    inGameUI.prepare()
  }

  def prepGameNoUI(): Unit = {

    addUIDs()

    broadcastThisToPlayers()

  }

  def addUIDs(): Unit = {
    var i = 1
    playerList.foreach(x => {x.uid = i; i = i + 1})
  }

  def fillUpScoreBoard(): List[Score] = playerList.map(p => Score(p, 0))

  def goBack(): Unit = {
    Helpers.deleteChildrenOf("game")
    basicUI.main(dom.document.getElementById("input").asInstanceOf[html.Div])
  }
}
