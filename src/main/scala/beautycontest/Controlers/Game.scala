package beautycontest.Controlers

import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

class Game(p: Float, numberOfPlayers: Int, numberOfRounds: Int, playerList: List[Player]) {

    // public for all players:
  var previousRoundList:List[Round] = List[Round]()
  var currentRoundNumber = 0

  case class Score(player: Player, roundsWon: Int)
  var scoreBoard: List[Score] = fillUpScoreBoard()

    // this is called by basicUI
 // prepGame()

  def jumpToEnd(): Unit = {
    //noinspection LoopVariableNotUpdated
    while (currentRoundNumber < numberOfRounds) nextRound()
  }

  def nextRound(): Unit = {
    if (currentRoundNumber >= numberOfRounds)
      return
    currentRoundNumber = currentRoundNumber + 1
    val currentRound = new Round(p, playerList)
    currentRound.startRound()
    printWhatHappened(currentRound)
    updateScoreBoard(currentRound)
    previousRoundList = currentRound :: previousRoundList
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
    if (currentRoundNumber == numberOfRounds)
      printScoreBoard()
  }

  def prepGame(): Unit = {
    val whereToOutput = dom.document.getElementById("output")
    val playerNamesListString = playerList.reverse.foldRight("")((pl, st) => st + pl.name + " ")
    val s:String = "Starting simulation with " + numberOfPlayers +
      " players and " + numberOfRounds + " rounds<br>List of players:  " + playerNamesListString
    whereToOutput.innerHTML = s
    addUIDs()
  }

  def addUIDs(): Unit = {
    var i = 1
    playerList.foreach(x => {x.uid = i; i = i + 1})
  }

  def fillUpScoreBoard(): List[Score] = playerList.map(p => Score(p, 0))

}
