package beautycontest.Controlers

import scala.scalajs.js.annotation.JSExport

class Game(p: Float, numberOfPlayers: Int, numberOfRounds: Int, playerList: List[Player]) {

    // public for all players:
  var previousRoundList:List[Round] = List[Round]()
  var currentRoundNumber = 0

  case class Score(player: Player, roundsWon: Int)
  var scoreBoard: List[Score] = fillUpScoreBoard()

  prepGame()

  def jumpToEnd(): Unit = {
    //noinspection LoopVariableNotUpdated
    while (currentRoundNumber < numberOfRounds) nextRound()
  }

  def nextRound(): Unit = {
    currentRoundNumber = currentRoundNumber + 1
    val currentRound = new Round(p, playerList)
    currentRound.startRound()
    printWhatHappened(currentRound)
    previousRoundList = currentRound :: previousRoundList
  }

  def printWhatHappened(r: Round): Unit = {
    println("ROUND  " + currentRoundNumber + ": ")
    println("  Win number:  " + r.winNumber + ";")
    println("  Winners:")
    val winners = r.infoList.filter(_.win) // filter all the winners
    winners.foreach(x => println("    Player " + x.player.uid
      + " with name " + x.player.name + " and choice " + x.choice))
  }

  def prepGame(): Unit = {
    addUIDs()
  }

  def addUIDs(): Unit = {
    var i = 1
    playerList.foreach(x => {x.uid = i; i = i + 1})
  }

  def fillUpScoreBoard(): List[Score] = playerList.map(p => Score(p, 0))

}
