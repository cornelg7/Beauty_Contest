package beautycontest.Controlers

import scala.collection.mutable.ListBuffer

// Needs:  p,   a list of Players
// Can return:  winNumber,   a list of (player, choice, win)
class Round(p: Float, playerList: List[Player]) {

  // public win number for this round
  var winNumber:Int = 0
    // did player player with choice choice win this round?
  case class InfoForPlayer(player: Player, choice: Int, win: Boolean)
    // public list of InfoForPlayer
  var infoList:List[InfoForPlayer] = List()
  // the players and their choices
  private var playerAndNumberList:List[(Player, Int)] = List()

  def startRound():Unit = {
    playerAndNumberList = playerList.zip(playerList.map((x:Player) => x.getChoice))
    winNumber = computeWinNumber()
    infoList = computeWinners().map(tupleToCaseClass)
  }

  private def computeWinNumber(): Int = {
    var sum:Integer = 0
    playerAndNumberList.foreach {
      case x@(_, choice) => sum += choice
    }
    (sum.toFloat / playerAndNumberList.length.toFloat * p).toInt
  }

  private def tupleToCaseClass(x:((Player, Int), Boolean)): InfoForPlayer = x match{case x@((p, c), w) => InfoForPlayer(p, c, w)}

  private def computeWinners():List[((Player, Int), Boolean)] = {
    var minDistance = 101 // shortest distance from winning number to each player

    playerAndNumberList.foreach {
      case x@(_, choice) => {
        val playerDist = Math.abs(winNumber - choice)
        minDistance = Math.min(minDistance, playerDist)
      }
    }

    var didPlayerWinListBuffer = new ListBuffer[Boolean]()
    playerAndNumberList.foreach {
      case x@(player, choice) => {
        val playerDist = Math.abs(winNumber - choice)
        val playerWon = if (playerDist == minDistance) true else false
        didPlayerWinListBuffer += playerWon
      }
    }

    playerAndNumberList.zip(didPlayerWinListBuffer.toList)
  }

}
