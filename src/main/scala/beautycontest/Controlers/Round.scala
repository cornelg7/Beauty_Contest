package beautycontest.Controlers

import scala.collection.mutable.ListBuffer

class Round(p: Float, playerList: List[Player]) {

  var winNumber:Integer = 0
  var winnerList:List[((Player, Int), Boolean)] = List()

  def run():Unit = {
    winNumber = computeWinNumber()
    winnerList = computeWinners()
  }

    // the players and their choices
  val playerAndNumberList:List[(Player, Int)] = playerList.zip(playerList.map((x:Player) => x.getChoice))

  private def computeWinNumber(): Int = {
    var sum:Integer = 0
    playerAndNumberList.foreach {
      case x@(_, choice) => sum += choice
    }
    (sum.toFloat / playerAndNumberList.length.toFloat * p).toInt
  }

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
