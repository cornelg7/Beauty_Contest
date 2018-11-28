package beautycontest.Controlers

import com.sun.xml.internal.bind.v2.model.core.MaybeElement

import scala.collection.mutable.ListBuffer

// Needs:  p,   a list of Players
// optional: team list; assertion: each player has maximum one team
// Can return:  winNumber,   a list of (player, choice, win)
class Round(p: Float, playerList: List[Player], teamList: List[List[Player]] = List()) {

  // public win number for this round
  var winNumber:Float = 0
  var sumNumber:Int = 0
  var avgNumber:Float = 0
    // did player player with choice choice win this round?
  case class InfoForPlayer(player: Player, choice: Int, win: Boolean)
    // public list of InfoForPlayer
  var infoList:List[InfoForPlayer] = List()
  // the players and their choices
  private var playerAndNumberList:List[(Player, Int)] = List()
  // did team # win this round?
  var teamWinListOfBoolean: ListBuffer[Boolean] = ListBuffer()


  def startRound():Unit = {
    teamWinListOfBoolean = resetTeamWins()
    playerAndNumberList = playerList.zip(playerList.map((x:Player) => x.getChoice))
    winNumber = computeWinNumber()
    infoList = computeWinners().map(tupleToCaseClass)
  }

  private def computeWinNumber(): Float = {
    sumNumber = 0
    playerAndNumberList.foreach {
      case x@(_, choice) => sumNumber += choice
    }
    avgNumber = sumNumber.toFloat / playerAndNumberList.length.toFloat
    avgNumber * p
  }

  private def tupleToCaseClass(x:((Player, Int), Boolean)): InfoForPlayer = x match{case y@((pl, c), w) => InfoForPlayer(pl, c, w)}

  private def resetTeamWins(): ListBuffer[Boolean] = {
    var toR = ListBuffer[Boolean]()
    teamList.foreach(x => { toR += false })
    toR
  }

  private def setWinToTeamOf(p: Player): Unit = {
    var i = 0
    teamList.foreach(xs => {
      var isIn = false
      xs.foreach (pp => {
        if (p.equals(pp)) isIn = true
      })
      if (isIn) {
        teamWinListOfBoolean(i) = true
      }
      i += 1
    })
  }

  private def didTeamOfWin(p: Player): Boolean = {
    var i = 0
    teamList.foreach(xs => {
      var isIn = false
      xs.foreach (pp => {
        if (p.equals(pp)) isIn = true
      })
      if (isIn) {
        return teamWinListOfBoolean(i)
      }
      i += 1
    })
    false
  }


  private def computeWinners():List[((Player, Int), Boolean)] = {
    var minDistance: Float = 101.toFloat // shortest distance from winning number to each player

    playerAndNumberList.foreach {
      case x@(_, choice) => {
        val playerDist = scala.math.abs(winNumber - choice)
        minDistance = scala.math.min(minDistance, playerDist)
      }
    }

    // did player win?
    var didPlayerWinListBuffer0 = new ListBuffer[Boolean]()
    playerAndNumberList.foreach {
      case x@(player, choice) => {
        val playerDist = scala.math.abs(winNumber - choice)
        val playerWon = if (playerDist == minDistance) {
          // set teamwin
          setWinToTeamOf(player)
          true
        }
        else false
        didPlayerWinListBuffer0 += playerWon
      }
    }

    // did team win?
    var didPlayerWinListBuffer1 = new ListBuffer[Boolean]()
    var i: Int = 0
    playerAndNumberList.foreach {
      case x@(player, choice) => {
        val playerDist = scala.math.abs(winNumber - choice)
        val teamWon = didTeamOfWin(player)
        didPlayerWinListBuffer1 += (teamWon || didPlayerWinListBuffer0(i))
        i += 1
      }
    }

    playerAndNumberList.zip(didPlayerWinListBuffer1.toList)
  }

}
