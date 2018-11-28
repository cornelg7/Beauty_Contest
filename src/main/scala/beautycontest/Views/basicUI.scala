package beautycontest.Views

import java.io.InputStream

import beautycontest.Controlers.Strategies.{SConstant, SMastermind, SPawn, SRandom}
import beautycontest.Controlers.{Game, Player}
import beautycontest.Helpers
import org.scalajs.dom
import org.scalajs.dom.{UIEvent, html}
import org.scalajs.dom.html.Input
import scalatags.JsDom.all.{`type`, input, placeholder}

import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom.all._



// Used for basic simulation
// Asks for the number of players, for the number of rounds and for the players
// Runs a new simulation with those parameters
@JSExport
object basicUI {

  var p: Float = 0
  var numberOfPlayers = 0
  var numberOfRounds = 0
  var players:List[Player] = List[Player]()
  var currentGame: Game = new Game(0, 0, 0, List())

  def updateOutput(): Unit = {

  }

    // parsing players of type SConstant and SRandom @TODO: make better parsing
  def parsePlayers(playersString: String): List[Player] = {
//    println("Trying to parse " + playersString)
    var toR: List[Player] = List()
    val playerStringList = playersString.split(", ").toList
//    println("step1: " + playerStringList)
    playerStringList.foreach(p => {
      val onePlayerString = p.split("\\(").toList
//      println("step2: " + onePlayerString)
      val playerNameString = onePlayerString.head
      var arg = 101
      if (onePlayerString.tail != Nil) arg = onePlayerString.tail.head.replace(")", "").toInt
      if (playerNameString.equals("SConstant"))
        toR = new SConstant(arg) :: toR
      else if (playerNameString.equals("SRandom"))
        toR = new SRandom(arg) :: toR
    })
    toR.reverse
  }

  def writeTemplateForJSON(): Unit = {
    var s = ""
    s = "{\"strategy\": \"SConstant\",\"r\": 1,\"p\": 0.8,\"games\":["
    for (i <- 2 to 100) {
      s = s + "{\"players\": [0"
      for (j <- 1 to i) s = s + ", " + 1
      s = s + "]}"
      if (i != 100)
        s = s + ", "
    }
    s = s + "]}"
    println(s)
  }

  def startConstWars(strats: String, outp: html.Div): Unit = {
   // val ci = strats.split(" ").toList.head.toInt
   // val cj = strats.split(" ").toList.tail.head.toInt
    var s = ""
    var ss = ""
    var cgame: Game = null
    case class gameResult(vs: Int, hm: Int, w: Int)
    var games = List[gameResult]()
    case class Ccell(p: Double)
    case class Rrow(vs: Int, body: List[Ccell])
    case class Ttable(hd: List[String], body: List[Rrow])
    var maxNumberOfPlayers = 100

    var tablehead = List[String]()
    for (i <- 0 to 100)
      tablehead = (" "+i+" ") :: tablehead
    tablehead = tablehead.reverse
    tablehead = "v ci, cj >" :: tablehead

    var tablebody = List[Rrow]()

    for (ci <- 0 to 0) {
      val protagonist = new SConstant(ci)
      protagonist.uid = 1
      var rowbody = List[Ccell]()
      for (cj <- 0 to maxNumberOfPlayers) {
        games = List[gameResult]()
        if (ci != cj) {
          for (i <- 2 to 10) {
            val nn = i + 1
            val rr = 1
            val pp: Float = 0.8.toFloat
            var pplayers = protagonist :: List[Player]()
            for (j <- 1 to i) pplayers = new SConstant(cj) :: pplayers
            pplayers = pplayers.reverse
            cgame = new Game(pp, nn, rr, pplayers)
            cgame.prepGameNoUI()
            cgame.jumpToEnd()
            var win = 0
            win = cgame.scoreBoard.filter(x => x.player.name == protagonist.name).head.roundsWon
            //cgame.scoreBoard.filter(x => x.roundsWon == 1).foreach(x => if (x.player.equals(protagonist)) win = 1)
            games = gameResult(cj, i, win) :: games
          }
          games = games.reverse
          var ws = 0
          ss = "i am " + ci + "<ul>"
          games.foreach(x => {
            ss = ss + "<li>" + "vs " + x.hm + " of " + x.vs + ": " + x.w + "</li>"
            ws = ws + x.w
          })
          ss = ss + "</ul>"
          ss = ws + ss
          var fr = Helpers.rround2(ws.toFloat/(maxNumberOfPlayers-1))
          rowbody = Ccell(fr) :: rowbody
        }
        else {
          rowbody = Ccell((-1).toFloat) :: rowbody
        }
      }
      rowbody = rowbody.reverse
      tablebody = Rrow(ci, rowbody) :: tablebody
    }
    tablebody = tablebody.reverse
    var results = Ttable(tablehead, tablebody)
    s = "<table>"
    results.hd.foreach(x => x)

    val tt = table(style:="border: 1px solid black; border-collapse: collapse")(
      tr(
        for (x <- results.hd) yield th(style:="border: 1px solid black;")(x)
      ),
      for (x <- results.body) yield tr(
        th(style:="border: 1px solid black;")(x.vs),
        for (y <- x.body) yield td(style:="border: 1px solid black;")(
          y.p
        )
      )
    ).render

    s = s + "</table>"
    //outp.innerHTML = ss
    outp.appendChild(tt)

  }


  def startRandWars(strats: String, outp: html.Div): Unit = {
    // val ci = strats.split(" ").toList.head.toInt
    // val cj = strats.split(" ").toList.tail.head.toInt
    var s = ""
    var ss = ""
    var cgame: Game = null
    case class gameResult(vs: Int, hm: Int, w: Double)
    var games = List[gameResult]()
    case class Ccell(p: Double)
    case class Rrow(vs: Int, body: List[Ccell])
    case class Ttable(hd: List[String], body: List[Rrow])
    var maxNumberOfPlayers = 10

    var tablehead = List[String]()
    for (i <- 0 to 100)
      tablehead = (" "+i+" ") :: tablehead
    tablehead = tablehead.reverse
    tablehead = "v ci, cj >" :: tablehead

    var tablebody = List[Rrow]()

    for (ci <- 0 to 1) {
      ss = "i am " + ci + "<ul>"
      val protagonist = new SRandom(ci)
      protagonist.uid = 1
      var rowbody = List[Ccell]()
      for (cj <- 0 to 100) {
        games = List[gameResult]()
        if (ci != cj) {
          for (i <- 2 to maxNumberOfPlayers) {
            val nn = i + 1
            val rr = 100
            val pp: Float = 0.8.toFloat
            var pplayers = protagonist :: List[Player]()
            for (j <- 1 to i) pplayers = new SRandom(cj) :: pplayers
            pplayers = pplayers.reverse
            cgame = new Game(pp, nn, rr, pplayers)
            cgame.prepGameNoUI()
            cgame.jumpToEnd()
            var win = 0.toDouble
            win = cgame.scoreBoard.filter(x => x.player.name == protagonist.name).head.roundsWon
            //cgame.scoreBoard.filter(x => x.roundsWon == 1).foreach(x => if (x.player.equals(protagonist)) win = 1)
            games = gameResult(cj, i, win/rr.toDouble) :: games
          }
          games = games.reverse
          var ws = 0.toDouble
          games.foreach(x => {
            ss = ss + "<li>" + "vs " + x.hm + " of " + x.vs + ": " + x.w + "</li>"
            ws = ws + x.w
          })
          ss = ws + ss
          var fr = Helpers.rround2(ws.toFloat/(maxNumberOfPlayers-1))
          rowbody = Ccell(fr) :: rowbody
        }
        else {
          rowbody = Ccell((-1).toFloat) :: rowbody
        }
      }
      ss = ss + "</ul>"
      rowbody = rowbody.reverse
      tablebody = Rrow(ci, rowbody) :: tablebody
    }
    tablebody = tablebody.reverse
    var results = Ttable(tablehead, tablebody)
    s = "<table>"
    results.hd.foreach(x => x)

    val tt = table(style:="border: 1px solid black; border-collapse: collapse")(
      tr(
        for (x <- results.hd) yield th(style:="border: 1px solid black;")(x)
      ),
      for (x <- results.body) yield tr(
        th(style:="border: 1px solid black;")(x.vs),
        for (y <- x.body) yield td(style:="border: 1px solid black;")(
          y.p
        )
      )
    ).render

    s = s + "</table>"
    //outp.innerHTML = ss
    outp.appendChild(tt)

  }


  def startConstVsRandWars(strats: String, outp: html.Div): Unit = {
    // val ci = strats.split(" ").toList.head.toInt
    // val cj = strats.split(" ").toList.tail.head.toInt
    var s = ""
    var ss = ""
    var cgame: Game = null
    case class gameResult(vs: Int, hm: Int, w: Double)
    var games = List[gameResult]()
    case class Ccell(p: Double)
    case class Rrow(vs: Int, body: List[Ccell])
    case class Ttable(hd: List[String], body: List[Rrow])
    var maxNumberOfPlayers = 10

    var tablehead = List[String]()
    for (i <- 0 to 100)
      tablehead = (" "+i+" ") :: tablehead
    tablehead = tablehead.reverse
    tablehead = "v ci, cj >" :: tablehead

    var tablebody = List[Rrow]()

    for (ci <- 0 to 100) {
      ss = "i am " + ci + "<ul>"
      val protagonist = new SConstant(ci)
      protagonist.uid = 1
      var rowbody = List[Ccell]()
      for (cj <- 0 to 100) {
        games = List[gameResult]()
        if (true) {  // constant i can play against rand i
          for (i <- 2 to maxNumberOfPlayers) {
            val nn = i + 1
            val rr = 1000
            val pp: Float = 0.8.toFloat
            var pplayers = protagonist :: List[Player]()
            for (j <- 1 to i) pplayers = new SRandom(cj) :: pplayers
            pplayers = pplayers.reverse
            cgame = new Game(pp, nn, rr, pplayers)
            cgame.prepGameNoUI()
            cgame.jumpToEnd()
            var win = 0.toDouble
            win = cgame.scoreBoard.filter(x => x.player.name == protagonist.name).head.roundsWon
            //cgame.scoreBoard.filter(x => x.roundsWon == 1).foreach(x => if (x.player.equals(protagonist)) win = 1)
            games = gameResult(cj, i, win/rr.toDouble) :: games
          }
          games = games.reverse
          var ws = 0.toDouble
          games.foreach(x => {
            ss = ss + "<li>" + "vs " + x.hm + " of " + x.vs + ": " + x.w + "</li>"
            ws = ws + x.w
          })
          ss = ws + ss
          var fr = Helpers.rround2(ws.toFloat/(maxNumberOfPlayers-1))
          rowbody = Ccell(fr) :: rowbody
        }
        else {
          rowbody = Ccell((-1).toFloat) :: rowbody
        }
      }
      ss = ss + "</ul>"
      rowbody = rowbody.reverse
      tablebody = Rrow(ci, rowbody) :: tablebody
    }
    tablebody = tablebody.reverse
    var results = Ttable(tablehead, tablebody)
    s = "<table>"
    results.hd.foreach(x => x)

    val tt = table(style:="border: 1px solid black; border-collapse: collapse")(
      tr(
        for (x <- results.hd) yield th(style:="border: 1px solid black;")(x)
      ),
      for (x <- results.body) yield tr(
        th(style:="border: 1px solid black;")(x.vs),
        for (y <- x.body) yield td(style:="border: 1px solid black;")(
          y.p
        )
      )
    ).render

    s = s + "</table>"
    //outp.innerHTML = ss
    outp.appendChild(tt)

  }


  def startMastermindVsRandWars(strats: String, outp: html.Div): Unit = {
    // val ci = strats.split(" ").toList.head.toInt
    // val cj = strats.split(" ").toList.tail.head.toInt
    var s = ""
    var ss = ""
    var cgame: Game = null
    case class gameResult(vs: Int, hm: Int, w: Double)
    var games = List[gameResult]()
    case class Ccell(p: Double)
    case class Rrow(vs: Int, body: List[Ccell])
    case class Ttable(hd: List[String], body: List[Rrow])
    var maxNumberOfPlayers = 10

    var tablehead = List[String]()
    for (i <- 1 to maxNumberOfPlayers)
      tablehead = (" "+i+" ") :: tablehead
    tablehead = tablehead.reverse
    tablehead = "v cj, # >" :: tablehead

    var tablebody = List[Rrow]()

   // for (ci <- 0 to 100) {
     // ss = "i am " + ci + "<ul>"
      val protagonist = new SMastermind
      protagonist.uid = 1
      val protagonistPawn = new SPawn(protagonist)
      protagonistPawn.uid = 2
      val teams:List[List[Player]] = List(List(protagonist, protagonistPawn))

      for (cj <- 0 to 5) {
        var rowbody = List[Ccell]()
        games = List[gameResult]()
       // if (true) {  // constant i can play against rand i
          for (i <- 1 to maxNumberOfPlayers) {
            val nn = i + 2
            val rr = 1000
            val pp: Float = 0.8.toFloat
            var pplayers = protagonistPawn :: protagonist :: List[Player]()
            for (j <- 1 to i) pplayers = new SRandom(cj) :: pplayers
            pplayers = pplayers.reverse
            cgame = new Game(pp, nn, rr, pplayers, teams)
            cgame.prepGameNoUI()
            cgame.jumpToEnd()
            var win = 0.toDouble
            win = cgame.scoreBoard.filter(x => x.player.name == protagonist.name).head.roundsWon
            //cgame.scoreBoard.filter(x => x.roundsWon == 1).foreach(x => if (x.player.equals(protagonist)) win = 1)
            games = gameResult(cj, i, win/rr.toDouble) :: games
            rowbody = Ccell(win/rr.toDouble) :: rowbody
          }
          games = games.reverse
          var ws = 0.toDouble
          games.foreach(x => {
            ss = ss + "<li>" + "vs " + x.hm + " of " + x.vs + ": " + x.w + "</li>"
            ws = ws + x.w
          })
          ss = ws + ss
          var fr = Helpers.rround2(ws.toFloat/(maxNumberOfPlayers-1))
          rowbody = rowbody.reverse
          tablebody = Rrow(cj, rowbody) :: tablebody
       // }
      //  else {
      //    rowbody = Ccell((-1).toFloat) :: rowbody
       // }
      }
      ss = ss + "</ul>"
   // }
    tablebody = tablebody.reverse
    var results = Ttable(tablehead, tablebody)
    s = "<table>"
    results.hd.foreach(x => x)

    val tt = table(style:="border: 1px solid black; border-collapse: collapse")(
      tr(
        for (x <- results.hd) yield th(style:="border: 1px solid black;")(x)
      ),
      for (x <- results.body) yield tr(
        th(style:="border: 1px solid black;")(x.vs),
        for (y <- x.body) yield td(style:="border: 1px solid black;")(
          y.p
        )
      )
    ).render

    s = s + "</table>"
    //outp.innerHTML = ss
    outp.appendChild(tt)

  }

  @JSExport
  def main(inp: html.Div): Unit = {
        // ask for p
    val inputForP: Input = input(
      `type`:="text",
      value:=0.8,
      placeholder:="p"
    ).render

        // ask for numberOfPlayers
    val inputForNumberOfPlayers = input(
      `type`:="number",
      value:=3,
      placeholder:="Number of players"
    ).render

        // ask for numberOfRounds
    val inputForNumberOfRounds = input(
      `type`:="number",
      value:=10,
      placeholder:="Number of rounds"
    ).render

        // ask for player types
    val inputForPlayers = input(
      `type`:="text",
      value:="SRandom, SRandom(80), SConstant(50)",
      placeholder:="Stategies used"
    ).render

    // go when press the button
    val goButton = input(
      `type`:="button",
      value:="Start simulation"
    ).render

    // ask for player types
    val inputForConstWars = input(
      `type`:="text",
      value:="0 1",
      placeholder:="ci cj"
    ).render

    // debuging
    val testButtonConstWar = input(
      `type`:="button",
      value:="mult const"
    ).render

    // debuging
    val testButtonRandWar = input(
      `type`:="button",
      value:="mult rand"
    ).render

    // debuging
    val testButtonConstRandWar = input(
      `type`:="button",
      value:="const vs rand"
    ).render

    // debuging
    val testButtonMastermindRandWar = input(
      `type`:="button",
      value:="mastermind vs rand"
    ).render

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

    // debugging
    val outputForGame = div(
      height:="500px",
      width:="800px",
      border:="1px solid",
      id:="output",
      overflow:="auto"
    ).render

    def startSimulation(): Unit = {
      p = inputForP.value.toFloat
      numberOfPlayers = inputForNumberOfPlayers.value.toInt
      numberOfRounds = inputForNumberOfRounds.value.toInt
      players = parsePlayers(inputForPlayers.value)
      currentGame = new Game(p, numberOfPlayers, numberOfRounds, players)

        // add onclick events
      nextRoundButton.onclick = (e: dom.MouseEvent) => currentGame.nextRound()
      jumpToEndButton.onclick = (e: dom.MouseEvent) => currentGame.jumpToEnd()

        // add nextRound button
      inp.appendChild(
        div(
          div(nextRoundButton),
          div(jumpToEndButton),
          outputForGame
        ).render
      )

      currentGame.prepGame()
    }

    goButton.onclick = (e: dom.MouseEvent) => startSimulation()
    testButtonConstWar.onclick = (e: dom.MouseEvent) => startConstWars(inputForConstWars.value, outputForGame)
    testButtonRandWar.onclick = (e: dom.MouseEvent) => startRandWars(inputForConstWars.value, outputForGame)
    testButtonConstRandWar.onclick = (e: dom.MouseEvent) => startConstVsRandWars(inputForConstWars.value, outputForGame)
    testButtonMastermindRandWar.onclick = (e: dom.MouseEvent) => startMastermindVsRandWars(inputForConstWars.value, outputForGame)

    inp.appendChild(
      div(
        div(inputForP),
        div(inputForNumberOfPlayers),
        div(inputForNumberOfRounds),
        div(inputForPlayers),
        div(goButton),
        div(inputForConstWars),
        div(testButtonConstWar),
        div(testButtonRandWar),
        div(testButtonConstRandWar),
        div(testButtonMastermindRandWar),
        div(outputForGame)
      ).render
    )

  }

}
