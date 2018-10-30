package beautycontest.Views

import beautycontest.Controlers.Strategies.{SConstant, SRandom}
import beautycontest.Controlers.{Game, Player}
import org.scalajs.dom
import org.scalajs.dom.html
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

  @JSExport
  def main(inp: html.Div): Unit = {
        // ask for p
    val inputForP = input(
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

    val outputForGame = div(
      height:="500px",
      width:="600px",
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

    inp.appendChild(
      div(
        div(inputForP),
        div(inputForNumberOfPlayers),
        div(inputForNumberOfRounds),
        div(inputForPlayers),
        div(goButton)
      ).render
    )

  }

}
