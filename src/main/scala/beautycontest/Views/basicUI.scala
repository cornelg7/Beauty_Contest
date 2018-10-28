package beautycontest.Views

import beautycontest.Controlers.{Game, Player}
import org.scalajs.dom.html
import scalatags.JsDom.all.{`type`, input, placeholder}

import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom.all._



// Used for basic simulation
// Asks for the number of players, for the number of rounds and for the players
// Runs a new simulation with those parameters
@JSExport
class basicUI {

  var p: Float = 0
  var numberOfPlayers = 0
  var numberOfRounds = 0
  var players = List()
  var currentGame: Game = new Game(0, 0, List())

  @JSExport
  def updateOutput(): Unit = {

  }

  @JSExport
  def main(inp: html.Div, outp: html.Div): Unit = {
        // ask for p
    val inputForP = input(
      `type`:="text",
      placeholder:="p"
    ).render

        // ask for numberOfPlayers
    val inputForNumberOfPlayers = input(
      `type`:="number",
      placeholder:="Number of players"
    ).render

        // ask for numberOfRounds
    val inputForNumberOfRounds = input(
      `type`:="number",
      placeholder:="Number of rounds"
    ).render

        // ask for player types
    val inputForPlayers = input(
      `type`:="text",
      placeholder:="Stategies used"
    ).render

        // go when press the button
    val goButton = input(
      `type`:="button",
      onclick:=startSimulation(),
      value:="Start simulation"
    ).render


    @JSExport
    def startSimulation(): Unit = {
      p = inputForP.value.toFloat
      numberOfPlayers = inputForNumberOfPlayers.value.toInt
      numberOfRounds = inputForNumberOfRounds.value.toInt
      players = List()
      println("starting simulation with n = " + numberOfPlayers + ", m = "
        + numberOfRounds + ", players: " + players)
      currentGame = new Game(numberOfPlayers, numberOfRounds, players)
    }


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
