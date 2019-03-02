package beautycontest

import beautycontest._
import beautycontest.Controlers._
import beautycontest.Controlers.Strategies._
import java.io.{BufferedWriter, File, FileWriter}


object Main{
  case class gameResult(vsWho: Int, vsHowMany: Int, winPercentage: Float)
  case class Ccell(p: Double)
  case class Rrow(vs: Int, body: List[Ccell])
  case class Ttable(hd: List[String], body: List[Rrow])
  
  def printTable(results: Ttable, outFile: String): Unit = {
    
      var s = "<table style=\" border: 1px solid black; border-collapse: collapse\">\n"
      
        /// Table head
      s = s + "<tr>"
      for (x <- results.hd) {
        s = s + "<th style:=\"border: 1px solid black;\">" + x + "</th>"
      }
      s = s + "</tr>\n"
      
        /// Table body
      for (x <- results.body) {
        s = s + "<tr>"
        s = s + "<th style:=\"border: 1px solid black;\">" + x.vs + "</th>"
        for (y <- x.body) {
          s = s + "<td style:=\"border: 1px solid black;\">" + y.p + "</td>\n"
        }
        s = s + "</tr>\n"
      }
      
      s = s + "</table>"
      
      val wr = new BufferedWriter(new FileWriter(outFile))
      wr.write(s)
      wr.flush()
      wr.close()
  }
  
  def startConstWars(outFile: String, groupWeights: (Int, Int) => Float, familyWeights: (Int, Int) => Float): Unit = {
      var cgame: Game = null
      var games = List[gameResult]()
      var maxNumberOfEnemyPlayers = 10
      
      
        /// Table head (first row)
      var tablehead = List[String]()
      for (i <- 0 to 100)
        tablehead = (" "+i+" ") :: tablehead
      tablehead = "Avg" :: tablehead
      tablehead = tablehead.reverse
      tablehead = "v ci, cj >" :: tablehead

      
      var tablebody = List[Rrow]()
      
        /// Here are the actual Games run.
      for (ci <- 0 to 100) {
        println("me work on " + ci + " now..")
        
          /// First set up the protagonist
        val protagonist = new SConstant(ci)
       
          /// Then set up the enemies
        var rowbody = List[Ccell]()
        var overallWeightedAverageOnRow: Float = 0
        for (cj <- 0 to 100) {
          games = List[gameResult]()
          
            /// Run 'one vs many' games for each i
          for (i <- 2 to maxNumberOfEnemyPlayers) {
            val nn = i + 1
            val rr = 1
            val pp: Float = 0.8.toFloat
            var pplayers = protagonist :: List[Player]()
            for (j <- 1 to i) pplayers = new SConstant(cj) :: pplayers
            pplayers = pplayers.reverse
            cgame = new Game(pp, nn, rr, pplayers)
            
            var roundsWonByProtagonist = 0
            roundsWonByProtagonist = cgame.scoreBoard.filter(x => x.player.equals(protagonist)).head.roundsWon
            var winPercentage = roundsWonByProtagonist.toFloat/rr
            
              /// Save in 'games' vsWho, vsHowMany and the winPercentage
            games = gameResult(cj, i, winPercentage) :: games
//   rowbody = Ccell(Helpers.rround2(winPercentage)) :: rowbody
          }
          
            /// After the 'one vs many' games are done, use 'games' to write in a Rrow;  use the GroupWeights here, eg. value more winning against 2 players than against 10.
          games = games.reverse
          var weightedAverage: Float = 0
          games.foreach(x => {
            weightedAverage = weightedAverage + x.winPercentage * groupWeights(x.vsHowMany, games.length)
          })
          overallWeightedAverageOnRow = overallWeightedAverageOnRow + weightedAverage * familyWeights(cj, 101)
            /// Add the weightedAverage to the cell
          rowbody = Ccell(Helpers.rround2(weightedAverage)) :: rowbody
        }
          /// Add the weighted average of all 'weightedAverage's
        rowbody = Ccell(Helpers.rround2(overallWeightedAverageOnRow)) :: rowbody
        
        rowbody = rowbody.reverse
        tablebody = Rrow(ci, rowbody) :: tablebody
      }
      tablebody = tablebody.reverse
      
      var results = Ttable(tablehead, tablebody)
      printTable(results, outFile)
  }


  def main(args: Array[String]): Unit = {
   // startConstWars("ConstvsConst-Unif-Unif.html", GroupWeightFunctions.uniformWeights, FamilyWeightFunctions.uniformWeights)
  // startConstWars("ConstvsConst-Exp-Unif.html", GroupWeightFunctions.exponentialWeights, FamilyWeightFunctions.uniformWeights)
   startConstWars("ConstvsConst-Unif-Exp.html", GroupWeightFunctions.uniformWeights, FamilyWeightFunctions.exponentialWeights)
  // startConstWars("ConstvsConst-Exp-Exp.html", GroupWeightFunctions.exponentialWeights, FamilyWeightFunctions.exponentialWeights)
    
  }
}