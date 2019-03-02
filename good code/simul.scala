/*def startConstWars(outFile: String): Unit = {
      var cgame: Game = null
      var games = List[gameResult]()
      var maxNumberOfEnemyPlayers = 10
      
      
        /// Table head (first row)
      var tablehead = List[String]()
      for (i <- 0 to 100)
        tablehead = (" "+i+" ") :: tablehead
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
        for (cj <- 0 to 100) {
          games = List[gameResult]()
          if (ci != cj) {
              /// Run 'one vs many' games for each i
            for (i <- 2 to maxNumberOfEnemyPlayers) {
              val nn = i + 1
              val rr = 1
              val pp: Float = 0.8.toFloat
              var pplayers = protagonist :: List[Player]()
             // var pplayers: List[Player] =  List(protagonist)
              for (j <- 1 to i) pplayers = new SConstant(cj) :: pplayers
              pplayers = pplayers.reverse
              cgame = new Game(pp, nn, rr, pplayers)
             // cgame.prepGameNoUI()
             // cgame.jumpToEnd()
              var win = 0
              win = cgame.scoreBoard.filter(x => x.player.equals(protagonist)).head.roundsWon
              //cgame.scoreBoard.filter(x => x.roundsWon == 1).foreach(x => if (x.player.equals(protagonist)) win = 1)
              games = gameResult(cj, i, win) :: games
            }
            games = games.reverse
            var ws = 0
          //  ss = "i am " + ci + "<ul>"
            games.foreach(x => {
           //   ss = ss + "<li>" + "vs " + x.hm + " of " + x.vs + ": " + x.w + "</li>"
              ws = ws + x.w
            })
          //  ss = ss + "</ul>"
          //  ss = ws + ss
            var fr = Helpers.rround2(ws.toFloat/games.length)
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
      printTable(results, outFile)
  }*/