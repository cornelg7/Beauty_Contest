package beautycontest.Controlers.Strategies

import beautycontest.Controlers.Player

class SMastermind extends Player{
  override def name: String = "SMastermind"

  var pawnNumber: Integer = 0
  var myPawns: List[SPawn] = List[SPawn]()
  var myChoice = 0
  var pawnChoices: List[Int] = List[Int]()

    // initially, everyone picks 50
  def setInitialChoices(): Unit = {
    myChoice = 50
    myPawns.foreach(x => pawnChoices = 50 :: pawnChoices)
  }

    // returns the ID of the pawn
  def addPawn(pawn: SPawn): Int = {
    myPawns = myPawns ++ List(pawn)
    pawnNumber += 1
    pawnNumber - 1
  }

  def addPawns(pawnList: List[SPawn]): Unit = {
    pawnList.foreach(x => addPawn(x))
  }

    // TODO: Update for better formula
    // TODO: update formula for more than one pawn
  def pawnChoiceFormula(wWithoutUs: Double, b: Double): Int = {
    ((wWithoutUs + (myGame.numberOfPlayers - 1) * b)/myGame.p).toInt
  }

  // TODO: predict the future better
  // "i think this (the average of all the previous win numbers) will be the new W"
  def predictingTheFuture(): Double = {
    var previousWinNumbers: List[Float] = List()
    myGame.previousRoundList.foreach(x => {
      previousWinNumbers = x.winNumber :: previousWinNumbers
    })
    var s: Float = 0
    var n: Int = 0
    previousWinNumbers.foreach(x => {
      s = s + x
      n = n + 1
    })
  //  println("current avg: " + s/n)
    s/n
  }

  def previousWinNumberWithoutUs(): Double = {
    var prevSumWithoutUs = myGame.previousRoundList.head.sumNumber
    prevSumWithoutUs -= myChoice
    pawnChoices.foreach(x => prevSumWithoutUs -= x)
    val nWithoutUs = myGame.numberOfPlayers - 1 - myPawns.length
    val avgWithoutUs = prevSumWithoutUs / nWithoutUs
    myGame.p * avgWithoutUs
  }

  def addPreviousWinNumberWithoutUsToAvg(currentAverage: Double): Double = {
    val toR = totalWWithoutUsN * currentAverage / (totalWWithoutUsN+1) + previousWinNumberWithoutUs()/(totalWWithoutUsN + 1)
    totalWWithoutUsN += 1
    toR
  }

  var totalWWithoutUsN: Int = 0
  var totalWWithoutUsAverage: Double = 0

  def prepareChoices(): Unit = {
    if (myGame.currentRoundNumber == 1)
      setInitialChoices()
    else {
//      println(myGame.previousRoundList.head.winNumber)
//      println("all choices: ")
//      myGame.previousRoundList.head.infoList.foreach(x => print(x.choice + " "))
//      println("")
//      println("DONE, next round........................")
//      println("")


      // find this number
      totalWWithoutUsAverage = addPreviousWinNumberWithoutUsToAvg(totalWWithoutUsAverage)
      val wWithoutUs: Double = totalWWithoutUsAverage

//      println("wwithoutus is: " + wWithoutUs)

      // then change the w in our favor by b
        // first find max b
          // TODO: choose b better
      var b: Double = -20
      val stp: Double = 0.5
      val startPointForB: Double = 10
      val threshold: Double = 2

      if (wWithoutUs >= 50) { // then try to have negative b
        b = -startPointForB
        while (pawnChoiceFormula(wWithoutUs, b) < 0 || wWithoutUs + b < 0) {
          b = b + stp
        }
        if (b == 0 || scala.math.abs(b) < threshold) {
          b = startPointForB
          while (pawnChoiceFormula(wWithoutUs, b) > 100 || wWithoutUs + b > 100) {
            b = b - stp
          }
        }
      }
      else { // try to have positive b
        b = startPointForB
        while (pawnChoiceFormula(wWithoutUs, b) > 100 || wWithoutUs + b > 100) {
          b = b - stp
        }
        if (b == 0 || scala.math.abs(b) < threshold) {
          b = -startPointForB
          while (pawnChoiceFormula(wWithoutUs, b) < 0 || wWithoutUs + b < 0) {
            b = b + stp
          }
        }
      }
//      println("b: " + b)

      if ((pawnChoiceFormula(wWithoutUs, b) >= 0 || wWithoutUs + b >= 0) && (pawnChoiceFormula(wWithoutUs, b) <= 100 || wWithoutUs + b <= 100)) {
        // then update my choice and pawns choices
        myChoice = (wWithoutUs + b).toInt
        pawnChoices = List()
        myPawns.foreach(x => {
          pawnChoices = pawnChoices ++ List(pawnChoiceFormula(wWithoutUs, b))
        })
      }
      else { // should never reach this.
        println("waaaaaaaaaaaaaaaaaat... b: " + b)
        // but in case it does, just pick the avg win number without them
        myChoice = wWithoutUs.toInt
        pawnChoices = List()
        myPawns.foreach(x => {
          pawnChoices = pawnChoices ++ List(wWithoutUs.toInt)
        })
      }

//      println("so i choose " + myChoice + " and pawn chooses " + pawnChoiceFormula(wWithoutUs, b))
//      println("but the actual win number was:")
    }
  }

  def whatToPickAsPawn(pawnID: Int): Int = {
    // asserting that prepareChoices has already been called
    pawnChoices(pawnID)
  }

  override def getChoice: Int = {
    prepareChoices()
    myChoice
  }
}
