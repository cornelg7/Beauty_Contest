package beautycontest.Controlers.Strategies

import beautycontest.Controlers.Player

class SPawn(myMaster: SMastermind) extends Player{
  override def name: String = "SPawn"

  // starting from 0
  val myID: Int = myMaster.addPawn(this)

  override def getChoice: Int = {
    myMaster.whatToPickAsPawn(myID)
  }
}
