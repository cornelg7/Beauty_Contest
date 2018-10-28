package beautycontest.Controlers.Strategies

import beautycontest.Controlers.Player


// basic strategy that always chooses the given number
class SConstant(choice: Integer) extends Player{
  override def name: String = "SConstant"

  override def getChoice: Int = choice
}
