package beautycontest.Controlers.Strategies

import beautycontest.Controlers.Player
import scala.util.Random


// basic strategy that returns a random number between 0 and the given (optional) one
class SRandom(maxRandom: Integer = 101) extends Player{
  override def name: String = "SRandom"

  val rand = new Random()
  override def getChoice: Int = rand.nextInt(maxRandom)
}
