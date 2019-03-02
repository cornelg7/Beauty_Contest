package beautycontest

object FamilyWeightFunctions {
  
  def uniformWeights(vsWho: Int, bound: Int): Float = 1.toFloat/bound

  def exponentialWeights(vsHowMany: Int, bound: Int): Float = 1.toFloat/scala.math.pow(2, vsHowMany+1).toFloat
}