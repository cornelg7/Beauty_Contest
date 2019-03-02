package beautycontest

object GroupWeightFunctions {
  
  def uniformWeights(vsHowMany: Int, bound: Int): Float = 1.toFloat/bound

  def exponentialWeights(vsHowMany: Int, bound: Int): Float = 1.toFloat/scala.math.pow(2, vsHowMany-1).toFloat
}