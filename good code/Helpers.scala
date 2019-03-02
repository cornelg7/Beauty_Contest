package beautycontest

object Helpers {
  def rround2double(d: Double): Double = math.rint(d * 100) / 100

  def rround2(x: Float): Double = (math rint x * 100) / 100
}
