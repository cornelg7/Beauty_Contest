package beautycontest.Controlers

trait Player {
  var uid: Int = -1
  def name: String
  def getChoice: Int
  var myGame: Game = _

  override def equals(obj: Any): Boolean = obj match {
    case x: Player => x.uid == this.uid
    case _ => super.equals(obj)
  }
}
