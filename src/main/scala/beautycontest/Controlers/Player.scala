package beautycontest.Controlers

trait Player {
  var uid: Int = 0
  def name: String
  def getChoice: Int

  override def equals(obj: Any): Boolean = obj match {
    case x: Player => x.uid == this.uid
    case _ => super.equals(obj)
  }
}
