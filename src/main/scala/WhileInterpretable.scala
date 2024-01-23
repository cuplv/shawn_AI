import java.util.UUID

trait WhileTransition
trait WhileCmd extends Interpretable[WhileLoc, WhileTransition]{
  def id:UUID
  def preLoc:WhileLoc = WhileLoc(Pre, id, this)
  def postLoc:WhileLoc = WhileLoc(Post,id, this)
  def findLoc(loc:WhileLoc):Option[WhileCmd]
  def up(loc:WhileLoc):Option[WhileCmd]
}

case class WhileSeq(c1:WhileCmd, c2:WhileCmd) extends WhileCmd{
  val uuid = java.util.UUID.randomUUID
  override def id: UUID = uuid
  def findLoc(loc:WhileLoc):Option[WhileCmd] = if(id == loc.id) Some(this) else c1.findLoc(loc).orElse(c2.findLoc(loc))
  def up(loc:WhileLoc):Option[WhileCmd] = {
    if(List(c1,c2).exists{cn => cn.preLoc == loc || cn.postLoc == loc}){
        Some(this)
      }else
        c1.up(loc).orElse(c2.up(loc))
  }
}


  trait WhileRVal
case class Num(n:Integer) extends WhileRVal

case class WhileAssign(varname:String, rhs:WhileRVal) extends WhileCmd{
  val uuid = java.util.UUID.randomUUID
  override def id: UUID = uuid
  def findLoc(loc:WhileLoc):Option[WhileCmd] = if(id == loc.id) Some(this) else None
  def up(loc:WhileLoc):Option[WhileCmd] = None
}

sealed trait PrePost
case object Pre extends PrePost
case object Post extends PrePost

case class WhileLoc(prePost:PrePost, id:UUID, cmd:WhileCmd){
  def mkPost = this.copy(prePost=Post)
  def mkPre = this.copy(prePost=Pre)

}

type WhileState = Map[String,Integer]



