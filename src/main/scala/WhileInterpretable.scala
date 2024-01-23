import java.util.UUID

trait WhileCmd extends Program{
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

case class WhileLoc(prePost:PrePost, id:UUID, cmd:WhileCmd) extends Loc{
  def getSeedProvider(dir:Dir): SeedProvider = (cmd, dir) match {
    case (_: WhileSeq, ForwardsDir) => Single
    case (_: WhileSeq, BackwardsDir) => Single
    case (_: WhileAssign, ForwardsDir) => Single
    case (_: WhileAssign, BackwardsDir) => Infinite
  }
  def mkPost = this.copy(prePost=Post)
  def mkPre = this.copy(prePost=Pre)

}

type WhileState = Map[String,Integer]

case object NoNext extends Exception

case object WhileInterpretable extends Interpretable[WhileCmd, WhileLoc,WhileState, Integer] {
  def getInitLoc(program:WhileCmd): WhileLoc =  program.preLoc
  def getInitState: WhileState = Map.empty
  def isTerm(program:WhileCmd, state:WhileState, loc:WhileLoc):Boolean = ???

  private def forwardInterpRval(r:WhileRVal, preState:WhileState): Seed => Integer = r match
    case Num(n) => _ => n
    case _ => ???

  private def forwardInterpCmd(srcLoc:WhileLoc, c:WhileCmd, preState:WhileState):Option[StateSelector] =
    def singleResult(c:WhileCmd):Seed => (WhileLoc, WhileState) = c match {
      case WhileAssign(varname, rhs) if c.preLoc == srcLoc =>
        s => (c.postLoc, preState + (varname -> forwardInterpRval(rhs, preState)(s)))
      case WhileSeq(c1, c2) if srcLoc == c.preLoc => s => (c1.preLoc, preState)
      case WhileSeq(c1, c2) if srcLoc == c1.postLoc => s => (c2.preLoc, preState)
      case WhileSeq(c1, c2) if srcLoc == c2.postLoc => s => (c.postLoc, preState)
      case _:WhileAssign => throw NoNext
    }
    try {
      Some(StateSelector(Single, singleResult(c)))
    }catch {
      case NoNext => None
    }



  def concStep(program: WhileCmd, direction: Dir,
               srcLoc: WhileLoc, srcState: WhileState): StateSelector = {
    val c = program.findLoc(srcLoc).getOrElse(throw new IllegalArgumentException("bad location"))
    direction match {
      case ForwardsDir =>
        forwardInterpCmd(srcLoc,c,srcState).getOrElse{
          forwardInterpCmd(srcLoc,program.up(srcLoc).get, srcState).get
        }
    }
  }



}
