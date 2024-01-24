import java.util.UUID
import scala.collection.View


type LocID = UUID
trait WhileLoc extends Loc{
  def locID:LocID
  def preTopo(other:Loc):Integer = ???
}

case class WhilePre(locID:LocID) extends WhileLoc
case class WhilePost(locID:LocID) extends WhileLoc

trait WhileCmd extends Interpretable[WhileLoc]{
  def getStep(pre:WhileLoc, post:WhileLoc):Step
  def id:LocID
  def findLoc(loc:WhileLoc):Option[WhileCmd]

  def preLoc:WhileLoc
  def postLoc:WhileLoc
}


sealed trait Step //while commands decomposed into small steps
case object Nop extends Step

case class WhileSeq(c1:WhileCmd, c2:WhileCmd) extends WhileCmd {


  val uuid = java.util.UUID.randomUUID
  override def id: LocID = uuid
  def findLoc(loc:WhileLoc):Option[WhileCmd] =
    if(id == loc.locID)
      Some(this)
    else
      c1.findLoc(loc).orElse(c2.findLoc(loc))

  override def getInitLoc: WhileLoc = WhileSeq.PreC1(id)

  override def transitionsFwd(loc: WhileLoc): Option[Iterable[WhileLoc]] =
    loc match {
      case WhileSeq.PreC1(lic) if lic == id => Some(Seq(c1.preLoc))
      case s if s == c1.postLoc => Some(Seq(WhileSeq.PostC1(id)))
      case WhileSeq.PostC1(lic) if lic == id => Some(Seq(c2.preLoc))
      case s if s == c2.postLoc => Some(Seq(WhileSeq.PostC2(id)))
      case loc =>
        c1.transitionsFwd(loc).orElse(c2.transitionsFwd(loc))
    }

  override def transitionsBkwd(loc: WhileLoc): Option[Iterable[WhileLoc]] = ???

  override def hasIncomingBackEdges(loc: WhileLoc): Boolean = false

  override def getStep(pre: WhileLoc, post: WhileLoc): Step = Nop

  override def preLoc: WhileLoc = WhileSeq.PreC1(id)

  override def postLoc: WhileLoc = WhileSeq.PostC2(id)
}

object WhileSeq{
  sealed trait SeqLoc extends WhileLoc
  case class PreC1(locID:LocID) extends SeqLoc
  case class PostC1(locID:LocID) extends SeqLoc
  case class PostC2(locID:LocID) extends SeqLoc
}



trait WhileRVal
case class Num(n:Integer) extends WhileRVal

case class WhileAssign(varname:String, rhs:WhileRVal) extends WhileCmd with Step{
  val uuid = java.util.UUID.randomUUID
  override def id: UUID = uuid
  def findLoc(loc:WhileLoc):Option[WhileCmd] = if(id == loc.locID) Some(this) else None

  override def getInitLoc: WhileLoc = ???

  override def transitionsFwd(loc: WhileLoc): Option[Iterable[WhileLoc]] =
    if(loc.locID == id)
      Some(Seq(postLoc))
    else
      None

  override def transitionsBkwd(loc: WhileLoc): Option[Iterable[WhileLoc]] = ???

  override def hasIncomingBackEdges(loc: WhileLoc): Boolean = false

  override def getStep(pre: WhileLoc, post: WhileLoc): Step = this

  override def preLoc: WhileLoc = WhilePre(id)

  override def postLoc: WhileLoc = WhilePost(id)
}


sealed trait PrePost
case object Pre extends PrePost
case object Post extends PrePost



case object Inf

/**
 * Represents a numeric value within a defined interval (inclusive if number exclusive if inf)
 * @param low lower bound of interval
 * @param high upper bound of interval
 */
case class Interval(low:Int | Inf.type, high:Int | Inf.type)


sealed trait IntervalState{
  def putVar(name:String, interval:Interval):IntervalState
}
case class SomeIntervalState(mem: Map[String,Interval]) extends IntervalState{
  override def putVar(name: String, interval: Interval): IntervalState = this.copy(mem = mem + (name -> interval))
}
case object BotIntervalState extends IntervalState{
  override def putVar(name: String, interval: Interval): IntervalState = BotIntervalState
}




case object IntervalTransfer extends Transfer[IntervalState, WhileLoc,WhileCmd]{

  override def getDirection: Dir = ForwardsDir

  override def getInitState(): IntervalState = SomeIntervalState(Map.empty)

  override def transfer(srcState: IntervalState, program: WhileCmd, srcLoc:WhileLoc,
                        tgtLoc:WhileLoc): IntervalState = {
    program.findLoc(srcLoc).map{cmd =>
      val step = cmd.getStep(srcLoc, tgtLoc)
      transferStep(srcState,step)
    }.getOrElse(throw new IllegalStateException(s"location $srcLoc not found"))
  }
  def transferRVal(srcState:IntervalState, rval:WhileRVal) :Interval = rval match
    case Num(n) => Interval(n,n)
    case _ => ???

  def transferStep(srcState:IntervalState, step:Step):IntervalState = step match
    case Nop => srcState
    case WhileAssign(varname, rval) =>
      srcState.putVar(varname,transferRVal(srcState,rval))

  override def join(s1: IntervalState, s2: IntervalState): IntervalState = (s1,s2) match{
    case (BotIntervalState,s2) => s2
    case (s1,BotIntervalState) => s1
    case (SomeIntervalState(mem1), SomeIntervalState(mem2)) =>
      val combined = View(mem1,mem2)
      SomeIntervalState(???)
  }

  def joinValue(v1:Interval,v2:Interval):Interval =
    ???

  override def widen(s1: IntervalState, s2: IntervalState): IntervalState = ???

  override def bottomValue: IntervalState = BotIntervalState
}



