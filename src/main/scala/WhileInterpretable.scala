import WhileWhile.WhileEPostCmd

import java.util.UUID
import scala.collection.View


type LocID = UUID
trait WhileLoc extends Loc{
  def locID:LocID
  def cmd:WhileCmd
  override def preTopo(interpretable:Interpretable[_],other:Loc):Integer = {
      //    if(!interpretable.isInstanceOf[WhileCmd]){
      //      return 0 // incomparable with other interpretables
      //    }
      //    val whileCmd = interpretable.asInstanceOf[WhileCmd]
      //    val whileLoc = other.asInstanceOf[WhileLoc]
      //    val c1 = whileCmd.findLoc(this)
      //    val c2 = whileCmd.findLoc(whileLoc)
    0 // TODO: implement reasonable topo ordering
  }
}

case class WhilePre(locID:LocID, cmd:WhileCmd) extends WhileLoc
case class WhilePost(locID:LocID, cmd:WhileCmd) extends WhileLoc

trait WhileCmd extends Interpretable[WhileLoc]{
  def getStep(pre:WhileLoc, post:WhileLoc):Step
  def id:LocID
  def findLoc(loc:WhileLoc):Option[WhileCmd]

  def preLoc:WhileLoc
  def postLoc:WhileLoc
//  def toStringWithInvar[T](invar:Map[WhileLoc,T], printPrePost:Boolean = true):String
}


sealed trait Step //while commands decomposed into small steps
case object Nop extends Step

case class Assume(cond:WhileRVal) extends Step

case class WhileSeq(c1:WhileCmd, c2:WhileCmd) extends WhileCmd {


  val uuid = java.util.UUID.randomUUID
  override def id: LocID = uuid
  def findLoc(loc:WhileLoc):Option[WhileCmd] =
    if(id == loc.locID)
      Some(this)
    else
      c1.findLoc(loc).orElse(c2.findLoc(loc))

  override def getInitLoc: WhileLoc = WhileSeq.PreC1(id,this)

  override def transitionsFwd(loc: WhileLoc): Option[Iterable[WhileLoc]] =
    loc match {
      case s if s == c2.postLoc => Some(Seq(WhileSeq.PostC2(id,this)))
      case WhileSeq.PreC1(lic,_) if lic == id => Some(Seq(c1.preLoc))
      case s if s == c1.postLoc => Some(Seq(WhileSeq.PostC1(id,this)))
      case WhileSeq.PostC1(lic,_) if lic == id => Some(Seq(c2.preLoc))
      case loc =>
        c1.transitionsFwd(loc).orElse(c2.transitionsFwd(loc))
    }

  override def transitionsBkwd(loc: WhileLoc): Option[Iterable[WhileLoc]] = ???

  override def hasIncomingBackEdges(loc: WhileLoc): Boolean = false

  override def getStep(pre: WhileLoc, post: WhileLoc): Step = Nop

  override def preLoc: WhileLoc = WhileSeq.PreC1(id,this)

  override def postLoc: WhileLoc = WhileSeq.PostC2(id,this)

  override def toStringWithInvar[T](invar: Map[WhileLoc, T], printPrePost: Boolean): String = {
    val rest = s"${c1.toStringWithInvar(invar, false)} " +
      s"${invar.getOrElse(WhileSeq.PostC1(id,this), BotIntervalState)} ; ${c2.toStringWithInvar(invar,false)}"
    if(printPrePost){
      s"${invar.getOrElse(preLoc, BotIntervalState)} ${rest} ${invar.getOrElse(postLoc,BotIntervalState)}"
    } else rest
  }
}

object WhileSeq{
  sealed trait SeqLoc extends WhileLoc
  case class PreC1(locID:LocID, cmd:WhileCmd) extends SeqLoc
  case class PostC1(locID:LocID, cmd:WhileCmd) extends SeqLoc
  case class PostC2(locID:LocID, cmd:WhileCmd) extends SeqLoc
}



case class WhileAssign(lhs:WhileLVal, rhs:WhileRVal) extends WhileCmd with Step{
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

  override def preLoc: WhileLoc = WhilePre(id, this)

  override def postLoc: WhileLoc = WhilePost(id, this)

  override def toStringWithInvar[T](invar: Map[WhileLoc, T], printPrePost: Boolean): String = {
    val rest = s"$lhs = $rhs"
    if(printPrePost){
      s"${invar(preLoc)} ${rest} ${invar(postLoc)}"
    } else rest
  }
}

object WhileAssign {
  def apply(name:String,rval:WhileRVal):WhileAssign = WhileAssign(Var(name),rval)

}
case class WhileWhile(cond:WhileRVal, cmd:WhileCmd) extends WhileCmd {

  val uuid = java.util.UUID.randomUUID

  override def getStep(pre: WhileLoc, post: WhileLoc): Step = (pre,post) match {
    case (WhileWhile.WhileEPre(id1,_), WhileWhile.WhileETrue(id2,_)) if id1 == id2 && id1 == id => Assume(cond)
    case (WhileWhile.WhileEPre(id1,_), WhileWhile.WhileEDone(id2,_)) if id1 == id2 && id1 == id => Assume(Not(cond))
    case (WhileWhile.WhileETrue(id1,_), WhilePre(locID, _)) if id1 == id => Nop
    case (WhileWhile.WhileEPostCmd(id1,_), WhileWhile.WhileEPre(id2,_)) if id1 == id2 && id == id1 =>
      Assume(cond)
    case (WhileWhile.WhileEPostCmd(id1,_), WhileWhile.WhileEDone(id2,_)) if id1 == id2 && id == id1 =>
      Assume(Not(cond))
    case (WhileWhile.WhileEDone(id1,_), tgt) if id1 == id && tgt.locID != id => Nop
    case (a,b) =>
      println(a)
      println(b)
      ???

  }

  override def id: LocID = uuid

  override def findLoc(loc: WhileLoc): Option[WhileCmd] = loc match{
    case _:WhileWhile.WhileWhileLoc if loc.locID == id => Some(this)
    case _ => cmd.findLoc(loc)
  }

  override def preLoc: WhileLoc = WhileWhile.WhileEPre(id,this)

  override def postLoc: WhileLoc = WhileWhile.WhileEDone(id,this)

  override def hasIncomingBackEdges(loc: WhileLoc): Boolean = ???

  override def getInitLoc: WhileLoc = ???

  override def transitionsFwd(loc: WhileLoc): Option[Iterable[WhileLoc]] = loc match
    case WhileWhile.WhileEPre(locID,_) if locID == id =>
      Some(Seq(WhileWhile.WhileETrue(id,this), WhileWhile.WhileEDone(id,this)))
    case WhileWhile.WhileETrue(locID,_) if locID == id =>
      Some(Seq(cmd.preLoc))
    case l if l == cmd.postLoc => Some(Seq(WhileWhile.WhileEPostCmd(id,this)))
    case WhileWhile.WhileEPostCmd(locID,_) if locID == id =>
      Some(Seq(WhileWhile.WhileEPre(id,this), WhileWhile.WhileEDone(id,this)))
    case WhilePre(locID, cmd) if locID != id => cmd.transitionsFwd(loc)
    case loc if loc.locID != id => cmd.transitionsFwd(loc)


  override def transitionsBkwd(loc: WhileLoc): Option[Iterable[WhileLoc]] = ???

  override def toStringWithInvar[T](invar: Map[WhileLoc, T], printPrePost: Boolean): String = {
    val rest = s"WHILE $cond DO ${invar.getOrElse(WhileWhile.WhileETrue(id,this), BotIntervalState)} " +
      s"${cmd.toStringWithInvar(invar,false)} ${invar.getOrElse(WhileWhile.WhileEPostCmd(id,this), BotIntervalState)} ELIHW"
    if(printPrePost){
      s"${invar(preLoc)} ${rest} ${invar(postLoc)}"
    } else rest
  }
}

object WhileWhile:
  trait WhileWhileLoc extends WhileLoc
  case class WhileEPre(locID: LocID, cmd:WhileCmd) extends WhileWhileLoc
  case class WhileETrue(locID: LocID, cmd:WhileCmd) extends WhileWhileLoc
  case class WhileEPostCmd(locID: LocID, cmd:WhileCmd) extends WhileWhileLoc
  case class WhileEDone(locID:LocID, cmd:WhileCmd) extends WhileWhileLoc

end WhileWhile

trait WhileRVal
case class Num(n:Integer) extends WhileRVal
case class Not(whileRVal: WhileRVal) extends WhileRVal
trait WhileLVal extends WhileRVal
case class Var(name:String) extends WhileLVal

sealed trait PrePost
case object Pre extends PrePost
case object Post extends PrePost



case object Inf

trait IntervalVal{
  def truthEy:Boolean
  def falseEy:Boolean
  def join(other:IntervalVal):IntervalVal
}
/**
 * Represents a numeric value within a defined interval (inclusive if number exclusive if inf)
 * @param low lower bound of interval
 * @param high upper bound of interval
 */
case class Interval(low:Int | Inf.type, high:Int | Inf.type) extends IntervalVal{
  override def toString():String =
    val first = low match{
      case i:Int => s"[$i"
      case Inf => s"(∞"
    }
    val last = high match{
      case i:Int => s"$i]"
      case Inf => s"∞)"
    }
    s"$first , $last"
  def truthEy:Boolean = (low,high) match{
    case (low:Int,high:Int) => low < 0 || high > 0
    case (Inf,_) => true
    case (_,Inf) => true
  }
  def falseEy:Boolean = (low,high) match{
    case (low:Int, high:Int) => low <= 0 || high >=0
    case (low:Int, Inf) => low <= 0
    case (Inf, high:Int) => high >=0
  }

  override def join(other: IntervalVal): IntervalVal = other match
    case BotVal => this
    case Interval(otherLow, otherHigh) => {
      val newLow = (otherLow,low) match{
        case (Inf, _) => Inf
        case (_, Inf) => Inf
        case (v1:Int, v2:Int) if v1 < v2 => v1
        case (v1:Int, v2:Int) => v2
      }
      val newHigh = (otherHigh,high) match{
        case (Inf, _) => Inf
        case (_, Inf) => Inf
        case (v1:Int, v2:Int) if v1 > v2 => v1
        case (v1:Int, v2:Int) => v2
      }
      Interval(newLow,newHigh)
    }

}
case object BotVal extends IntervalVal{
  override def truthEy: Boolean = false // cannot eval to true if unreachable
  override def falseEy: Boolean = false // cannot eval to true if unreachable

  override def join(other: IntervalVal): IntervalVal = other
}


sealed trait IntervalState{
  def getVar(name:String):IntervalVal
  def putVar(name:String, interval:IntervalVal):IntervalState
}
case class SomeIntervalState(mem: Map[String,IntervalVal]) extends IntervalState{
  override def toString() = s"{${mem.toList.map{a => s"${a._1} -> ${a._2}"}.mkString(",")}}"
  override def putVar(name: String, interval: IntervalVal): IntervalState = {
    this.copy(mem = mem + (name -> interval))
  }

  override def getVar(name: String): IntervalVal = mem.getOrElse(name,BotVal)
}
case object BotIntervalState extends IntervalState{
  override def toString() = "\u22A5"
  override def putVar(name: String, interval: IntervalVal): IntervalState = BotIntervalState

  override def getVar(name: String): IntervalVal = BotVal
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
  def transferRVal(srcState:IntervalState, rval:WhileRVal) :IntervalVal = rval match
    case Num(n) => Interval(n,n)
    case Var(n) => srcState.getVar(n)
    case Not(v) =>
      val toNeg = transferRVal(srcState, v)
      if (toNeg.falseEy && toNeg.truthEy) {
        Interval(0, 1)
      } else if (toNeg.falseEy) {
        Interval(1, 1)
      } else if (toNeg.truthEy) {
        Interval(0, 0)
      } else
        BotVal

    case a => println(a); ???

  def transferStep(srcState:IntervalState, step:Step):IntervalState = step match
    case Nop => srcState
    case WhileAssign(Var(varname), rval) =>
      srcState.putVar(varname,transferRVal(srcState,rval))
    case Assume(cond) =>
      val condEval = transferRVal(srcState,cond)
      if(condEval.truthEy){
        srcState //TODO: narrowing
      }else
        BotIntervalState

  override def join(s1: IntervalState, s2: IntervalState): IntervalState = (s1,s2) match{
    case (BotIntervalState,s2) => s2
    case (s1,BotIntervalState) => s1
    case (SomeIntervalState(mem1), SomeIntervalState(mem2)) =>
      val combined = mem1.toList ++ mem2.toList //TODO: inefficient concat
      val newMap = combined.groupBy(a => a._1)
        .map{case (k, vals) => vals.reduce{case ((n,v1),(_,v2)) => (n,v1.join(v2))}}.flatMap{
          case t@(k,v:Interval) => Some(t)
          case _ => None
        }
      SomeIntervalState(newMap)
  }

  def joinValue(v1:Interval,v2:Interval):Interval =
    ???

  override def widen(s1: IntervalState, s2: IntervalState): IntervalState = ???

  override def bottomValue: IntervalState = BotIntervalState
}



