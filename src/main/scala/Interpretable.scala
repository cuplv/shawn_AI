import scala.collection.{SortedSet}
import scala.annotation.tailrec

sealed trait Dir
case object ForwardsDir extends Dir
case object BackwardsDir extends Dir

trait Loc{
  /**
   * Interpreter must define a topological ordering over locations
   * @param other  Compare whether `this` or `other` location comes first in the order
   * @return negative if `other` comes first, zero if same, positive if this comes first.
   */
  def preTopo(other:Loc):Integer
}


trait Interpretable[ILoc] {
  def hasIncomingBackEdges(loc:ILoc):Boolean

  def getInitLoc:ILoc
  
  def transitionsFwd(loc:ILoc):Option[Iterable[ILoc]]
  
  def transitionsBkwd(loc:ILoc):Option[Iterable[ILoc]]

}

trait Transfer[IState, ILoc<:Loc, IInterpretable<:Interpretable[ILoc]]{

  def getDirection:Dir

  def getInitState():IState

  def transfer(srcState:IState, interpretable:IInterpretable, preLoc:ILoc, postLoc:ILoc):IState
  
  def join(s1:IState, s2:IState):IState

  def widen(s1:IState, s2:IState):IState

  def bottomValue:IState

}


/**
 * Represent an invariant map for interpretation
 * @param loc2invar current invariant at a given loation
 * @param mod was this location modified on the last step
 * @tparam ILoc locations used
 * @tparam IState states used
 */
case class InvarMap[ILoc<:Loc, IState](loc2invar: Map[ILoc,IState], mod:SortedSet[ILoc]){
  def apply(loc:ILoc):Option[IState] = loc2invar.get(loc)

  def popMod = (mod.head, this.copy(mod = mod.tail))
  def insertLoc(loc:ILoc, state:IState):InvarMap[ILoc,IState] =
    this.copy(loc2invar + (loc -> state), mod + loc)
}

case class Interpreter[IState,ILoc<:Loc,IInterpretable<:Interpretable[ILoc]](interpretable:IInterpretable,
                                                transfer:Transfer[IState,ILoc,IInterpretable]){
  implicit val LocOrder: Ordering[ILoc] = (x: ILoc, y: ILoc) => {
    x.preTopo(y)
  }
  def initialInvarMap:InvarMap[ILoc,IState] = {
    InvarMap(Map(interpretable.getInitLoc -> transfer.getInitState()), SortedSet(interpretable.getInitLoc))
  }
  
  def step(in:InvarMap[ILoc,IState]):InvarMap[ILoc,IState] =
    val (srcLoc, in2) = in.popMod
    val srcState: IState = in(srcLoc).getOrElse(throw new IllegalStateException("src should always be defined"))
    val fwdTrans = interpretable.transitionsFwd(srcLoc).getOrElse(Seq.empty)
    fwdTrans.foldLeft(in2){ case (in, tgtLoc) =>
      val tgtState = transfer.transfer(srcState,interpretable,srcLoc,tgtLoc)

      val currTgtState = in(tgtLoc).getOrElse(transfer.bottomValue)
      val insState = if(interpretable.hasIncomingBackEdges(tgtLoc))
        transfer.widen(currTgtState,tgtState)
      else transfer.join(currTgtState,tgtState)
      in.insertLoc(tgtLoc,tgtState)
    }
  def fixedPoint():InvarMap[ILoc,IState] = {
    @tailrec
    def iFixedPoint(invar:InvarMap[ILoc,IState]): InvarMap[ILoc,IState] = {
      if(invar.mod.isEmpty)
        invar
      else
        iFixedPoint(step(invar))
    }
    iFixedPoint(initialInvarMap)
  }

}