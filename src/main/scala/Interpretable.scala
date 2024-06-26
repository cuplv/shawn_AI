import scala.util.Random
import scala.collection.BitSet
import scala.annotation.tailrec
import scala.collection.{SortedSet}

sealed trait Dir
case object ForwardsDir extends Dir
case object BackwardsDir extends Dir

trait Loc{
  /**
   * Interpreter must define a topological ordering over locations
   * @param other  Compare whether `this` or `other` location comes first in the order
   * @return negative if `other` comes first, zero if same, positive if this comes first.
   */
  def preTopo(interpretable:Interpretable[_],other:Loc):Integer
}


trait Interpretable[ILoc] {
  def hasIncomingBackEdges(loc:ILoc):Boolean

  def getInitLoc:ILoc

  /**
   *
   * @param loc
   * @return None if we cannot generate post commands from this location/cmd,
   *         Some(list) for when we can generate locations
   */
  def transitionsFwd(loc:ILoc):Option[Iterable[ILoc]]

  def transitionsBkwd(loc:ILoc):Option[Iterable[ILoc]]

  def toStringWithInvar[IState](invar:Map[ILoc,IState], printPrePost:Boolean = true):String
  def getForwardPreAndPostFormula(pre:Loc, post:Loc):LogicalFormula
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
 * Represent an invariant map for interpretation as well as a priority queue of modified locations.
 * @param loc2invar current invariant at a given loation
 * @param modifiedStateQueue Keep track of the modified locations and prioritize them based on topological order.
 *                           Note that the order comes from defining `implicit val LocOrder: Ordering[ILoc]` somewhere.
 *                           The SortedSet type is used because it efficiently maintains an order of locations
 *                           while maintaining immutability.
 *
 * @tparam ILoc locations used
 * @tparam IState states used
 */
case class InvarMap[ILoc<:Loc, IState](loc2invar: Map[ILoc,IState],
                                       modifiedStateQueue:SortedSet[ILoc]){
  def apply(loc:ILoc):Option[IState] = loc2invar.get(loc)

  /**
   * Find next modified location to apply transfer functions to based on ordering
   * @return
   */
  def popMod = (modifiedStateQueue.head, this.copy(modifiedStateQueue = modifiedStateQueue.tail))

  /**
   * Given a new state computed by the transfer functions, add it to the invariant map.
   * @param loc location of the new state
   * @param state the state to put at that location (note that join/widen are not applied here)
   * @return the new invariant map
   */
  def insertLoc(loc:ILoc, state:IState):InvarMap[ILoc,IState] =
    val oldLoc = this(loc)
    this.copy(loc2invar + (loc -> state), if(oldLoc.contains(state)) modifiedStateQueue else modifiedStateQueue + loc)
}

/**
 * Top level implementation of chaotic iteration.
 * @param interpretable the program under analysis
 * @param transfer defiinitions of how to transfer states across transitions in the interpretable
 * @tparam ILoc type capturing syntactic locations in the program (e.g. line and column of a source file)
 * @tparam IState Abstract states computed at each program location
 * @tparam IInterpretable the type of program under analysis.
 */
case class Interpreter[IState,ILoc<:Loc,IInterpretable<:Interpretable[ILoc]](interpretable:IInterpretable,
                                                transfer:Transfer[IState,ILoc,IInterpretable]){

  // Define the topological order used by the priority queue in the invariant map
  implicit val LocOrder: Ordering[ILoc] = (x: ILoc, y: ILoc) => {
    x.preTopo(interpretable,y)
  }
  def initialInvarMap:InvarMap[ILoc,IState] = {
    InvarMap(Map(interpretable.getInitLoc -> transfer.getInitState()), SortedSet(interpretable.getInitLoc))
  }
  val dbg = true

  def step(invariant:InvarMap[ILoc,IState]):InvarMap[ILoc,IState] =
    val (srcLoc, invariantWithoutCurLoc) = invariant.popMod
    val srcState: IState = invariant(srcLoc).getOrElse(throw new IllegalStateException("src should always be defined"))
    val fwdTrans = interpretable.transitionsFwd(srcLoc).getOrElse(Seq.empty)
    fwdTrans.foldLeft(invariantWithoutCurLoc){ case (in, tgtLoc) =>
      // apply the transfer function
      val tgtState = transfer.transfer(srcState,interpretable,srcLoc,tgtLoc)

      val currTgtState = in(tgtLoc).getOrElse(transfer.bottomValue)
      // the state to insert is either joined or widened based on whether we have incoming back edges
      //TODO: later it would be advantageous to make this policy configurable
      val insState = if(interpretable.hasIncomingBackEdges(tgtLoc))
        transfer.widen(currTgtState,tgtState)
      else transfer.join(currTgtState,tgtState)

      if(dbg){
        println(s"src loc: ${srcLoc}")
        println(s"src state: ${srcState}")
        println(s"tgt loc: ${tgtLoc}")
        println(s"tgt state${tgtState}")
      }
      // put the final state into the invariant map
      in.insertLoc(tgtLoc,insState)
    }
  def fixedPoint():InvarMap[ILoc,IState] = {
    @tailrec
    def iFixedPoint(invar:InvarMap[ILoc,IState]): InvarMap[ILoc,IState] = {
      if(dbg){
        println(interpretable.toStringWithInvar(invar.loc2invar,true))
      }
      if(invar.modifiedStateQueue.isEmpty)
        invar
      else
        iFixedPoint(step(invar))
    }
    iFixedPoint(initialInvarMap)
  }

}