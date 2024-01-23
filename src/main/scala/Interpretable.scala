import scala.util.Random
import scala.collection.BitSet
import scala.annotation.tailrec

sealed trait Dir
case object ForwardsDir extends Dir
case object BackwardsDir extends Dir


trait Interpretable[ILoc, ITransition] {

  def getInitLoc:ILoc
  
  def transitionsFwd(loc:ILoc):Iterable[ITransition]
  
  def transitionsBkwd(loc:ILoc):Iterable[ITransition]

}

trait Transfer[IState, ITransition]{

  def getDirection:Dir

  def getInitState():IState

  def transfer(srcState:IState, transition:ITransition):IState
  
  def join(s1:IState, s2:IState):IState
  

}

//trait Interpreter[ILoc, IState,
//  ITransition<:Transition,
//  IInterpretable<:Interpretable[ILoc, ITransition],
//  ITransfer<:Transfer[IState,ITransition]]{
case class Interpreter[IState,ILoc,ITransition](interpretable:Interpretable[ILoc,ITransition],
                                                transfer:Transfer[IState,ITransition]){
  
  type InvarMap = Map[ILoc,IState]
  def initialInvarMap = Map(interpretable.getInitLoc -> Set(transfer.getInitState()))
  
  def step(in:InvarMap):InvarMap

}

//[1] p: (lambda x:x x) (lambda x: x x)
//   execution: 1 p -> 1 p -> 1 p -> ...
// [0] while true [1] skip [2]
//   execution: 0 -> 1 -> 2 -> 1 -> 2 ...

// [0] if rand() then [1] x = 1 else [2] x = 2 fi [3]
// first ex 0 -> 1[x:1] -> 3[x:1]
// second ex 0 -> 2[x:2] -> 3[x:2]
// step from [0] results in StateSelector that can give 1 or 2
