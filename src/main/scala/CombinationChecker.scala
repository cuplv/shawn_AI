/**
 *  The goal of this is to do a "pre-analysis" check to see if the transfer functions are fully defined over a given program.
 *  Given a program, initial states, and a set of transfers, checkAllValid should only return true if interpret cannot
 *  get stuck due to a missing transfer function.
 *
 *  For example,
 *  a numeric domain may have commands like `x = x - 1`  and states like `x -> 5` at each location
 *  and a blocking domain may have commands like `assume x > 1` and states like `blocked` or `not blocked` at a location
 *
 *  for a program like `while x > 1 do x = x-1` the blocking domain applies after the condition and numeric in other places.
 *  // TODO: still thinking through this
 */
case class CombinationChecker(program:Program, initialStates:Set[(StateC, Location)], transfers:Set[TransferHandler]) {
  // ensure that no state transition pair may get stuck with no transfer
  def checkAllValid():Boolean = {
    val atLocation: Map[Location, Set[(StateC, Location)]] = initialStates.groupBy{case (_,loc) => loc}
    val invariant:Map[Location, Set[StateID]] = atLocation.map{case (loc,v) => loc -> v.map{case (state,_) => state.tag}}

    ??? // TODO: worklist algorithm to accumulate the StateID possible at each location and ensure transfer functions exist for each
  }

  def interpret():Map[Location, Set[StateC]] = ???

}


// *** Abstract Definitions For Programs Transitions and Locations***
case class Program(transitions:Set[Transition])

type TransitionID = String
case class Location(id:Int)

trait Transition{
  def tag:TransitionID
  def getPre:Location
  def getPost:Location
}
// *** State Definitions ***

type StateID = String
trait StateC{
  def tag:StateID

}

// *** Interpreter definitions ***

trait TransferHandler{
  def forwardStateTransitionSupport(stateT:StateID, transitionT:TransitionID):Option[StateID]
  def forwardTransfer(pre:StateC, transition:Transition):StateC

}

// *** numeric example ***

case class NAssign(tgtExpr:RVal, srcExpr:LVal, getPre:Location, getPost:Location) extends Transition{
  def tag:TransitionID = "NAssign"
}

case class NState(mem:Map[Var,Int]) extends StateC{
  def tag:StateID = "NState"
}

case object NumericTransfer extends TransferHandler{
  def forwardStateTransitionSupport(stateT:StateID, transitionT:TransitionID):Option[StateID] = (stateT, transitionT) match{
    case ("NState", "NTransition") => Some("NTransition")
    case _ => None
  }
  def forwardTransfer(pre:StateC, transition:Transition):StateC = {
    ???
  }
}

// ***