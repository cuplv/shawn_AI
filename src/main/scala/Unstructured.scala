case class Unstructured(edges:Map[UnstructuredLoc, Set[(Step,UnstructuredLoc)]],
                        initialLoc:UnstructuredLoc) extends Interpretable[UnstructuredLoc] {

  def findLoc(loc:UnstructuredLoc):Set[(Step,UnstructuredLoc)] = edges(loc)

  override def hasIncomingBackEdges(loc: UnstructuredLoc): Boolean = ???

  override def getInitLoc: UnstructuredLoc = initialLoc

  /**
   *
   * @param loc
   * @return None if we cannot generate post commands from this location/cmd,
   *         Some(list) for when we can generate locations
   */
  override def transitionsFwd(loc: UnstructuredLoc): Option[Iterable[UnstructuredLoc]] = if(edges.contains(loc)){
    Some(edges(loc).map{case (_,tgtLoc) => tgtLoc})
  } else None

  override def transitionsBkwd(loc: UnstructuredLoc): Option[Iterable[UnstructuredLoc]] = ???

  override def toStringWithInvar[IState](invar: Map[UnstructuredLoc, IState], printPrePost: Boolean): String = ???

  override def getForwardPreAndPostFormula(pre: Loc, post: Loc): LogicalFormula = (pre,post) match{
    case (pre:UnstructuredLoc, post:UnstructuredLoc) =>
      val step: Option[(Step, UnstructuredLoc)] = edges(pre).find{case (step,progPost) => post == progPost}
      step match
        case Some((step,_)) => step.getForwardPreAndPostFormula
        case None => LFalse
  }
}

case class UnstructuredLoc(i:Int) extends Loc {
  override def preTopo(interpretable:Interpretable[_],other:Loc):Integer = {
    0
  }
}


/**
 * Precise logical representation of state.
 */
case class StateFormula() //TODO


trait LogiState[IState]{
  def abstractStateFormula(stateFormula: StateFormula):IState

}


//TODO: unused, delete later
case object BasicIntervalAbstraction extends Transfer[IntervalState, UnstructuredLoc,Unstructured]{

  override def getDirection: Dir = ForwardsDir

  override def getInitState(): IntervalState = SomeIntervalState(Map.empty)

  override def transfer(srcState: IntervalState, program: Unstructured, srcLoc:UnstructuredLoc,
                        tgtLoc:UnstructuredLoc): IntervalState = {
    val edges = program.findLoc(srcLoc)
    val targetEdges: Set[(Step, UnstructuredLoc)] = edges.filter(_._2 == tgtLoc)
    assert(targetEdges.size == 1, s"Found ${targetEdges.size} edges expected 1")
    val (step:Step,_:UnstructuredLoc) = targetEdges.head
    transferStep(srcState,step)
  }
  def lt(v1:Int | Inf.type , v2:Int | Inf.type):Boolean = {
    ???
  }

  def evalLt(i1:IntervalVal, i2:IntervalVal):IntervalVal = (i1, i2) match{
    case (BotVal, _) => BotVal
    case (_,BotVal) => BotVal
    case (i1:Interval, i2:Interval) if lt(i1.high,i2.low) => Interval(1,1)
    case (i1:Interval, i2:Interval) if i1 == i2 => Interval(0,0)
    case v =>
      println(v)
      ???
  }
  def transferRVal(srcState:IntervalState, rval:RVal) :IntervalVal = rval match
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
    case Havoc => Interval(Inf, Inf)
    case Lt(r1, r2) =>
      val r1interval = transferRVal(srcState, r1)
      val r2interval = transferRVal(srcState, r2)
      evalLt(r1interval, r2interval)
    case a => println(a); ???

  def transferStep(srcState:IntervalState, step:Step):IntervalState = step match
    case Nop => srcState
    case StepAssign(Var(varname), rval) =>
      srcState.putVar(varname,transferRVal(srcState,rval))
    case StepAssign(a,b) =>
      println(s"unimplemented $a = $b") ; ???
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
trait RVal
case class Num(n:Integer) extends RVal
case class Plus(e1:RVal, e2:RVal) extends RVal
case class Not(whileRVal: RVal) extends RVal


case class Lt(r1:RVal, r2:RVal) extends RVal

/**
 * Expression that may return any value (standin for user input/random etc)
 */
case object Havoc extends RVal
trait LVal extends RVal
case class Var(name:String) extends LVal
sealed trait Step{
  def getForwardPreAndPostFormula:LogicalFormula

}
case object Nop extends Step:
  override def getForwardPreAndPostFormula: LogicalFormula = LTrue

case class Assume(cond:RVal) extends Step:
  override def getForwardPreAndPostFormula: LogicalFormula = cond match
    case Var(name) => LAnd(LNot(LEq(VVar(name,Post), VNumber(0))), LEq(VVar(name,Pre), VVar(name,Post)))



case class StepAssign(lhs:LVal, rhs:RVal) extends Step:
  override def getForwardPreAndPostFormula: LogicalFormula = ???
