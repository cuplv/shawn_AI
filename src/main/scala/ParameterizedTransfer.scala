sealed trait LogicalFormula
case class LAnd(v1:LogicalFormula, v2:LogicalFormula) extends LogicalFormula
case class LOr(v1:LogicalFormula, v2:LogicalFormula) extends LogicalFormula
case class LNot(v:LogicalFormula) extends LogicalFormula
case class LEq(v1:ValueFormula, v2:ValueFormula) extends LogicalFormula
case class LLt(v1:ValueFormula, v2:ValueFormula) extends LogicalFormula
case object LTrue extends LogicalFormula
case object LFalse extends LogicalFormula

sealed trait ValueFormula
case class VNumber(n:Int) extends ValueFormula
case class VVar(name:String, prePost:PrePost) extends ValueFormula
trait AbstractDomain[T]{
  def formulaToAbstraction(formula:LogicalFormula, prePost: PrePost)(implicit ctx:TransferSolverContext):T
  def abstractionToFormula(abstraction:T, prePost:PrePost):LogicalFormula
}

//case class ConjunctedState(conjunctions:List[Any])

//case class ConjunctedStateHandler(domains:List) extends AbstractDomain[ConjunctedState]{
//  def formulaToAbstraction(formula: LogicalFormula, prePost: PrePost): ConjunctedState = ???
//
//  def abstractionToFormula(abstraction: ConjunctedState): LogicalFormula = ???
//
//}
case class ParameterizedTransfer[T](domains:AbstractDomain[T]) extends
  Transfer[T, UnstructuredLoc,Unstructured]{
  implicit val transferSolverContext:TransferSolverContext = Z3SolverContext
  override def getDirection: Dir = ???

  override def getInitState(): T = ???

  override def transfer(srcState: T, interpretable: Unstructured,
                        preLoc: UnstructuredLoc, postLoc: UnstructuredLoc): T =
    val formulaTransfer = interpretable.getForwardPreAndPostFormula(preLoc,postLoc)
    val srcConstraints = domains.abstractionToFormula(srcState,Pre)
    domains.formulaToAbstraction(LAnd(formulaTransfer, srcConstraints), Post)

  override def join(s1: T, s2: T): T= ???

  override def widen(s1: T, s2: T): T= ???

  override def bottomValue: T= ???
}
