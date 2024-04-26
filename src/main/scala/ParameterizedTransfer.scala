sealed trait LogicalFormula{
  def symbols:Set[VVar]
}
case class LAnd(v1:LogicalFormula, v2:LogicalFormula) extends LogicalFormula{
  override def symbols: Set[VVar] = v1.symbols.union(v2.symbols)
}
case class LOr(v1:LogicalFormula, v2:LogicalFormula) extends LogicalFormula {
  override def symbols: Set[VVar] = v1.symbols.union(v2.symbols)
}
case class LNot(v:LogicalFormula) extends LogicalFormula{
  override def symbols: Set[VVar] = v.symbols
}
case class LEq(v1:ValueFormula, v2:ValueFormula) extends LogicalFormula{
  override def symbols: Set[VVar] = v1.symbols.union(v2.symbols)
}
case class LLt(v1:ValueFormula, v2:ValueFormula) extends LogicalFormula{
  override def symbols: Set[VVar] = v1.symbols.union(v2.symbols)
}
case object LTrue extends LogicalFormula{
  override def symbols = Set()
}
case object LFalse extends LogicalFormula{
  override def symbols = Set()
}

sealed trait ValueFormula{
  def symbols:Set[VVar]
}
case class VNumber(n:Int) extends ValueFormula{
  override def symbols = Set()
}
case class VVar(name:String, prePost:PrePost) extends ValueFormula{
  override def symbols = Set()
}
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
