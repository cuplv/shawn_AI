import org.sosy_lab.common.ShutdownManager
import org.sosy_lab.common.configuration.Configuration
import org.sosy_lab.common.log.{BasicLogManager, LogManager}
import org.sosy_lab.java_smt.SolverContextFactory
import org.sosy_lab.java_smt.SolverContextFactory.Solvers
import org.sosy_lab.java_smt.api.SolverContext
import com.microsoft.z3._


trait TransferSolverContext{
  def simplify(formula:LogicalFormula):LogicalFormula
}
case object JSolverContext extends TransferSolverContext{
   val config:Configuration = Configuration.defaultConfiguration()
  val logger:LogManager  = BasicLogManager.create(config)
  val shutdown:ShutdownManager = ShutdownManager.create()

  // SolverContext is a class wrapping a solver context.
  // Solver can be selected either using an argument or a configuration option
  // inside `config`.
  val context:SolverContext  = SolverContextFactory.createSolverContext(
    config, logger, shutdown.getNotifier(), Solvers.SMTINTERPOL)

  override def simplify(formula: LogicalFormula): LogicalFormula = ???
}

case object Z3SolverContext extends TransferSolverContext{
  val context = new Context()
  val solver = context.mkSolver()

  def valueToSmt(v:ValueFormula):Expr[_<:ArithSort] = v match
    case VVar(name,prePost) => context.mkConst(s"${name}_${prePost.toString}", context.mkIntSort())
    case VNumber(n) => context.mkInt(n)
  def formulaToSmt(formula:LogicalFormula):Expr[BoolSort] = formula match {
    case LAnd(v1, v2) => context.mkAnd(formulaToSmt(v1), formulaToSmt(v2))
    case LLt(v1,v2) => context.mkLt(valueToSmt(v1), valueToSmt(v2))
    case LNot(v) => context.mkNot(formulaToSmt(v))
    case LEq(v1,v2) => context.mkEq(valueToSmt(v1), valueToSmt(v2))
    case LTrue => context.mkTrue()
    case LFalse => context.mkFalse()

  }
  override def simplify(formula: LogicalFormula): LogicalFormula = {
    solver.reset()
    solver.add(formulaToSmt(formula))
    solver.check()
    val model = solver.getModel
    ???
  }
}
