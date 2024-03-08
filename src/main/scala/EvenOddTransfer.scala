sealed trait EvenOddVal{
  def join(other:EvenOddVal):EvenOddVal
}

case object TopEvenOddVal extends EvenOddVal{
  override def join(other: EvenOddVal): EvenOddVal = TopEvenOddVal
}
case object Even extends EvenOddVal:
  override def join(other: EvenOddVal): EvenOddVal = other match
    case TopEvenOddVal => TopEvenOddVal
    case Even => Even
    case Odd => TopEvenOddVal
    case BotEvenOddVal => Even

case object Odd extends  EvenOddVal:
  override def join(other: EvenOddVal): EvenOddVal = other match
    case TopEvenOddVal => TopEvenOddVal
    case Even => TopEvenOddVal
    case Odd => Odd
    case BotEvenOddVal => Odd

case object BotEvenOddVal extends EvenOddVal:
  override def join(other: EvenOddVal): EvenOddVal = other
sealed trait EvenOddState {
  def getVar(name:String):EvenOddVal
  def putVar(name:String, evenOdd:EvenOddVal):EvenOddState

}

case class SomeEvenOddState(mem: Map[String,EvenOddVal]) extends EvenOddState{
  override def toString() = s"{${mem.toList.map{a => s"${a._1} -> ${a._2}"}.mkString(",")}}"
  override def putVar(name: String, evenOdd: EvenOddVal): EvenOddState = {
    this.copy(mem = mem + (name -> evenOdd))
  }

  override def getVar(name: String): EvenOddVal = mem.getOrElse(name,BotEvenOddVal)
}
case object BotEvenOddState extends EvenOddState{
  override def toString() = "\u22A5"
  override def putVar(name: String, interval: EvenOddVal): EvenOddState = BotEvenOddState

  override def getVar(name: String): EvenOddVal = BotEvenOddVal
}
case object EvenOddTransfer extends Transfer[EvenOddState, WhileLoc, WhileCmd] {

  override def getDirection: Dir = ???

  override def getInitState(): EvenOddState = SomeEvenOddState(Map())

  def transferRVal(srcState: EvenOddState, rval: RVal): EvenOddVal = rval match
    case Num(n) if n%2 == 0 => Even
    case Num(n) if n%2 == 1 => Odd
    case Var(n) => srcState.getVar(n)
    case Not(v) =>
      val toNeg = transferRVal(srcState, v)
      ???
    case Havoc => TopEvenOddVal
    case Lt(r1, r2) =>
      ???
    case a =>
      println(a); ???
  def transferStep(state: EvenOddState, step: Step):EvenOddState = step match
    case Nop => state
    case Assume(cond) =>
      state
//      val condEval = transferRVal(state,cond)
//      condEval match
//        case TopEvenOddVal => ???
//        case Even => ???
//        case Odd => ???
//        case BotEvenOddVal => ???
    case StepAssign(Var(varname), rhs) => state.putVar(varname, transferRVal(state,rhs))

  override def transfer(srcState: EvenOddState, interpretable: WhileCmd, preLoc: WhileLoc, postLoc: WhileLoc): EvenOddState = srcState match {
    case SomeEvenOddState(mem) =>
      interpretable.findLoc(preLoc).map{cmd =>
        val step = cmd.getStep(preLoc, postLoc)
        transferStep(srcState,step)
      }.getOrElse(throw new IllegalStateException(s"location $preLoc not found"))
    case BotEvenOddState => BotEvenOddState
  }

  override def join(s1: EvenOddState, s2: EvenOddState): EvenOddState = (s1,s2) match{
    case (BotEvenOddState,o) => o
    case (o,BotEvenOddState) => o
    case (SomeEvenOddState(mem1), SomeEvenOddState(mem2)) =>
      val combined = mem1.toList ++ mem2.toList //TODO: inefficient concat
      val newMap = combined.groupBy(a => a._1)
        .map{case (k, vals) => vals.reduce{case ((n,v1),(_,v2)) => (n,v1.join(v2))}}.flatMap{
          case t@(k,v:Interval) => Some(t)
          case _ => None
        }
      SomeEvenOddState(newMap)
  }

  override def widen(s1: EvenOddState, s2: EvenOddState): EvenOddState = ???

  override def bottomValue: EvenOddState = BotEvenOddState
}
