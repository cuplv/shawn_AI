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
  (low, high) match{
    case (low:Int, high:Int) => assert(low <= high, s"Cannot construct contradictory interval low: $low  high: $high")
    case _ =>
  }
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
    case (Inf, Inf) => false
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
object IntervalAbstraction extends LogiState[IntervalState] {
  def abstractStateFormula(stateFormula: StateFormula):IntervalState = {
    ???
  }
}
sealed trait IntervalState {
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

case object IntervalStateAbstraction extends AbstractDomain[IntervalState]{

  override def formulaToAbstraction(formula: LogicalFormula, prePost: PrePost)
                                   (implicit ctx:TransferSolverContext): IntervalState =
    val simplified = ctx.simplify(formula)
    ???

  override def abstractionToFormula(abstraction: IntervalState, prePost: PrePost): LogicalFormula = abstraction match
    case SomeIntervalState(mem) => mem.map{ case (v,c) =>
      val upper = c match
        case BotVal => LFalse
        case Interval(_, high:Int) => LLt(VVar(v, prePost),VNumber(high +1))
        case Interval(_, Inf) => LTrue
      val lower = c match{
        case BotVal => LFalse
        case Interval(low:Int, _) => LLt(VNumber(low - 1), VVar(v,prePost))
        case Interval(Inf, _) => LTrue
      }
      LAnd(upper,lower)
    }.reduce{LAnd}
    case BotIntervalState => LFalse

}