object IntervalEvenOddCombination extends ConstrainWith[IntervalState, EvenOddState]{

  def roundDown(v:Int, oddVal: EvenOddVal):Int = oddVal match
    case TopEvenOddVal => v
    case Even if v%2 == 0=> v
    case Even if v%2 == 1=> v - 1
    case Odd if v%2 == 0 => v - 1
    case Odd if v%2 == 1 => v

  def constrainInterval(intervalVal: IntervalVal, oddVal: EvenOddVal):IntervalVal =
    if(oddVal == BotEvenOddVal) {
      BotVal
    } else {
      intervalVal match {
        case Interval(low: Int, high: Int) => Interval(roundDown(low, oddVal), roundDown(high, oddVal))
        case Interval(low: Int, Inf) => Interval(roundDown(low, oddVal), Inf)
        case Interval(Inf, high: Int) => Interval(Inf, roundDown(high, oddVal))
        case i: Interval => i
      }
    }
  override def constrain(toConstrain: IntervalState, constrainWith: EvenOddState): IntervalState = (toConstrain,constrainWith) match{
    case (SomeIntervalState(inState), SomeEvenOddState(eoState)) =>
      SomeIntervalState(inState.map{
        case (variable, interval) => (variable, constrainInterval(interval, eoState.getOrElse(variable, TopEvenOddVal)))
      })
    case (BotIntervalState, _) => BotIntervalState
    case (_, BotEvenOddState) => BotIntervalState
  }
}

object EvenOddIntervalCombination extends ConstrainWith[EvenOddState, IntervalState] {
  override def constrain(toConstrain: EvenOddState, constrainWith: IntervalState): EvenOddState = toConstrain
}
