// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class WhileTest extends munit.FunSuite {
  val prg = WhileSeq(WhileAssign("x", Num(1)), WhileAssign("y", Num(2)))
  test("take a couple steps forward into a simple program") {
    val interp = Interpreter(prg, IntervalTransfer)
    val initial = interp.initialInvarMap

    val out = (1 until 7).foldLeft(initial){case (state,_) =>
      println(state)
      interp.step(state)
    }
    val stateAtLast = out(prg.postLoc).get
    assert( stateAtLast.asInstanceOf[SomeIntervalState].mem ==
      Map("x" -> Interval(1,1), "y" -> Interval(2,2)) )
  }

  test("fully execute a program forward"){
//    val res = WhileInterpretable.executeFwd(prg)
//    println(res)
//    assert(res == Map("x" -> 1, "y" -> 2))
???
  }
}
