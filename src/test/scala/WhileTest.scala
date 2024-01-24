// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class WhileTest extends munit.FunSuite {
  test("take a couple steps forward into a simple program") {
    val prg = WhileSeq(WhileAssign("x", Num(1)), WhileAssign("y", Num(2)))
    val interp = Interpreter(prg, IntervalTransfer)
    val initial = interp.initialInvarMap

    val out = (1 until 7).foldLeft(initial){case (state,_) =>
      println(state)
      interp.step(state)
    }
    val stateAtLastSingleStep = out(prg.postLoc).get

    val stateAtLastFixedPoint = interp.fixedPoint()(prg.postLoc).get
    List(stateAtLastSingleStep,stateAtLastFixedPoint).foreach { stateAtLast =>
      assertEquals(stateAtLast.asInstanceOf[SomeIntervalState].mem,
        Map("x" -> Interval(1, 1), "y" -> Interval(2, 2)))
    }
  }

  test("Reach a fixed point over a program with a loop"){
???
  }
}
