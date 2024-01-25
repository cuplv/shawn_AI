// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

import munit.Clue.generate
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

  test("Test simple terminating while loop"){
    val prg =
      WhileSeq(
        WhileAssign("x",Num(1)),
        WhileWhile(Var("x"), WhileAssign("x",Num(0)))
      )
    val interp = Interpreter(prg, IntervalTransfer)
    val outState = interp.fixedPoint()
    println(outState)
    println(prg.toStringWithInvar(outState.loc2invar, true))
    assertEquals(outState(prg.postLoc).asInstanceOf[SomeIntervalState].mem,
      Map("x" -> Interval(0, 0))) //TODO: === still failing
  }
}
