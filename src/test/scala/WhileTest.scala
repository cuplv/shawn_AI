// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

import munit.Clue.generate
class WhileTest extends munit.FunSuite {
  test("take a couple steps forward into a simple program") {
    val prg = WhileSeq(WhileAssign("x", Num(1)), WhileAssign("y", Num(2)))
    val interp = Interpreter(prg, WhileIntervalTransfer)
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
    val interp = Interpreter(prg, WhileIntervalTransfer)
    val outState = interp.fixedPoint()
    println(prg.toStringWithInvar(outState.loc2invar, true))
    assertEquals(outState(prg.postLoc).get.asInstanceOf[SomeIntervalState].mem,
      Map("x" -> Interval(0, 1))) //TODO: should be 0,0 when we can narrow
  }

  test("Test simple non-terminating while loop"){
    val prg =
      WhileSeq(
        WhileAssign("x", Num(1)),
        WhileWhile(Var("x"), WhileAssign("x", Num(1)))
      )
    val interp = Interpreter(prg, WhileIntervalTransfer)
    val outState = interp.fixedPoint()
    // assert exit is bottom or unreachable
    assertEquals(outState(prg.postLoc).getOrElse(BotIntervalState), BotIntervalState)
  }
  test("Test simple widen"){
    val prg = WhileSeq(
      WhileAssign("x", Num(1)),
      WhileAssign("i", Havoc),
      WhileWhile(Lt(Var("x"),Var("i")),
        WhileAssign("x", Plus(Var("x"),Num(1))))
    )
    val interp = Interpreter(prg, WhileIntervalTransfer)
    val outState = interp.fixedPoint()

    ???
  }
}
