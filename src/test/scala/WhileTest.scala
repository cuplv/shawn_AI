// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class WhileTest extends munit.FunSuite {
  val prg = WhileSeq(WhileAssign("x", Num(1)), WhileAssign("y", Num(2)))
  test("take a couple steps forward into a simple program") {
    val interp = Interpreter(prg, IntervalTransfer)
    val initial = interp.initialInvarMap

    val out = (1 until 8).foldLeft(initial){case (state,_) =>
      println(state)
      interp.step(state)
    }

    ???
  }

  test("fully execute a program forward"){
//    val res = WhileInterpretable.executeFwd(prg)
//    println(res)
//    assert(res == Map("x" -> 1, "y" -> 2))
???
  }
}
