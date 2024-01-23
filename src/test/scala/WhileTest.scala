// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class WhileTest extends munit.FunSuite {
  val prg = WhileSeq(WhileAssign("x", Num(1)), WhileAssign("y", Num(2)))
  test("take a couple steps forward into a simple program") {
    val step1 = WhileInterpretable.concStep(prg, ForwardsDir, WhileInterpretable.getInitLoc(prg),
      WhileInterpretable.getInitState)
    val (step1loc, step1state) = step1.sample
    val step2 = WhileInterpretable.concStep(prg, ForwardsDir, step1loc, step1state)

    assert(step2.sample._2 == Map("x" -> 1))
  }

  test("fully execute a program forward"){
    val res = WhileInterpretable.executeFwd(prg)
    println(res)
    assert(res == Map("x" -> 1, "y" -> 2))
  }
}
