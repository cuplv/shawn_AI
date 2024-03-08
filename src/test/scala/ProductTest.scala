class ProductTest extends munit.FunSuite {

  test("Test simple terminating while loop"){
    val prg =
      WhileSeq(
        WhileAssign("x",Num(1)),
        WhileWhile(Var("x"), WhileAssign("x",Num(0)))
      )
    val interp = Interpreter(prg, ProductTransfer(WhileIntervalTransfer, 
      EvenOddTransfer, IntervalEvenOddCombination, EvenOddIntervalCombination))
    val outState = interp.fixedPoint()
    println(prg.toStringWithInvar(outState.loc2invar, true))
    assertEquals(outState(prg.postLoc).get.asInstanceOf[SomeIntervalState].mem,
      Map("x" -> Interval(0, 1))) //TODO: should be 0,0 when we can narrow
  }
}
