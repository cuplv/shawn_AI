class ProductTest extends munit.FunSuite {

  test("transfer interval"){
    val prg = Assume(Not(Var("x")))
//    val transfer = ProductTransfer(WhileIntervalTransfer,
//      EvenOddTransfer, IntervalEvenOddCombination, EvenOddIntervalCombination)
    val intPre = SomeIntervalState(Map("x" -> Interval(0,0)))
    val eoPre = SomeEvenOddState(Map("x" -> Odd))
    val intPostState = WhileIntervalTransfer.transferStep(intPre, prg)
    assert(intPostState == intPre)
    val eoPostState = EvenOddTransfer.transferStep(eoPre,prg)
    assert(eoPostState == eoPre)

    val intPre2 = SomeIntervalState(Map("x" -> Interval(1, 1)))
    val eoPre2 = SomeEvenOddState(Map("x" -> Even))
    val intPostState2 = WhileIntervalTransfer.transferStep(intPre2, prg)
    assert(intPostState2 == intPre2)
    val eoPostState2 = EvenOddTransfer.transferStep(eoPre2, prg)
    assert(eoPostState2 == eoPre2)
  }
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
    val stateAtEnd = outState(prg.postLoc)
    assertEquals(stateAtEnd.get.asInstanceOf[SomeIntervalState].mem,
      Map("x" -> Interval(0, 1))) //TODO: should be 0,0 when we can narrow
  }
}
