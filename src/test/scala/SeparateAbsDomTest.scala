class SeparateAbsDomTest extends munit.FunSuite {
  test("concrete step over transition") {

    val l1 = UnstructuredLoc(1)
    val l2 = UnstructuredLoc(1)
    val prog = Unstructured(Map(l1 -> Set((Assume(Var("x")), l2))), l1)

    // set up transfer for interval
    val transfer = ParameterizedTransfer[IntervalState](IntervalStateAbstraction)

    val preState = SomeIntervalState(Map("x" -> Interval(Inf, -2)))

    val postState = transfer.transfer(preState, prog, l1, l2)


  }
  test("concrete step over transition to bottom") {

    val l1 = UnstructuredLoc(1)
    val l2 = UnstructuredLoc(1)
    val prog = Unstructured(Map(l1 -> Set((Assume(Var("x")), l2))), l1)

    // set up transfer for interval
    val transfer = ParameterizedTransfer[IntervalState](IntervalStateAbstraction)

    val preState = SomeIntervalState(Map("x" -> Interval(0, 0)))

    val postState = transfer.transfer(preState, prog, l1, l2)

    assertEquals(postState,BotIntervalState)


  }
}