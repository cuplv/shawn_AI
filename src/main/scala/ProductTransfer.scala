case class ProductState[State1,State2](s1:State1, s2:State2)

trait ConstrainWith[State1,State2]{
  def constrain(toConstrain:State1, constrainWith:State2):State1
}
case class ProductTransfer[State1,State2](
                                           transfer1:Transfer[State1, WhileLoc, WhileCmd],
                                           transfer2:Transfer[State2, WhileLoc, WhileCmd],
                                           constrainState1With2:ConstrainWith[State1,State2],
                                           constrainState2With1:ConstrainWith[State2,State1]
                                         )

  extends Transfer[ProductState[State1, State2], WhileLoc, WhileCmd] {
  override def getDirection: Dir = ???

  override def getInitState(): ProductState[State1, State2] =
    ProductState(transfer1.getInitState(), transfer2.getInitState())

  override def transfer(srcState: ProductState[State1, State2],
                        interpretable: WhileCmd, preLoc: WhileLoc, postLoc: WhileLoc): ProductState[State1, State2] =
    srcState match{
      case ProductState(s1,s2) =>
        val transferredS1 = transfer1.transfer(s1, interpretable, preLoc, postLoc)
        val transferredS2 = transfer2.transfer(s2, interpretable, preLoc, postLoc)
        val newS1 = constrainState1With2.constrain(transferredS1, transferredS2)
        val newS2 = constrainState2With1.constrain(transferredS2, transferredS1)
        ProductState(newS1,
          newS2)
    }

  override def join(s1: ProductState[State1, State2], s2: ProductState[State1, State2]): ProductState[State1, State2] =
    (s1,s2) match {
      case (ProductState(s11, s21), ProductState(s12, s22)) =>
        ProductState(transfer1.join(s11,s12), transfer2.join(s21,s22))
    }

  override def widen(s1: ProductState[State1, State2], s2: ProductState[State1, State2]): ProductState[State1, State2] =
    (s1, s2) match {
      case (ProductState(s11, s21), ProductState(s12, s22)) =>
        ProductState(transfer1.widen(s11, s12), transfer2.widen(s21, s22))
    }

  override def bottomValue: ProductState[State1, State2] =
    ProductState[State1,State2](transfer1.bottomValue, transfer2.bottomValue)

}