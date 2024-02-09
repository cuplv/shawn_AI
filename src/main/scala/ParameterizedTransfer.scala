class ParameterizedTransfer[AbstractionFunctions, IState] extends Transfer[IState, UnstructuredLoc,Unstructured]{
  override def getDirection: Dir = ???

  override def getInitState(): IState = ???

  override def transfer(srcState: IState, interpretable: Unstructured, preLoc: UnstructuredLoc, postLoc: UnstructuredLoc): IState = ???

  override def join(s1: IState, s2: IState): IState = ???

  override def widen(s1: IState, s2: IState): IState = ???

  override def bottomValue: IState = ???
}
