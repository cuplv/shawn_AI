object IntervalAbstraction extends LogiState[IntervalState] {
  def abstractStateFormula(stateFormula: StateFormula):IntervalState = {
    ???
  }
}
sealed trait IntervalState {
  def getVar(name:String):IntervalVal
  def putVar(name:String, interval:IntervalVal):IntervalState
}
case class SomeIntervalState(mem: Map[String,IntervalVal]) extends IntervalState{
  override def toString() = s"{${mem.toList.map{a => s"${a._1} -> ${a._2}"}.mkString(",")}}"
  override def putVar(name: String, interval: IntervalVal): IntervalState = {
    this.copy(mem = mem + (name -> interval))
  }

  override def getVar(name: String): IntervalVal = mem.getOrElse(name,BotVal)
}
case object BotIntervalState extends IntervalState{
  override def toString() = "\u22A5"
  override def putVar(name: String, interval: IntervalVal): IntervalState = BotIntervalState

  override def getVar(name: String): IntervalVal = BotVal
}