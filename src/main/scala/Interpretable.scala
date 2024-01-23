import scala.util.Random
import scala.collection.BitSet
import scala.annotation.tailrec

trait Loc{
  def getSeedProvider(dir:Dir):SeedProvider
}
sealed trait Dir
case object ForwardsDir extends Dir
case object BackwardsDir extends Dir
trait Program

trait Seed

case class EnumeratedSeed(v:BitSet) extends Seed

case object OnlySeed extends Seed
sealed trait SeedProvider{
  def sample:Seed
}
case object Single extends SeedProvider{
  def sample:Seed = OnlySeed
}
//case class Bound(n:Integer) extends SeedProvider:
//  override def sample: Seed = ???
case object Infinite extends SeedProvider:
  val random = Random()
  def randomBitSet:BitSet = {
    var cset = BitSet.empty
    var ind = 0
    while(random.nextBoolean()) {
      if(random.nextBoolean()){
        cset = cset + ind
      }
      ind = ind + 1
    }
    cset
  }
  override def sample: Seed = EnumeratedSeed(randomBitSet)

trait Interpretable[IProgram<:Program, ILoc<:Loc,IState, IValue] {

  case class StateSelector(seedProvider: SeedProvider, stateThunk: Seed => (ILoc, IState)){
    def sample:(ILoc,IState) = stateThunk(seedProvider.sample)
  }

  def getInitLoc(program:IProgram):ILoc
  def getInitState:IState
  def isTerm(program:IProgram, state:IState, loc:ILoc):Boolean
  def concStep(program:IProgram, direction: Dir, srcLoc:ILoc, srcState:IState):StateSelector
  def executeFwd(program:IProgram):IState = {
    @tailrec
    def iExecute(program:IProgram, state:IState, loc:ILoc):IState = {
      val (nextLoc:ILoc,nextState:IState) = concStep(program,ForwardsDir, loc, state).sample
      iExecute(program, nextState, nextLoc)
    }
    iExecute(program, getInitState, getInitLoc(program))
  }
}



//[1] p: (lambda x:x x) (lambda x: x x)
//   execution: 1 p -> 1 p -> 1 p -> ...
// [0] while true [1] skip [2]
//   execution: 0 -> 1 -> 2 -> 1 -> 2 ...

// [0] if rand() then [1] x = 1 else [2] x = 2 fi [3]
// first ex 0 -> 1[x:1] -> 3[x:1]
// second ex 0 -> 2[x:2] -> 3[x:2]
// step from [0] results in StateSelector that can give 1 or 2
