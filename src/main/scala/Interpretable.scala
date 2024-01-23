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

trait Interpretable[IProgram<:Program, ILoc<:Loc, ITransition, IState] {

  def getInitLoc(program:IProgram):ILoc
  
  def transitionsFwd(loc:ILoc):Iterable[ITransition]
  
  def transitionsBkwd(loc:ILoc):Iterable[ITransition]

}

trait Transfer[IState, IInvariant]{
  
}

//[1] p: (lambda x:x x) (lambda x: x x)
//   execution: 1 p -> 1 p -> 1 p -> ...
// [0] while true [1] skip [2]
//   execution: 0 -> 1 -> 2 -> 1 -> 2 ...

// [0] if rand() then [1] x = 1 else [2] x = 2 fi [3]
// first ex 0 -> 1[x:1] -> 3[x:1]
// second ex 0 -> 2[x:2] -> 3[x:2]
// step from [0] results in StateSelector that can give 1 or 2
