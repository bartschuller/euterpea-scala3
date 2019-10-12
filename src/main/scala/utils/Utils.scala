package utils

trait IntOps
    def (a: Int) mod (b: Int) =
        val x = a % b; if (x < 0) x + b else x

given IntOps

trait RandomGen[G <: RandomGen[_]]
    def next: (Int, G)
    def split: (G, G)
    def nextBetween(lo: Int, hi: Int): (Int, G)
    def nextBetween(lo: Double, hi: Double): (Double, G)
    
class StdGen(private val r: java.util.Random) extends RandomGen[StdGen]
    // The returned StdGen contains the mutable java Random which will generate a fresh number
    // The original StdGen will have memorized the returned value, preventing new calls to
    // the java Random which would affect the other one.
    lazy val next: (Int, StdGen) = (r.nextInt(), StdGen(r))
    def split: (StdGen, StdGen) =
        import StdGen.mkStdGen
        (mkStdGen(r.nextLong), mkStdGen(r.nextLong))
    // I'm cheating and adding this handy utility method.
    def nextBetween(lo: Int, hi: Int): (Int, StdGen) =
        (lo+r.nextInt(hi-lo+1), StdGen(r))
    def nextBetween(lo: Double, hi: Double): (Double, StdGen) =
        (lo+(hi-lo)*r.nextDouble(), StdGen(r))

object StdGen
    def mkStdGen(seed: Long) = StdGen(new java.util.Random(seed))

trait Bounded[A]
    def range: (A, A) = (minBound, maxBound)
    def minBound: A
    def maxBound: A

object Bounded
    given Bounded[Int]
        def minBound = Integer.MIN_VALUE
        def maxBound = Integer.MAX_VALUE

/** With a source of random number supply in hand, the Random class allows the programmer to extract random values of a variety of types.
Minimal complete definition: randomR and random.
*/
trait Random[A]
    /**
    * Takes a range (lo,hi) and a random number generator g, and returns a random value uniformly distributed in the closed interval [lo,hi], together with a new generator. It is unspecified what happens if lo>hi. For continuous types there is no requirement that the values lo and hi are ever produced, but they may be, depending on the implementation and the interval.
    */
    def randomR[G <: RandomGen[_]](range: (A, A), g: RandomGen[G]): (A, G)
    def random[G <: RandomGen[_], B <: A : Bounded](g: RandomGen[G]): (A, G) =
        randomR(summon[Bounded[B]].range, g)

object Random
    import scala.language.implicitConversions
    def randomR[G <: RandomGen[_], A: Random](range: (A, A), g: RandomGen[G]): (A, G) =
        summon[Random[A]].randomR(range, g)

    def randomRs[G <: RandomGen[_], A: Random](range: (A, A), g: RandomGen[G]): LazyList[A] =
        val (a, gp) = randomR(range, g)
        a #:: randomRs(range, gp)

    def random[G <: RandomGen[_], A: Random : Bounded](g: RandomGen[G]): (A, G) =
        summon[Random[A]].random(g)

    def randoms[G <: RandomGen[_], A: Random : Bounded](g: RandomGen[G]): LazyList[A] =
        val (a, gp) = random(g)
        a #:: randoms(gp)

    given Random[Int]
        def randomR[G <: RandomGen[_]](range: (Int, Int), g: RandomGen[G]): (Int, G) =
            g.nextBetween(range._1, range._2)
        override def random[G <: RandomGen[_], B <: Int : Bounded](g: RandomGen[G]): (Int, G) =
            g.next
        
    given Random[Double]
        def randomR[G <: RandomGen[_]](range: (Double, Double), g: RandomGen[G]): (Double, G) =
            g.nextBetween(range._1, range._2)

trait Enum[A]
    def toEnum(i: Int): A
    def fromEnum(a: A): Int
object Enum
    def apply[A](given Enum[A]) = summon[Enum[A]]

/**
   * The mapAccumL function behaves like a combination of map and foldl; it applies a function to each element of a list, passing an accumulating parameter from left to right, and returning a final value of this accumulator together with the new list.
   */
def mapAccumL[X, Y, ACC](s: ACC, lx: List[X])(f: (ACC, X) => (ACC, Y)): (ACC, List[Y]) =
    lx match
    case Nil => (s, Nil)
    case x :: xs =>
        val (sp, y) = f(s, x)
        val (spp, ys) = mapAccumL(sp, xs)(f)
        (spp, y :: ys)