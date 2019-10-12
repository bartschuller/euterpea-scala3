package jazz
import utils.{given, _}
import jazz.JazzTypes._
import euterpea.Music._
import Primitive._

object Utils
    type PitchSpace = Seq[AbsPitch]
    def filterByScale(s: Scale)(ps: PitchSpace): PitchSpace =
        ps.filter(p => s.contains(p mod 12))
    
    def orderByNearest(ps: PitchSpace, p: AbsPitch): PitchSpace =
        val dists = ps.map(p1 => (p1 - p).abs)
        dists.zip(ps).sortBy(_._1).map(_._2)

    def pitches[A](m: Music[A]): LazyList[A] =
        def pFun(prim: Primitive[A]): LazyList[A] =
            prim match
            case Note(d, p) => LazyList(p)
            case Rest(d) => LazyList.empty
        val append = (s1: LazyList[A], s2: LazyList[A]) => s1 ++ s2
        def ctrlFun(c: Control, l: LazyList[A]) = l
        mFold(pFun, append, append, ctrlFun)(m)

    // choose: select uniformly at random from a list
    def choose[A](g: StdGen, xs: Seq[A]): (StdGen, A) =
        if (xs.isEmpty)
            sys.error("Nothing to choose from!")
        else
            val (r, g2) = g.next
            (g2, xs(r mod xs.length))
    
    def chooseN[A](g0: StdGen, i: Int, xs: Seq[A]): (StdGen, List[A]) =
        if i <= 0 then (g0, Nil) else
            val (g1, ys) = chooseN(g0, i-1, xs)
            val (g2, y) = choose(g1, xs)
            (g2, y::ys)
end Utils