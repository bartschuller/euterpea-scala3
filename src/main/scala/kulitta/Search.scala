package kulitta
/*
Search algoritm for use with chord spaces
Donya Quick and Paul Hudak
Scala translation by Bart Schuller

Implementation of a search algorithm for traversing chord spaces using
let-in constraints as well as progression level predicates.
*/
import utils.{given, _}
import QuotientSpaces._
import chordspaces.OPTIC._
import Constraints._

object Search
    type Constraints = Seq[Seq[(Int, Int)]]
    type Index = Int
    type Bound = Int
/*
Now we define the greedy algorithm, greedyProg.

We define choose, a function to stochastically select an element
from a list.
*/
    def choose[A](g: StdGen, xs: Seq[A]): (StdGen, A) =
        val (r, gp) = g.next
        (gp, xs(r mod xs.length))
/*
And finally the recursive, greedy function, greedyProg, and its
"helper," greedyChord.
*/
    type Fallback[A] = (EqClass[A], StdGen, A) => (StdGen, A)

    def greedyChord[A]( e: EqClass[A], yprev: A, hpair: Predicate[(A,A)],
                        f: Fallback[A], g: StdGen): (StdGen, A) =
        val (rand, gp) = g.next
        val yxs = LazyList.continually(yprev).zip(e)
        val ys = yxs.filter(hpair).map(_._2)
        if ys.isEmpty then f(e, gp, yprev)
        else (gp, ys(rand % ys.length))
/*
And also a nearest neighbor fallback function that would always 
succeed if the equivalence class has at least one element (which 
should always be the case).

> nearFall ::  EqClass AbsChord -> StdGen -> AbsChord -> (StdGen, AbsChord)
> nearFall e g x = 
>     let ds = map (simpleDist x) e :: [Double]
>         y = e !! (head $ findIndices (==minimum ds) ds)
>     in  (g, y)
*/
    def nearFall(e: EqClass[AbsChord], g: StdGen, x: AbsChord): (StdGen, AbsChord) =
        val ds: Seq[Double] = e.map(simpleDist(_, x))
        import Ordering.Double.TotalOrdering
        val y = e(ds.indexOf(ds.min))
        (g, y)
/*
This version of greedyProg operates over a list of equivalence classes.
*/
    def greedyProgp[A](c: Predicate[(A,A)], f: Fallback[A], g: StdGen, ess: List[EqClass[A]]): Seq[A] =
        ess match
        case Nil => Nil
        case e :: es =>
            val (gp, y0) = choose(g, e) // randomly choose the first AbsChord
            def greedyRecp( c: Predicate[(A,A)], f: Fallback[A], g: StdGen,
                            y: A, pts: Seq[EqClass[A]]): List[A] =
                if pts.isEmpty then List(y) else
                    val (gp, yi) = greedyChord(pts.head, y, c, f, g)
                    y :: greedyRecp(c, f, gp, yi, pts.tail)
            greedyRecp(c, f, g, y0, es)
/*
The applyCons function applys let-in constraints to an index list. Indices
on the left are given preference when satisfying constraints.
*/
    def applyCons(inds: Seq[Int], ijss: Seq[(Int,Int)]): Seq[Int] =
        ???
/*
==================================

THIS VERSION OF GREEDY PROG DOES NOT WORK - LETS ARE NOT DONE RIGHT

A version of greedyProg to support Let statements. Constraints are satisfied from 
left to right. Breaks are likely to occurr with repeats. For example, with the
progression

	let x = ... in x x

a break in voice-leading behavior is likely to occur between the last chord of the
first instance of x and the first chord of the second instance of x. There is 
currently no way around this using the greedy approach. The more exact searches
further up in this file are alternatives in such cases.
*/
    def greedyLet[A](p: Predicate[(A,A)], f: Fallback[A], k: Constraints,
                    es: List[EqClass[A]], g: StdGen): Seq[A] =
        import math.Ordering.Implicits._
        val n = es.length
        val cs = greedyProgp(p, f, g, es)
        val consPat = k.sorted.foldLeft(0 until n)(applyCons)
        consPat.map(cs(_))
end Search