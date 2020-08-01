package kulitta
/*
Search algoritm for use with chord spaces
Donya Quick and Paul Hudak
Scala translation by Bart Schuller

Implementation of a search algorithm for traversing chord spaces using
let-in constraints as well as progression level predicates.
*/
import utils.{given _, _}
import QuotientSpaces._
import chordspaces.OPTIC._
import Constraints._
import PTGG._
import Term._

object Search:
    type Constraints = List[List[(Int, Int)]]
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

    def greedyProg[A](  qs: QSpace[A], r: EqRel[A], c: Predicate[(A,A)],
                        f: Fallback[A], g: StdGen, xss: List[A]): List[A] =
        xss match
        case Nil => Nil
        case x :: xs =>
            val e = eqClass(qs, r)(x)
            val (gp, y0) = choose(g, e) // randomly choose the first AbsChord
            def greedyRec(  qs: QSpace[A], r: EqRel[A], c: Predicate[(A,A)],
                            f: Fallback[A], g: StdGen, y: A, pts: List[A]): List[A] =
                if pts.isEmpty then List(y) else
                    val (gp, yi) = greedyChord(eqClass(qs, r)(pts.head), y, c, f, g)
                    y :: greedyRec(qs, r, c, f, gp, yi, pts.tail)
            greedyRec(qs, r, c, f, gp, y0, xs)

    def greedyChord[A]( e: EqClass[A], yprev: A, hpair: Predicate[(A,A)],
                        f: Fallback[A], g: StdGen): (StdGen, A) =
        val (rand, gp) = g.next
        val yxs = LazyList.continually(yprev).zip(e)
        val ys = yxs.filter(hpair).map(_._2)
        if ys.isEmpty then f(e, gp, yprev)
        else (gp, ys(rand mod ys.length))
/*
Now we need some fall-back functions. We can construct them from
predicates.
*/
    def fallFun[A](c: Predicate[(A,A)])(e: EqClass[A], g: StdGen, x: A): (StdGen, A) =
        val ys = LazyList.continually(x).zip(e).filter(c).map(_._2)
        val (i, gp) = g.next
        if ys.isEmpty then sys.error(s"Stuck at symbol $x. No viable options left!")
        else (gp, ys(i mod ys.length))
/*
This is the default fallback function we used in our experiments:
*/
    val defFall = fallFun(maxClass(7))
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
    def greedyProgp[A](c: Predicate[(A,A)], f: Fallback[A], g: StdGen, ess: List[EqClass[A]]): List[A] =
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
The findInds function looks through a Term for let-in expressions.
When it finds Let x a exp, it calls findIndsSub on x and exp to 
determine which indices in the sequence are occupied by instances
of x. 
*/
    def findInds[A,B](e: List[(String, List[Term[A,B]])], tss: Seq[Term[A,B]]): List[List[(Int,Int)]] =
        tss match
        case Nil => Nil
        case t +: ts =>
            val rest = findInds(e, ts)
            t match
            case Var(_) => sys.error("undefined")
            case NT(_, _) => findInds(e, ts).map(_.map(pAdd(1)))
            case Let(x, a, exp) =>
                val ap = expand(e, a)
                val expp = expand((x,ap)::e, exp)
                findInds(e, a) ++ (findIndsSub(x, ap.length, expandp(e, exp)) ::
                    rest.map(_.map(pAdd(expp.length))))

    def expandp[A,B](e: List[(String, List[Term[A,B]])], tss: Sentence[A,B]): Sentence[A,B] =
        tss match
        case Nil => Nil
        case t :: ts =>
            t match
            case Let(x, a, exp) => expandp((x, expandp(e, a)) :: e, exp ++ expandp(e, ts))
            case Var(x) => e.find(_._1 == x).fold(Var(x) :: expandp(e, ts))(_._2 ++ expandp(e, ts))
            case x => x :: expandp(e, ts)
/*
The findIndsSub function looks for instances of a variable and
determines what indices they occupy.
*/
    def findIndsSub[A,B](x: String, xLen: Int, tss: List[Term[A,B]]): List[(Int,Int)] =
        tss match
        case Nil => Nil
        case t :: ts =>
            val rest = findIndsSub(x, xLen, ts)
            t match
            case Var(y) => if x==y then (0, xLen-1) :: rest.map(pAdd(xLen)) else rest
            case NT(_, _) => findIndsSub(x, xLen, ts).map(pAdd(1))
            case Let(_, _, _) => sys.error("(find Instances) This point should be unreachable.")

    def pAdd(amt: Int)(a: Int, b: Int) = (a+amt, b+amt)

/*
The applyCons function applys let-in constraints to an index list. Indices
on the left are given preference when satisfying constraints.
*/
    def applyCons(inds: Seq[Int], ijss: Seq[(Int,Int)]): List[Int] =
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
                    es: List[EqClass[A]], g: StdGen): List[A] =
        import math.Ordering.Implicits._
        val n = es.length
        val cs = greedyProgp(p, f, g, es)
        val consPat = k.sorted.foldLeft((0 until n).toList)(applyCons)
        consPat.map(cs(_))
end Search