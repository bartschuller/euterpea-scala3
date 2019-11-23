package kulitta
package chordspaces
/*
Chord Spaces Implementation
Donya Quick and Paul Hudak
Scala translation by Bart Schuller
*/
import QuotientSpaces._
import utils.{given, _}
object OPTIC
/*
Type definitions:
*/
    type PitchNum = Int // same as Euterpea's AbsPitch
    type AbsChord = Seq[Int]
    type Prog = Seq[AbsChord] // Chord progression
/*
The makeRange function will generate Z^n for user-specified ranges.

> makeRange :: [(PitchNum, PitchNum)] -> [AbsChord]
> makeRange = foldr (\(l,u) xs -> [(a:b) | a<-[l..u], b<-xs]) [[]]
*/
    def makeRange(pairs: Seq[(PitchNum,PitchNum)]): Seq[AbsChord] =
        pairs.foldRight(Seq(Seq.empty[Int])) {case ((l, u), xs) =>
            for
                a <- l to u
                b <- xs
            yield (a +: b) }
/*
A version of makeRange for use with sorted spaces:
*/
    def makeRangep(pairs: Seq[(PitchNum,PitchNum)]): Seq[AbsChord] =
        def pSort(xs: Seq[Int]): Boolean =
            xs match
            case a +: b +: t => a < b
            case _ => true
        pairs.foldRight(Seq(Seq.empty[Int])) {case ((l, u), xs) =>
            for
                a <- l to u
                b <- xs if pSort(a +: b)
            yield (a +: b) }
/*
========= O, P, & T IMPLEMENTATION =========

First we will define the octave and transposition operations. 
For f(x)=y with f in {o, t, p}, x~y for the corresponding 
equivalence relation (O, T, and P respectively).

*/
    def o(is: Seq[Int], c: AbsChord): AbsChord =
        is.zipWith(c) {(i, x) => x + 12 * i }
    def p(s: Seq[Int], xs: AbsChord): AbsChord =
        s.map(xs(_))
    def t(c: Int)(ac: AbsChord): AbsChord =
        ac.map(_+c)
/*
We define normalizations for O, P, T, OP, OT, and PT.
We also add a new definition, OPC.
*/
    def normO(x: AbsChord): AbsChord = x.map(_ % 12)
    def normT(x: AbsChord): AbsChord = x.map(_ - x.head)
    def normP(x: AbsChord): AbsChord = x.sorted
    def normOP(x: AbsChord): AbsChord = normO(x).sorted
    def normPT(x: AbsChord): AbsChord = normT(x.sorted)
    def normOT(x: AbsChord): AbsChord = normO(normT(x))
    def normPC(x: AbsChord): AbsChord = normP(x).distinct
    def normOPC(x: AbsChord): AbsChord = normOP(x).distinct
    def normOC(x: AbsChord): AbsChord = normC(normO(x))

    def normC(x: AbsChord): AbsChord =
        x.toList match
        case x1 :: x2 :: xs =>
            if x1 == x2 then normC(x2 :: xs) else x1 +: normC(x2 :: xs)
        case x => x
/*
Given a normalization, it can be turned into an 
equivalence relation.
*/
    def oEq(a: AbsChord, b: AbsChord): Boolean = normO(a) == normO(b)
    def pEq(a: AbsChord, b: AbsChord): Boolean = normP(a) == normP(b)
    def tEq(a: AbsChord, b: AbsChord): Boolean = normT(a) == normT(b)
    def opEq(a: AbsChord, b: AbsChord): Boolean = normOP(a) == normOP(b)
    def ptEq(a: AbsChord, b: AbsChord): Boolean = normPT(a) == normPT(b)
    def opcEq(a: AbsChord, b: AbsChord): Boolean = normOPC(a) == normOPC(b)

    def optEq(a: AbsChord, b: AbsChord): Boolean =
        val n = b.length
        val (ap, bp) = (normT(normOP(a)), normT(normOP(b)))
        val is = (0 to n).map { k => Seq.fill(k)(1) ++ Seq.fill(n-k)(0) }
        val s = is.map {i => o(i, bp)}.map { x => normT(normP(x)) }
        s.exists(_ == ap)
/*
OPTC-equivalence can be implemented similarly to OPT-equivalence.  
*/
    def optcEq(a: AbsChord, b: AbsChord): Boolean = optEq(normOPC(a), normOPC(b))
    
end OPTIC