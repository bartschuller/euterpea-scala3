package kulitta
/*
Module for musical constraints
Donya Quick
Scala translation by Bart Schuller
*/
import QuotientSpaces._
import chordspaces.OPTIC._
import utils.{given, _}
object Constraints
/*
Musical constraints are modeled as predicates over some number of
chords. These are "hard" constraints such that a piece of music 
either does or does not satisfy the constraints. 

======== PREDICATES =========

First we defint the hProg function that turns a pairwise predicate 
into a progression predicate.

> hProg :: Predicate (a,a) -> Predicate [a]
> hProg f xs = and $ map f $ zip xs $ tail xs

> foldPreds :: [Predicate a] -> Predicate a
> foldPreds fs xs = and $ map ($xs) fs
*/
    def hProg[A](f: Predicate[(A,A)])(xs: Seq[A]): Boolean =
        ! xs.zip(xs.tail).map(f).exists(!_)
/*
Here we define the notion of voices not crossing to mean that if there 
is a permutation that sorts both chords, the voices do not cross. This 
allows for voices that may be in unison.

> hNotCross :: Predicate (AbsChord, AbsChord)
> hNotCross (c1,c2) = 
>     let sn = permutations [0..length c1-1]
>         ps1 = filter (\s -> p s c1 == sort c1) sn -- find permutation, p1, to sort c1
>         ps2 = filter (\s -> p s c2 == sort c2) sn -- find permutation, p2, to sort c2
>     in  not $ null [p | p<-ps1, elem p ps2]  
*/
    def hNotCross(c12: (AbsChord,AbsChord)): Boolean =
        val (c1, c2) = c12
        val sn = (0 until c1.length).permutations
        val ps1 = sn.filter{s => p(s, c1) == c1.sorted }
        val ps2 = sn.filter{s => p(s, c2) == c2.sorted }
        ps1.exists{p => ps2.contains(p)}
/*
Now we define a predicate for avoiding parallel motion (all voices 
moving in the same direction). We will consider parallel motion in 
the context of vectors, so this is not the strictest possible case.
We don't care about intervals of 0, since that does not constitute
voice movement.
*/
    def hNotPar1(c12: (AbsChord, AbsChord)): Boolean =
        val (c1, c2) = c12
        val diffs = c1.zipWith(c2)(subtract)
        !hasDups(diffs.filter(_ != 0))

    def hasDups[A](as: Seq[A]): Boolean =
        as.distinct.length < as.length
/*
Now some distance metric-type approaches.


> simpleDist, eucDist, maxDist :: DistMeasure
> simpleDist a b = fromIntegral $ sum $ map abs $ zipWith subtract a b
> eucDist a b = sqrt $ sum $ map fromIntegral $ zipWith subtract a b
> maxDist a b = fromIntegral $ maximum $ map abs $ zipWith subtract a b

> distClass :: DistMeasure -> Predicate Double -> Predicate (AbsChord, AbsChord)
> distClass d ft (x,y) = ft $ d x y

> simpleClass t = distClass simpleDist (<=t)
> eucClass t = distClass eucDist (<=t)
> maxClass t = distClass maxDist (<=t)
*/
    type DistMeasure = ((AbsChord, AbsChord)) => Double
    type Threshold = Double

    def simpleDist(a: AbsChord, b: AbsChord): Double =
        a.zipWith(b)(subtract).map(_.abs).sum.toDouble

    // this one doesn't look quite right, shouldn't we square the diffs?
    def eucDist(a: AbsChord, b: AbsChord): Double =
        math.sqrt(a.zipWith(b)(subtract).sum.toDouble)

    def maxDist(a: AbsChord, b: AbsChord): Double =
        a.zipWith(b)(subtract).map(_.abs).max.toDouble

    def distClass(d: DistMeasure, ft: Predicate[Double])(xy: (AbsChord, AbsChord)): Boolean =
        ft(d(xy))
    
    def maxClass(t: Double): Predicate[(AbsChord, AbsChord)] =
        distClass(maxDist, _ <= t)

    def noCPL(i: Double)(x: (AbsChord, AbsChord)): Boolean =
        maxClass(i)(x) && hNotPar1(x) && hNotCross(x)
/*
============================

ADDITIONAL THESIS PREDICATES

--- Single chords ---
*/
    def sorted(x: AbsChord): Boolean = x == x.sorted

    def spaced(lims: LazyList[(Int, Int)])(x: AbsChord): Boolean =
        lims.zipWith(
            x.zipWith(x.tail)(subtract)){
                case ((l, u), diff) => l <= diff && diff <= u
            }.find(!_).isEmpty
    
    val triads: Seq[AbsChord] =
        Seq(Seq(0,0,4,7), Seq(0,4,7,7), Seq(0,0,3,7), Seq(0,3,7,7), Seq(0,0,3,6))

    def doubled(templates: Seq[AbsChord])(x: AbsChord): Boolean =
        val allTriads = (0 to 11).flatMap { c => templates.map(x => normOP(t(c)(x))) }
        allTriads.exists(_ == normOP(x))
    
    def satbFilter(x: AbsChord): Boolean =
        Seq[Predicate[AbsChord]](sorted, spaced(satbLimits), doubled(triads)).map(f => f(x)).find(!_).isEmpty
    
    def satbFilter2(x: AbsChord): Boolean =
        Seq[Predicate[AbsChord]](sorted, spaced(satbLimits)).map(f => f(x)).find(!_).isEmpty
    
    val satbLimits = LazyList.continually((3, 12))

    val satbRanges = Seq((40,60), (47,67), (52,76), (60,81))

    val satbChords = makeRange(satbRanges).filter(satbFilter)

    val satbOP: QSpace[AbsChord] = satbChords \ opEq

    def satbR(g: StdGen, f: Predicate[AbsChord], r: EqRel[AbsChord]): QSpace[AbsChord] =
        randomize(g, makeRange(satbRanges).filter(f)) \ r

    def pianoChord(x: AbsChord): Boolean =
        x.length <= 5 && x.max - x.min <= 12
/*
--- Pairs ---
*/
    def voiceLeading(preds: Seq[Predicate[(PitchNum,PitchNum)]])(x: AbsChord, y: AbsChord) =
        preds.zipWith(x.zip(y))((pred, xy) => pred(xy)).find(!_).isEmpty

    def vl7: Predicate[(AbsChord, AbsChord)] =
        def f(a: Int, b: Int) = (a - b).abs <= 7
        voiceLeading(LazyList.continually(f))
end Constraints