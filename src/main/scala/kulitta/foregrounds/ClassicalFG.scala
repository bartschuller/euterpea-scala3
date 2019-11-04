package kulitta
package foregrounds
/*
Classical Foreground Module
Donya Quick
Scala translation by Bart Schuller
*/
import utils.{given, _}
import QuotientSpaces._
import chordspaces.OPTIC._
import PostProc._
import Search._
import Constraints._

object ClassicalFG
    case class CConstants(
        ntLimC: Int, // limit for neighboring tone distance
        ptLimC: Int, // limit for passing tone distance
        pHalfC: Double, // probability of dividing a note's duration in half (alternative is x-en and en)
        pTieC: Double, // probability of tying two identical notes
        rootBassThreshC: Double, // probability of enforcing that the bass be the root
        noCPLThreshC: Int // voice-leading maximum, setting to 0 forces nearest neighbor fallback
    )
// A set of default constants that work pretty well in most cases.
    val defConsts = CConstants(2, 3, 0.5, 0.5, 0.8, 7)
/*
Similarly, there are instances when we may want to use a classical chord space, but
not add a classical foreground. This can be useful for mixing styles.

> classicalCS :: StdGen -> [RChord] -> Constraints -> (StdGen, [TChord])
> classicalCS g rcs consts = 
>     classicalCS2 g (map toAbsChord rcs) consts 

> classicalCS2 :: StdGen -> [TChord] -> Constraints -> (StdGen, [TChord])
> classicalCS2 g aChords consts = 
>    let justChords = map (\(a,b,c) -> c) aChords
>        (g1,g2) = split g
>        (g3, eqs) = classBass 0.8 g2 $ map (eqClass satbOP opcEq) justChords
>        csChords = greedyLet (noCPL 7) nearFall consts eqs g3 
>        aChords' = zipWith (\(a,b,c) d -> (a,b,d)) aChords csChords
>    in  (g3, aChords')
*/
    def classicalCS2(g: StdGen, aChords: Seq[TChord], consts: Constraints): (StdGen, Seq[TChord]) =
        val justChords = aChords.map(_._3)
        val (g1, g2) = g.split
        val (g3, eqs) = classBass(0.8, g2, justChords.map(eqClass(satbOP, opcEq)))
        val csChords = greedyLet(noCPL(7), nearFall, consts, eqs, g3)
        val aChordsp = aChords.zipWith(csChords){ case ((a,b,c), d) => (a,b,d) }
        (g3, aChordsp)
/*
The classicalCS2 function uses a stochastic filter over equivalence classes.
This filter enforces that the bass holds the root with a certain probability 
(the "thresh" value). If the constraints can't be met, the bass is allowed 
to deviate from this rule for the sake of producing a result.

> classBass :: Double -> StdGen -> [EqClass AbsChord] -> (StdGen, [EqClass AbsChord])
> classBass thresh g [] = (g, [])
> classBass thresh g (e:es) = 
>     let (r,g') = randomR (0,1.0::Double) g
>         e' = if r > thresh then e else filter rootFilter e
>         e'' = if null e' then e else e'
>         (g'', es') = classBass thresh g es
>     in  (g'', e'':es') where
>     rootFilter :: Predicate AbsChord
>     rootFilter x = or $ map (opcEq x) [[0,0,4,7], [0,0,3,7], [0,0,3,6]]
*/
    def classBass(thresh: Double, g: StdGen, ess: Seq[EqClass[AbsChord]]): (StdGen, Seq[EqClass[AbsChord]]) =
        ???
end ClassicalFG