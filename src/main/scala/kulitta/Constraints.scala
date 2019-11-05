package kulitta
/*
Module for musical constraints
Donya Quick
Scala translation by Bart Schuller
*/
import QuotientSpaces._
import chordspaces.OPTIC._

object Constraints
    def noCPL(i: Double)(x: (AbsChord, AbsChord)): Boolean =
        ???
    //> noCPL i x = maxClass i x && hNotPar1 x && hNotCross x
/*
============================

ADDITIONAL THESIS PREDICATES

--- Single chords ---
*/
    def sorted(x: AbsChord): Boolean = x == x.sorted

    def spaced(lims: LazyList[(Int, Int)])(x: AbsChord): Boolean =
        ???
    
    val triads: Seq[AbsChord] =
        Seq(Seq(0,0,4,7), Seq(0,4,7,7), Seq(0,0,3,7), Seq(0,3,7,7), Seq(0,0,3,6))

    def doubled(templates: Seq[AbsChord])(x: AbsChord): Boolean =
        ???
    
    def satbFilter(x: AbsChord): Boolean =
        ???
    
    val satbLimits = LazyList.continually((3, 12))

    val satbRanges = Seq((40,60), (47,67), (52,76), (60,81))

    val satbChords = makeRange(satbRanges).filter(satbFilter)

    val satbOP: QSpace[AbsChord] = satbChords \ opEq

end Constraints