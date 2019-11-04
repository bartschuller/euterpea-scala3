package kulitta
/*
Module for musical constraints
Donya Quick
Scala translation by Bart Schuller
*/
import QuotientSpaces._
import chordspaces.OPTIC._

object Constraints
    val satbOP: QSpace[AbsChord] = ???
    def noCPL(i: Double)(x: (AbsChord, AbsChord)): Boolean =
        ???
    //> noCPL i x = maxClass i x && hNotPar1 x && hNotCross x
end Constraints