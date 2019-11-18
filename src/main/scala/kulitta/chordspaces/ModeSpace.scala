package kulitta
package chordspaces
/*
Mode Space Implementation
Donya Quick
Scala translation by Bart Schuller
*/
import QuotientSpaces._
import OPTIC._

object ModeSpace
    type AbsMode = Seq[PitchNum]
    type JChord = (AbsChord, AbsMode)

    def modeEq(a: JChord, b: JChord): Boolean =
        normO(a._2) == normO(b._2)
/*
The set of all modes rooted at 0
*/
    val allModes: Seq[AbsMode] =
        def allRots(x: Seq[Int]) = LazyList.continually(doRot(x)).take(7)
        def doRot(x: Seq[Int]) = normO(normT(x.tail ++ Seq(x.head + 12)))
        allRots(Seq(0,2,4,5,7,9,11))

    val allKModes =
        for
            k <- 0 to 11
            m <- allModes
        yield normO(t(k)(m))

    val allJChords: Seq[JChord] =
        val masks = makeRange(Seq.fill(7)((0, 1)))
        def applyMask(f: AbsChord, m: AbsChord) = (f.zip(m).filter(_._1 > 0).map(_._2), m)
        for
            f <- masks
            m <- allKModes
        yield applyMask(f, m)

    val modeSpace: QSpace[JChord] = allJChords \ modeEq

    def modeSpacep(temps: Seq[Seq[Int]]): QSpace[JChord] =
        def toTemp(m: AbsMode)(t: Seq[Int]) = t.map(m(_))
        def f(temps: Seq[Seq[Int]])(c: AbsChord, m: AbsMode): Boolean =
            temps.map(toTemp(m)).contains(c)
        allJChords.filter(f(temps)) \ modeEq
end ModeSpace