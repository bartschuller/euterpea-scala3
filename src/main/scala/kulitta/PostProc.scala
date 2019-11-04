package kulitta
/*
Post Processing Module to Link Grammar with OPTIC Functions
Donya Quick and Paul Hudak
Scala translation by Bart Schuller

Post processing module to turn Terms into music using Euterpea.
*/
import PTGG._
import grammars.MusicGrammars._
import chordspaces.OPTIC._

object PostProc
/*
Intermediate types:
(NOTE: AbsPitch = PitchNum)
*/
    type Key = (AbsPitch, Mode)
    type RChord = (Key, Dur, CType)
    type TChord = (Key, Dur, AbsChord)
    type TNote = (Key, Dur, AbsPitch)
    type Voice = Seq[TNote]
/*
The goal using these intermediate types is the following:

INPUT	     STEP            OUTPUT         FUNCTION
Seeds -----(grammar)-------> Sentence       gen
Sentence --(mode info)-----> [TChord]       toAbsChords
[TChord] ------------------> [Voice] 		toVoices
[Voice] -------------------> Music Pitch    vsToMusic or vsToMusicI
*/
    def unTerm[A](ts: List[Term[A,MP]]): List[(Key, Dur, A)] =
        toPairs(expand(Nil, ts)).map {(a, mp) => ((mp.key, mp.mode), mp.dur, a)}

    def toChords(ts: List[Term[CType,MP]]): List[RChord] =
        unTerm(ts)

    def toAbsChords(ts: List[Term[CType,MP]]): List[TChord] =
        toChords(ts).map(toAbsChord)

    def toAbsChord(rc: RChord): TChord =
        rc match
        case ((k,m),d,c) => ((k,m), d, t(k)(toAs(c, m)))
/*
Conversion of a single chord to a mode rooted at zero:

> toAs :: CType -> Mode -> [AbsPitch]
> toAs ct m = 
>     let s = getScale m ++ map (+12) s -- fininite scale
>         i = head $ findIndices (==ct) [I, II, III, IV, V, VI, VII] -- can be updated w/enum
>     in  map (s !!) $ map (+i) [0,2,4]
*/
    def toAs(ct: CType, m: Mode): Seq[AbsPitch] =
        val s = LazyList.iterate(getScale(m))(_.map(_+12)).flatten
        val i = ct.ordinal
        def fixDim(x: AbsChord): AbsChord = if optEq(x, Seq(0,3,6)) then t(x.head)(Seq(0,3,7)) else x
        fixDim(Seq(0,2,4).map(_+i)).map(s(_))
end PostProc