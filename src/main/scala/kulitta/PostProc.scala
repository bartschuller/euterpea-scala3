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
import utils.{given _, _}
import euterpea.Music.{Mode => _, _}

object PostProc:
/*
Intermediate types:
(NOTE: AbsPitch = PitchNum)
*/
    type Key = (AbsPitch, Mode)
    type RChord = (Key, Dur, CType)
    type TChord = (Key, Dur, AbsChord)
    type TNote = (Key, Dur, AbsPitch)
    type Voice = List[TNote]
/*
Accessing the members of a TNote:
*/
    def (kdp: TNote) tnK: Key = kdp._1
    def (kdp: TNote) tnD: Dur = kdp._2
    def (kdp: TNote) tnP: AbsPitch = kdp._3
    def newP[A,B,C](kdp: (A,B,C), pp: C): (A,B,C) = kdp.copy(_3 = pp)
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
/*
Transposition using a key (to avoid C-major assignment only):
*/
    def atTrans(a: AbsPitch)(ts: List[TChord]): List[TChord] =
        ts.map{ case ((k,m),d,c) => (((k+a) mod 12, m), d, t(a)(c)) }
/*
The toCords functon does a similar thing, but returns a CType and 
its key/mode context without performing the conversion to AbsChord.
*/
    def ctTrans(a: AbsPitch)(ts: List[(Key, Dur, CType)]): List[(Key, Dur, CType)] =
        ts.map { case ((k, m), d, c) => (((k+a) mod 12, m), d, c) }
/*
============ SPLITTING VOICES APART ===========

The code here places TChords into a form more suitable
for additional musical processing. A Voice is a list of
pitches with duration and key/mode context.
*/
    def toVoices(ts: Seq[TChord]): Seq[Voice] =
        def checkMatrix(is: Seq[Seq[Int]]): Boolean =
            if is.isEmpty then true else
                val l = is.head.length
                !is.exists(_.length != l)
        val (ks, ds, ps) = ts.unzip3
        if checkMatrix(ps) then
            ps.transpose.map(v => zip3(ks, ds, v))
        else sys.error("(toVoices) chords must all have the same number of voices!")
    
    def toNotes(v: Voice): Music[Pitch] =
        def notep(d: Dur, p: AbsPitch): Music[Pitch] =
            if p < 0 then rest(d) else note(d, pitch(p))
        line(v.map((k,d,p) => notep(d, p)))
    
    def vsToMusic(vs: Seq[Voice]): Music[Pitch] =
        chord(vs.map(toNotes))

    def vsToMusicI(is: LazyList[InstrumentName], vs: Seq[Voice]): Music[Pitch] =
        chord(is.zipWith(vs.map(toNotes))((i,m) => instrument(i, m)))
end PostProc