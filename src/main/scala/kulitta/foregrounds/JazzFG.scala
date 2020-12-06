package kulitta
package foregrounds

import chordspaces.ModeSpace._
import chordspaces.OPTIC._
import grammars.MusicGrammars._
import Mode._
import PostProc._
import QuotientSpaces._
import Constraints._
import Search._
import utils.{given, _}
import Random._
import euterpea.Music.{Music,Pitch, rest, note, chord, pitch, instrument, InstrumentName, addVolume, Volume}
import Music.:+:
import InstrumentName._
import ClassicalFG._
import scala.language.experimental.genericNumberLiterals

/*
Simple Jazz Foreground Algorithms
Donya Quick
Scala translation by Bart Schuller
*/
object JazzFG:
/*
First, we need to find the modes for Roman numerals interpreted
in a particular key/mode. The type JTriple is actually a synonym 
for TChord, but it is used for clarity to indicate that the pitch 
information represents a mode rather than a chord.
*/
    def rotateModes(i: Int) = allModes.drop(i) ++ allModes.take(i)
    val majorModes = allModes
    val ionianModes = allModes
    val dorianModes = rotateModes(1)
    val phrygianModes = rotateModes(2)
    val lydianModes = rotateModes(3)
    val mixolydianModes = rotateModes(4)
    val minorModes = rotateModes(5)
    val aoleanModes = minorModes
    val locrianModes = rotateModes(6)

    def modeLookup(m: Mode): Seq[AbsMode] =
        m match
        case Major => majorModes
        case Dorian => dorianModes
        case Phrygian => phrygianModes
        case Lydian => lydianModes
        case Mixolydian => mixolydianModes
        case Minor => minorModes
        case Locrian => locrianModes
    
    def chordMode(ct: CType, km: Key): AbsMode =
        val (k, m) = km
        val pModes = modeLookup(m)
        val ctMode = pModes(Enum[CType].fromEnum(ct))
        val ck = pModes(0)(Enum[CType].fromEnum(ct))
        t(k+ck)(ctMode)

    def toJTriple(km: Key, d: Dur, c: CType): (Key, Dur, AbsMode) = (km, d, chordMode(c, km))
    def classicalCS2(g: StdGen, aChords: List[TChord], consts: Constraints): (StdGen, List[TChord]) = ???

    def jazzChords(g: StdGen, chords: List[(Key, Dur, CType)], consts: Constraints):
        (StdGen, List[(Key, Dur, AbsChord)]) =
        val LazyList(gJ, gOPC, gp) = splitN(g).take(3)
        val jts = chords.map(toJTriple)
        val ms = jts.map((a,b,c) => (Nil,c))
        val qJ = modeSpacep(alg1Temps)
        val chordsJ = greedyLet[JChord](const(true), nearFallJ, consts, ms.map(eqClass(qJ, modeEq)), gJ)
        val qOPC = makeRangep(alg1Rans) \ opcEq
        val es = chordsJ.map(convOPC(qOPC, bassRoot))
        val chordsOPC = greedyProgp[AbsChord](const(true), nearFall, gOPC, es)
        val chordsOPCp = jts.zipWith(chordsOPC)(newP)
        (gp, chordsOPCp.toList)
/*
============================

Algorithm 1: chords and a stochastic bass. Let instantiation only takes 
place at the level of Roman numerals. 
*/
    val alg1Temps = Seq(Seq(0,2,4,6), Seq(0,1,2,4,6))

    val alg1Rans = (34,45) :: List.fill(4)((50,64))

    def bassRoot(chrd: Seq[Int], m: AbsChord): Boolean = (chrd.min mod 12) == normO(m).head

    def jazzFG1(g: StdGen, chords: List[(Key,Dur,CType)], consts: Constraints): (StdGen, Music[Pitch]) =
        val LazyList(gJ, gR, gOPC, gB) = splitN(g).take(4)
        val jts = chords.map(toJTriple)
        val ms = jts.map((a,b,c) => (Nil, c))
        val qJ = modeSpacep(alg1Temps)
        val chordsJ = greedyLet[JChord](const(true), nearFallJ, consts, ms.map(eqClass(qJ, modeEq)), gJ)
        val qOPC = makeRangep(alg1Rans) \ opcEq
        val es = chordsJ.map(convOPC(qOPC, bassRoot))
        val chordsOPC = greedyProgp[AbsChord](const(true), nearFall, gOPC, es)
        val chordsOPCp = jts.zipWith(chordsOPC)(newP)
        val voices = toVoices(chordsOPCp)
        val (gRet, bassLine) = stochBass(gB, voices.head)
        (gRet, instrument(AcousticBass, bassLine) :=:
            vsToMusicI(LazyList.continually(AcousticGrandPiano), voices.tail))

    def convOPC(q: QSpace[AbsChord], pj: Predicate[JChord])(cm: JChord): EqClass[AbsChord] =
        val (c, m) = cm
        eqClass(q, opcEq)(c).filter(x => pj(x, m))

    def stochBass(g: StdGen, notes: Seq[TNote]): (StdGen, Music[Pitch]) =
        notes match
        case Seq() => (g, rest(0))
        case (km,d,p) +: t =>
            val (gp, pat) = pickPattern(g, d, p)
            val (gpp, tp) = stochBass(gp, t)
            (gpp, pat :+: tp)
    
    def pickPattern(g: StdGen, d: Dur, p: AbsPitch): (StdGen, Music[Pitch]) =
        def f(d: Dur, p: AbsPitch) = note(d, pitch(p))
        val pats = Seq(
            f(d,p),
            if d >= hn then f(qn, p) :+: f(d-qn, p) else f(d, p),
            if d >= hn then f(d-en, p) :+: f(en, p) else f(d, p)
        )
        val (r, gp) = randomR((0, pats.length - 1), g)
        (gp, pats(r))
/*
=============================

Algorithm 2: simple bossa nova

This approach interprets Roman numerals through three separate
chord spaces in order to cut down the task's combinatorics. 
*/
    val alg2TempsC = List(List(0,2,4,6), List(1,2,4,6)) // for chords
    val alg2TempsB = List(List(0,4)) // for bass
    val alg2TempsL = List(List(0), List(2), List(4)) // for lead

    val alg2RansB = List((34,49), (34,49))
    val alg2RansC = List.fill(4)((50,64))
    val alg2RansL = List((65,80))

    def bassRoot2(jc: JChord) = normO(jc._1) == normO(List(jc._2(0), jc._2(4)))

    def alg2FilterC(x: AbsChord) = sorted(x) && pianoChord(x)

    def jazzFG2(g: StdGen, chords: List[(Key, Dur, CType)], consts: Constraints): (StdGen, Music[(Pitch, Volume)]) =
        val gs = splitN(g).take(10)
        val LazyList(gJC, gJB, gJL, gRC, gRB, gRL, gOPC_C, gOPC_B, gOPC_L, gL) = gs
        val jts = chords.map(toJTriple)
        val ms = jts.map((_,_,c) => (Nil, c))
        val qs@Seq(qJC, qJB, qJL) = Seq(alg2TempsC, alg2TempsB, alg2TempsL).map(modeSpacep) // jazz spaces
        val Seq(chordsJ, bassJ, leadJ) = qs.zipWith(gs.take(3))((q, gx) => // random walk for chords
            greedyProg(q, modeEq, const(true), nearFallJ, gx, ms))
        val qOPC_C = makeRangep(alg2RansC).filter(alg2FilterC) \ opcEq
        val qOPC_B = makeRange(alg2RansB) \ opcEq
        val qOPC_L = makeRangep(alg2RansL) \ opcEq
        val esC = chordsJ.map(convOPC(qOPC_C, const(true))) // OPC equivalence classes for chords
        val esB = bassJ.map(convOPC(qOPC_B, bassRoot2))
        val esL = leadJ.map(convOPC(qOPC_L, const(true)))
        val chordsOPC = greedyLet[AbsChord](const(true), nearFall, consts, esC, gOPC_C)
        val bassOPC = greedyLet[AbsChord](noCPL(7), nearFall, consts, esB, gOPC_B)
        val leadOPC = greedyLet[AbsChord](noCPL(7), nearFall, consts, esL, gOPC_L)
        val Seq(cc, bc, lc) = Seq(chordsOPC, bassOPC, leadOPC).map(opc => jts.zipWith(opc)(newP))
        val cm = bossaChords(cc)
        val bm = bossaBass(bc)
        val (gRet, lm) = bossaLead(gL, lc)
        (gRet, chord(Seq(addVolume(127, instrument(AcousticBass, bm)),
                        addVolume(75, instrument(AcousticGrandPiano, cm)),
                        addVolume(127, instrument(Flute, lm)))))

    def bossaBass(ts: Seq[TChord]): Music[Pitch] =
        ts match
        case Seq() => rest(0)
        case (km, d, c@Seq(p1,p2)) +: t =>
            def f1(b1: Int, b2: Int) = f2(b1,b2) :+: f2(b1,b2)
            def f2(b1: Int, b2: Int) = f3(b1, qn+en) :+: f3(b2, en)
            def f3(b1: Int, d: Dur) = note(d, pitch(b1))
            if d > wn then bossaBass((km,wn,c) +: (km,d-wn,c) +: t) else
            if d == wn then f1(p1, p2) :+: bossaBass(t) else
            if d == hn then f2(p1, p2) :+: bossaBass(t) else f3(p1, d) :+: bossaBass(t)
        case _ => sys.error("(bossaBass) Bad input")

    def bossaChords(ts: Seq[TChord]): Music[Pitch] =
        ts match
        case Seq() => rest(0)
        case (km, d, c) +: t =>
            def f1(c: AbsChord) =
                val cp = f2(en, c)
                rest(qn) :+: cp :+: rest(qn) :+: cp :+: rest(qn)
            def f2(d: Dur, c: AbsChord) =
                chord(c.map(p => note(d, pitch(p))))
            if d > wn then bossaChords((km,wn,c) +: (km,d-wn,c) +: t) else
            if d == wn then f1(c) :+: bossaChords(t) else f2(d, c) :+: bossaChords(t)

    def bossaLead(g: StdGen, ts: Seq[TChord]): (StdGen, Music[Pitch]) =
        val jConsts = CConstants(2, 3, 0.3, 0.5, 0.8, 7)
        def foreFunsJ(c: CConstants) = List((0.5, f1(c)), (0.5, f2(c)))
        val v = toVoices(ts).head
        val (gp, vp) = addFgToVoice(jConsts, foreFunsJ(defConsts), g, v)
        (gp, vsToMusic(Seq(vp)))
/*
======================

Redefinition of nearest neighbor for modal chords:
*/
    def nearFallJ(e: EqClass[JChord], g: StdGen, xm: JChord): (StdGen, JChord) =
        val (x, m) = xm
        val ds = e.map(e1 => simpleDist(x, e1._1))
        import Ordering.Double.TotalOrdering
        val y = e(ds.indexOf(ds.min))
        (g, y)
//end JazzFG