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
import euterpea.Music.{Music,Pitch, rest, note, pitch}
import Music.:+:

/*
Simple Jazz Foreground Algorithms
Donya Quick
Scala translation by Bart Schuller
*/
object JazzFG
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
        (StdGen, Seq[(Key, Dur, AbsChord)]) =
        val LazyList(gJ, gOPC, gp) = splitN(g).take(3)
        val jts = chords.map(toJTriple)
        val ms = jts.map((a,b,c) => (Nil,c))
        val qJ = modeSpacep(alg1Temps)
        val chordsJ = greedyLet[JChord](const(true), nearFallJ, consts, ms.map(eqClass(qJ, modeEq)), gJ)
        val qOPC = makeRangep(alg1Rans) \ opcEq
        val es = chordsJ.map(convOPC(qOPC, bassRoot))
        val chordsOPC = greedyProgp[AbsChord](const(true), nearFall, gOPC, es)
        val chordsOPCp = jts.zipWith(chordsOPC)(newP)
        (gp, chordsOPCp)
/*
============================

Algorithm 1: chords and a stochastic bass. Let instantiation only takes 
place at the level of Roman numerals. 
*/
    val alg1Temps = Seq(Seq(0,2,4,6), Seq(0,1,2,4,6))

    val alg1Rans = (34,45) :: List.fill(7)((50,64))

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
        ???

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