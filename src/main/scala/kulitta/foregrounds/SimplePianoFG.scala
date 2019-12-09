package kulitta
package foregrounds

import utils.{given, _}
import chordspaces.OPTIC._
import QuotientSpaces._
import PostProc._
import Search._
import Constraints._
import ClassicalFG._
import euterpea.Music._
import Music._

object SimplePianoFG
    def simplePianoFG1x(triads: List[TChord], g0: StdGen, k: Constraints): (Music[Pitch], Music[Pitch]) =
        val lr = (43, 60)
        val rr = (60,79)
        val rans= List(lr, lr, rr, rr)
        def pianoFilter(abcd: AbsChord): Boolean =
            val Seq(a,b,c,d) = abcd
            b-a <= 12 && d-c <=8
        val (g1, newChords) = classicalCS2x(pianoFilter, rans, g0, triads, k)
        val (g2, (lhM, rhM)) = classicalFGx(g1, newChords)
        (lhM, rhM)

    def classicalCS2x(  fFilter: Predicate[AbsChord], ranges: List[(AbsPitch, AbsPitch)], g: StdGen,
                        aChords: List[TChord], consts: Constraints): (StdGen, Seq[TChord]) =
        val justChords = aChords.map(_._3)
        val (g1, g2) = g.split
        val satbOPx = makeRangep(ranges).filter(fFilter) \ opcEq
        val (g3, eqs) = classBass(1.0, g2, justChords.map(eqClass(satbOPx, opcEq)))
        val csChords = greedyLet(noCPL(7), nearFall, consts, eqs, g3)
        val aChordsp = aChords.zipWith(csChords){ case ((a,b,c),d) => (a,b,d) }
        (g3, aChordsp)

    def classicalFGx(g: StdGen, aChordsp: Seq[TChord]): (StdGen, (Music[Pitch], Music[Pitch])) =
        val vs = toVoices(aChordsp)
        val (g1, csFG) = addFG(defConsts.copy(rootBassThreshC=1.0), g, Seq(vs(3)))
        val (g2, csFG2) = addFG(defConsts.copy(rootBassThreshC=1.0), g1, Seq(vs(0)))
        val rh = csFG ++ List(vs(2))
        val lh = List(vs(1)) ++ csFG2
        if vs.length == 4 then (g2, (vsToMusic(lh), vsToMusic(rh)))
        else sys.error("classicalFGx only works on chords with 4 voices")

    def simplePianoFGMelx(aChords: List[TChord], g0: StdGen, k: Constraints): (StdGen, (Music[Pitch], Music[Pitch])) =
        val (g1, lhPCs) = simpleLH(g0, aChords)
        val lhSpace = makeRange(Seq((40,59), (40,59))).filter{ case Seq(a,b) => b-a <= 12 && a <= b } \ opcEq
        val eqsL = lhPCs.map(eqClass(lhSpace, opcEq))
        val (g2, g3) = g1.split
        val lhChords = greedyLet[AbsChord](const(true), defFall, k, eqsL, g2)
        val lhTChords = aChords.zipWith(lhChords){ case ((k,d,_), x) => (k,d,x) }
        val (g4, lh) = lhFG(g3, lhTChords)
        //
        val (g5, rhPCs) = simpleRH(g4, aChords)
        val rhSpace = makeRange(Seq((60,80), (60,80))).filter(xs => xs.max - xs.min <= 9) \ opcEq
                        ++ (60 to 80).map(x => Seq(Seq(x)))
        val eqsR = rhPCs.map(eqClass(rhSpace, opEq))
        val (g6, g7) = g5.split
        val rhChords = greedyLet(smoothMel(4), nearestMel, k, eqsR, g6)
        val rhTChords = aChords.zipWith(rhChords){ case ((k,d,_), x) => (k,d,x) }
        val (g8, rh) = rhFG(g7, rhTChords)
        (g8, (lh, rh))

    def simpleLH(g: StdGen, xss: List[TChord]): (StdGen, List[AbsChord]) =
        xss match
        case Nil => (g, Nil)
        case List((k,d,Seq(r,t,f))) =>
            (g, List(Seq(r, f)))
        case (k,d,Seq(r,t,f)) :: xs =>
            val (gp, xp) = choose(g, Seq(Seq(r,t), Seq(r,f)))
            val (gpp, xsp) = simpleLH(gp, xs)
            (gpp, xp :: xsp)
        case _ => sys.error("simpleLH only works on 3 note combinations")
    
    def simpleRH(g: StdGen, xss: List[TChord]): (StdGen, List[AbsChord]) =
        xss match
        case Nil => (g, Nil)
        case List((k,d,Seq(r,t,f))) =>
            val (gp, xp) = choose(g, Seq(Seq(r), Seq(t)))
            (gp, List(xp))
        case (k,d,Seq(r,t,f)) :: xs =>
            val (gp, xp) = choose(g, Seq(Seq(r,t), Seq(t,f)))
            val (gpp, xsp) = simpleRH(gp, xs)
            (gpp, xp :: xsp)
        case _ => sys.error("simpleRH only works on 3 note combinations")

    def lhFG(g: StdGen, ts: Seq[TChord]): (StdGen, Music[Pitch]) =
        ts match
        case Nil => (g, rest(0))
        case Seq((k,d,c)) => (g, chord(c.map(p => note(d, pitch(p)))))
        case (k,d,c) +: t =>
            def pickPat(d: Dur, as: Seq[AbsPitch], g: StdGen) =
                as match
                case Seq(a1, a2) =>
                    val Seq(p1, p2) = as.map(pitch)
                    val (gp, x) = choose(g, Seq(
                        note(d/2, p1) :+: note(d/2, p2),
                        note(d/2, p2) :+: note(d/2, p1)
                    ))
                    (gp, x)
                case _ => sys.error("lhFG only works on 2 note combinations")
            val (gp, tp) = lhFG(g, t)
            val (gpp, x) = pickPat(d, c, gp)
            (gpp, x :+: tp)

    def rhFG(g: StdGen, chords: Seq[TChord]): (StdGen, Music[Pitch]) =
        def makeVoice(ts: Seq[TChord]): Voice =
            ts match
            case Nil => Nil
            case (k,d,Seq(x1)) +: t => (k,d,x1) +: makeVoice(t)
            case (k,d,Seq(x1,x2)) +: t => (k,d/2,x1) +: (k,d/2,x2) +: makeVoice(t)
            case x +: t => sys.error(s"Unsupported structure: $x")
        val v = makeVoice(chords)
        val (gp, vp) = makeMel(g, v)
        (gp, vsToMusic(Seq(vp)))

    def makeMel(g: StdGen, v: Voice): (StdGen, Voice) =
        v match
        case Nil => (g, Nil)
        case List(x) => (g, List(x))
        case (x1@((k1,m1),d1,p1)) :: (x2@((k2,m),d2,p2)) :: xs =>
            def makeMelNT(g: StdGen, v: Voice): (StdGen, Voice) =
                v match
                case Nil => (g, Nil)
                case List(x) => (g, List(x))
                case (x1@((k1,m1),d1,p1)) :: (x2@((k2,m),d2,p2)) :: xs =>
                    val (gp,pt) = pickNT(4)(g, x1, x2)
                    pt.fold( (gp, x1 :: x2 :: xs) )(v =>
                        (gp, ((k1,m1),d1/2,p1) :: ((k1,m1),d1/2,v) :: x2 :: xs)
                    )
            val (gp, pt) = pickPT(4)(g, x1, x2)
            val (gpp, rest) = makeMel(gp, x2 :: xs)
            pt.fold(makeMelNT(gpp, x1 :: rest))(v => (gpp, ((k1,m1),d1/2,p1) :: ((k1,m1),d1/2,v) :: rest))

    def smoothMel(thresh: AbsPitch)(c12: (AbsChord,AbsChord)): Boolean =
        c12 match
        case (Nil, c2) => sys.error("Empty chord")
        case (c1, Nil) => sys.error("Empty chord")
        case (c1, c2) => (c1.last - c2.head).abs < thresh
    
    def nearestMel(e: EqClass[AbsChord], g: StdGen, c: AbsChord): (StdGen, AbsChord) =
        if e.isEmpty then sys.error(s"Empty equivalence class for: $c") else
            val p = c.last
            val ps = e.map(_.head)
            val dists = ps.map(x => (p-x).abs)
            val minDist = dists.min
            val (gp, i) = choose(g, dists.zipWithIndex.filter(_._1 == minDist).map(_._2))
            (gp, e(i))
/*
==============================================

    Arpeggio-based pieces, playable on piano with pedal. They may not 
    always be best played with the indicated split of right and left hands.
    Sometimes the lowest note of the right-hand part may be better played
    by the left-hand, although it can difficult to automatically represent 
    on a score this way with software such as MuseScore.
*/
    def simplePianoFGArpx(aChords: List[TChord], g0: StdGen, k: Constraints): (StdGen, (Music[Pitch], Music[Pitch])) =
        val (g1, tcs) = classicalCS2xp(g0, aChords, k)
        val (tLH, tRH) = splitTChords(1, tcs)
        val rhM = toArpMusic(tRH)
        val lhM = vsToMusic(toVoices(tLH))
        (g1, (lhM, rhM))
/*
Another redoing of the chorale-inspired chord spaces.
*/
    def classicalCS2xp(g: StdGen, aChords: List[TChord], consts: Constraints): (StdGen, Seq[TChord]) =
        val satbRangesx = Seq((30,60), (47,67), (52,76), (60,81))
        val satbChordsx = makeRange(satbRangesx).filter(x => arpFilter(x) && satbFilter(x))
        val satbOPx = satbChordsx  \ opEq
        val justChords = aChords.map(_._3)
        val (g1, g2) = g.split
        val (g3, eqs) = classBass2(0.8, g2, justChords.map(eqClass(satbOPx, opcEq)))
        val csChords = greedyLet[AbsChord](myClass, nearFall, consts, eqs, g3)
        val aChordsp = aChords.zipWith(csChords) { case ((a,b,c),d) => (a,b,d) }
        (g3, aChordsp)

    def classBass2(thresh: Double, g: StdGen, ess: List[EqClass[AbsChord]]): (StdGen, List[EqClass[AbsChord]]) =
        ess match
        case Nil => (g, Nil)
        case List(e) => classBass(1.0, g, ess)
        case e :: es =>
            val (gp, ep) = classBass(thresh, g, List(e))
            val (gpp, esp) = classBass2(thresh, gp, es)
            (gpp, ep ++ esp)
/*
Constraint for appropriate spacings between voices (treated as 4-note chords,
not yet arpeggiated).
*/
    def arpFilter(ac: AbsChord): Boolean =
        def arpFilterSub(pss: AbsChord): Boolean =
            pss match
            case p1 :: p2 :: ps => p1-p2 <= 6 && arpFilterSub(p2 :: ps)
            case _ => true
        arpFilterSub(ac.tail)
    
    def myClass(c1: AbsChord, c2: AbsChord): Boolean =
        c1 != c2 && noCPL(7)(c1, c2)
/*
For splitting a TChord into lefthand and righthand sections by voice
count. For example, if amt = 1, a single voice will end up in the 
left hand portion.
*/
    def splitTChords(amt: Int, chords: Seq[TChord]): (Seq[TChord], Seq[TChord]) =
        def f(amt: Int)(km: Key, d: Dur, x: AbsChord) =
            ((km, d, x.take(amt)), (km, d, x.drop(amt)))
        chords.map(f(amt)).unzip
/*
Arpeggiate a bunch of TChords.
*/
    def toArpMusic(ts: Seq[TChord]): Music[Pitch] =
        ts match
        case Seq() => rest(0)
        case Seq((k,d,aps)) => chord(aps.map(p => note(d, pitch(p))))
        case x +: t => (rest(sn) :+: vsToMusic(Seq(toArp(x)))) :+: toArpMusic(t)

    def toArp(tc: TChord): List[TNote] =
        val (km@(k,m), d, aps) = tc
        def takeDur(dur: Dur, xss: LazyList[TNote]): List[TNote] =
            (dur, xss) match
            case (_, LazyList()) => Nil
            case (0, _) => Nil
            case (d, (h@(k, dp, x)) #:: t) =>
                if d >= dp then h :: takeDur(d-dp, t) else List((k,d,x))
        if d <= sn then List((km, d, aps.head)) else
            takeDur(d-sn, LazyList.continually(aps.map(x => (km, sn, x))).flatten)

end SimplePianoFG