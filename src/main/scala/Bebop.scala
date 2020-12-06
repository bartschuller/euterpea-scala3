/*
A Bebop Band
Donya Quick
Scala translation by Bart Schuller

This file demonstrates 2 things:
- Generating drums
- Converting between Music types within the generative system.

Part functions are given for drums, bass, chords (piano), and solo.
*/
import utils.{given, _}
import Rat.given
import Random._
import euterpea.Music.{given, _}
import Music._
import euterpea.midi.MEvent._
import jazz.JazzTypes._
import jazz.Utils._
import SegCat._
import InstrumentName._
import euterpea.midi.ToSmidi._
import smidi._
import scala.language.experimental.genericNumberLiterals

object Bebop:
/*
This implementation is going to use an combined state type that holds 
all the information any part would need. Each part will only use/access 
its own information.
*/
    case class BebopState(nextRoot: AbsPitch, lastSoloPitch: AbsPitch)

    val defState = BebopState(-1, -1) // -1 indicates a null value here

/* Drums */

    val hh: AbsPitch = 46 // open hi hat
    val hc: AbsPitch = 44 // closed hi hat
    val k: AbsPitch = 36 // kick
    val kv: Volume = 100 // kick volume
    val s: AbsPitch = 40 // snare
    val sv: Volume = 85 // snare volume
    val crash: AbsPitch = 49 // crash cymbal
/*
The following functions are from an older implementation that created
infinite Music values for jazz drums. We can reuse it here to make a PartFun.
The functions are simple stochastic choice over patterns for hi-hats, snare,
and kick. 

Volume is very important for these to sound reasonable, so we must use 
the Music (AbsPitch, Volume) type.
*/
    type AV = (AbsPitch, Volume)

    def genHiHats(g: StdGen): Music[AV] =
        val (v, g1) = randomR((0, 100), g)
        val (g2, x) = choose(g1, Seq(note(en, (hh,v)), note(en, (hh,v)), note(en, (hh,v)), note(en, (hc,v))))
        (if v<30 then rest(en) else x) :+: genHiHats(g2)

    def genSnare(g: StdGen): Music[AV] =
        val (g1, x) = choose (g, Seq(
            rest(qn) :+: note(en, (s, sv)) :+: rest (en + hn),
            rest(qn) :+: note(en, (s, sv)) :+: rest (en) :+: rest(qn) :+: note(en, (s, sv)) :+: rest (en),
            rest(dhn) :+: note(en, (s, sv)) :+: rest (en)))
        x :+: genSnare(g1)
    
    def genKick(g: StdGen): Music[AV] =
        val (g1, x) = choose(g, Seq(
            note(en, (k,kv)) :+: rest(dqn) :+: note(en, (k,kv)) :+: rest(dqn),
            note(en, (k,kv)) :+: rest(hn+en) :+: note(en, (k,kv)) :+: rest(en)))
        x :+: genKick(g1)
    
    def bebopDrumsFun[S]: PartFun[AV,S] = (s, seg1, seg2opt, hist, g) =>
        val d = seg1.segDur
        seg2opt match
        case None =>
            (g, s, note(d, (k, 120)) :=: note(d, (crash, 120)))
        case Some(seg2) =>
            val gs = splitN(g)
            val hats = genHiHats(gs(0))
            val snare = genSnare(gs(1))
            val kicks = genKick(gs(2))
            (gs(3), s, SimpleBossa.trimTo(seg1, hats :=: snare :=: kicks))
/*
    Chords

We'll create chords by placing the root, third, fifth, and 
seventh of the chord randomly within the pitch space. 

We'll start by defining some rhythms for the chords.
Negative values will intigate rests of that duration.
So, -en is an eighth rest, while en will indicate a 
note.
*/
    val chordRhythms = Seq(
        Seq(-en, qn, -en, -hn), 
        Seq(-en, en, -dqn, en, -qn),
        Seq(en,-dqn,en,-dqn), 
        Seq(en,-en,-hn,en,-en), 
        Seq(-qn, qn, -en, en,-qn))
/*
Now, a function for selecting pitches for a collection 
of pitch classes. They will be randomly assigned within 
the pitch space.
*/
    def choosePitches(g: StdGen, pcnums: List[PCNum], pSpace: PitchSpace): (StdGen, List[AbsPitch]) =
        pcnums match
        case Nil => (g, Nil)
        case pc :: pcs =>
            val (g1, p) = choose(g, pSpace.filter(p=>(p mod 12) == pc))
            val (g2, ps) = choosePitches(g1, pcs, pSpace)
            (g2, p :: ps)
/*
And finally the part function for generating chords.
*/
    def chordFunV[S]: PartFun[AV,S] = (s, seg1, seg2, hist, g) =>
        val pcs = List(0,2,4,6).map(i=> seg1.chordCtxt.scale(i))
        val (g1, ps) = choosePitches(g, pcs, 55 to 69)
        val measures = (seg1.segDur / 4).ceil
        val (g2, rhyths) = chooseN(g1, measures, chordRhythms)
        val rhyth = rhyths.flatten
        def nFun(d: Dur) = chord(ps.map(p=>note(d, (p, 80))))
        val m = line(rhyth.map(d=> if d < 0 then rest(-d) else nFun(d)))
        seg2 match
        case None => (g2, s, nFun(seg1.segDur/4))
        case Some(_) => (g2, s, SimpleBossa.trimTo(seg1, m))
/*
====== Walking Bass with Volume ======

Since the drums are using volume, we can lift the bass function
from SimpleWalkingBass into the new Music (AbsPitch,Volume) type.
*/
    import SimpleWalkingBass.WalkingState._
    def wBassFunV: PartFun[AV,BebopState] = (s, seg1, seg2, hist, g) =>
        val seg1p = segMap(seg1)(_._1)
        val seg2p = seg2.map(s=>segMap(s)(_._2))
        val histp = hist.map((pt,m) => (pt, m.mMap(_._1)))
        val sp = if s.nextRoot <= 0 then NullState
                 else NextRoot(s.nextRoot)
        val (gp, NextRoot(nextRP), m) = SimpleWalkingBass.wBassFun(sp, seg1p, seg2p, histp, g)
        (gp, s.copy(nextRoot=nextRP), m.mMap(p=>(p, 100)))
/*
===== Solo Algorithm with Volume =====

We're going to alter the soloPSpace function a bit more invasively,
so rather than lifting it as above we'll redefine it. Here, we 
use stochastic volumes and create a rest if a volume is below 
a certain level.
*/
    val soloPSpace = 70 to 84

    def soloFunV: PartFun[AV,BebopState] = (s, seg1, seg2, hist, g0) =>
        val sPSpace = filterByScale(seg1.chordCtxt.scale)(soloPSpace)
        val n = (seg1.segDur*2).ceil
        val (g1, lp) = if s.lastSoloPitch < 0 then choose(g0, sPSpace) else (g0, s.lastSoloPitch)
        val gs = splitN(g1)
        val ps = SimpleBossa.randMelody(gs(0), sPSpace, lp).take(n)
        val vols = randomRs((0,127), gs(1))
        def boostV(v: Volume) = 127 min (v + 40)
        def nFun(p: AbsPitch, v: Volume) = if v < 40 then rest(en) else note(en, (p, boostV(v)))
        val mel = line(ps.lazyZip(vols).map(nFun))
        val lastP = ps.last
        val sp = s.copy(lastSoloPitch = lastP)
        seg2 match
        case None => (gs(2), sp, note(seg1.segDur / 4, (ps.head, 100)))
        case Some(_) => (gs(2), sp, cut(seg1.segDur / 4, mel))
/*
Utility Functions
*/
    def segMap[A,B](segA: Segment[A])(f: A => B): Segment[B] =
        val Segment(ctxt, cat, styles, o, d, ts) = segA
        def fsty(f: A => B)(style: (PartType, SegStyle[A])): (PartType, SegStyle[B]) =
            import SegStyle._
            style match
            case (pt, FixedMusic(m)) => (pt, FixedMusic(m.mMap(f)))
            case (pt, Free) => (pt, Free)
            case (pt, FixedPitch(fp)) => (pt, FixedPitch(fp))
        val stylesp = styles.map(fsty(f))
        Segment(ctxt, cat, stylesp, o, d, ts)
    
    def splitN(g: StdGen): LazyList[StdGen] =
        val (g1, g2) = g.split
        g1 #:: splitN(g2)
/*
Jazz Band
*/
    import PartType._
    val myJB: JazzBand[AV, BebopState] = LazyList(
        JazzPart(Drums, Percussion, bebopDrumsFun, defState),
        JazzPart(Bass, AcousticBass, wBassFunV, defState),
        JazzPart(Bass, ElectricGrandPiano, chordFunV, defState),
        JazzPart(Bass, Marimba, soloFunV, defState))
/*
Random lead sheet of major/minor 7th chords
*/
    def randomLeadSheet(g0: StdGen, onset: Onset): LeadSheet[AV] =
        val (m, b) = onset
        val (g1, mode) = choose(g0, Seq("M", "m"))
        val (g2, root) = choose(g1, 0 to 11)
        val scale = (if mode=="M" then
                        Seq(0,2,4,5,7,9,11)
                    else
                        Seq(0,2,3,5,7,8,10)).map(p=> (p+root) mod 12)
        val pcStr = Seq("C", "Db", "D", "Eb", "E", "F", "F#", "G", "Ab", "A", "Bb", "B")(root)
        val chordStr = pcStr + mode + "7"
        val ctxt = ChordCtxt(chordStr, scale)
        Segment(ctxt, Regular, Nil, (m,b), 4, TimeSig(4, 4)) #:: randomLeadSheet(g2, (m+1, b))
    
    val ls = randomLeadSheet(StdGen.mkStdGen(600), (0, Rat(0)))

    val m = tempo(2, runBand(myJB, LazyList.empty, ls, StdGen.mkStdGen(700)))

    def main(args: Array[String]): Unit =
        val pf = perform(cut(20, m))
        val sequence = toMidi(pf)
        MidiSystem.write(sequence, 1, "bebop.mid")
        val sequencer = MidiSystem.sequencer
        sequencer.setSequence(sequence)
        sequencer.open()
        sequencer.start()
        while (sequencer.isRunning)
        Thread.sleep(1000L)
        sequencer.stop()
        sequencer.close()
end Bebop