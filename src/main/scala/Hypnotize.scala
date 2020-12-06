/*
Hypnotize: an inifinte, algorithmic jazz composition
Donya Quickan
Scala translation by Bart Schuller

"Hypnotize" is an algorithmic composition using the models 
in JazzTypes.lhs. It utilizes random lead sheet generation.
The music is slow and somewhat ambient.

The implementation here uses type Music (AbsPitch,Volume) to 
achieve more diverse textures (in Music AbsPitch, the volumes
are all constant). 
*/
import utils.{given, _}
import Random._
import euterpea.Music.{given, _}
import Music._
import Primitive._
import euterpea.midi.MEvent._
import jazz.JazzTypes._
import PartType.{PartType => _, _}
import jazz.Utils._
import SegCat._
import InstrumentName._
import euterpea.midi.ToSmidi._
import smidi._
import scala.language.experimental.genericNumberLiterals

object Hypnotize:
/*
State definitions. Only the bass is keeping track of state here.
A data type definition was used to show how state information can 
be extracted from more complex types in the future.
*/
    case class HypnoState(nextBassPitch: AbsPitch)

    val nullState = HypnoState(-1) // we use -1 to indicate no pitch
/*
Hypnotize has three parts: a celesta, a piano, and a bass. Both 
the celesta and piano are chord-based generative strategies, although
the celesta arpeggiates its chord over a longer time to give the 
effect of a melody. 
*/
    val celestaSpace = 60 until 85

    type AV = (AbsPitch, Volume)

    def celestaFun: PartFun[AV, HypnoState] = (inState, seg1, seg2, hist, g) =>
        val s = seg1.chordCtxt.scale
        val d = seg1.segDur
        val (g2, newChord) = pickChord(s, celestaSpace, g)
        val (g3, newChordp) = permute(g2, newChord)
        val (g4, x) = stagger(g3, d, newChordp)
        val m = removeZeros(cut(d, x))
        (g4, inState, trimTo(seg1, m))
/*
The bassFun is similar to a walking bass, but plays relatively few 
pitches per segment.
*/
    val bassRange = 36 to 50

    def bassFun:PartFun[AV, HypnoState] = (inState, seg1, seg2opt, hist, g0) =>
        val seg2 = seg2opt.get
        val nbr = inState.nextBassPitch
        val thisS = seg1.chordCtxt.scale
        val nextS = seg2.chordCtxt.scale
        val (g1, rbr) = choose(g0, bassRange.filter(p => (p mod 12) == thisS.head))
        val thisRoot = if nbr > 0 then nbr else rbr
        val (g2, nextRoot) = choose(g1, bassRange.filter(p => (p mod 12) == nextS.head))
        val fifthsA = bassRange.filter(p => (p mod 12) == thisS(4))
        val fifthsB = fifthsA.filter(p => p < (thisRoot max nextRoot) && p > (thisRoot min nextRoot))
        val (g3, thisFifth) = choose(g2, if fifthsB.isEmpty then fifthsA else fifthsB)
        val (r, g4) = randomR((0.0, 1.0), g3)
        val d = seg1.segDur
        val nrDown = if nextRoot-1 < 36 then nextRoot+1 else nextRoot-1
        val fDown = if thisFifth-1 < 36 then thisFifth+1 else thisFifth
        val pat1 = note(d-hn, (thisRoot, 100)) :+: note(hn, (thisFifth, 100))
        val pat2 = note(d-hn, (thisRoot, 100)) :+: note(hn, (thisRoot+1, 100))
        val pat3 = note(d-hn, (thisRoot, 100)) :+: note(hn, (nrDown, 100))
        val pat4 = note(d-hn, (thisRoot, 100)) :+: note(hn, (nextRoot+1, 100))
        val pat5 = note(d-hn, (thisRoot, 100)) :+: note(hn, (nrDown, 100))
        val pat6 = note(d-tn, (thisRoot, 100)) :+: note(tn, (nextRoot+1, 60))
        val pat7 = note(d-tn, (thisRoot, 100)) :+: note(tn, (nrDown, 60))
        val pat8 = note(d-hn-tn, (thisRoot, 100)) :+: note(tn, (fDown, 60)) :+: note(hn, (thisFifth, 100))
        val pat9 = note(d-hn-tn, (thisRoot, 100)) :+: note(tn, (fDown, 60)) :+: note(hn, (thisFifth, 100))
        val outState = inState.copy(nextBassPitch = nextRoot)
        val pats = if d > wn then Seq(pat1, pat2, pat3, pat4)
                    else Seq(pat1, pat2, pat3, pat4, pat5, pat6, pat7, pat8, pat9)
        val (g5, m) = choose(g4, pats)
        (g5, outState, trimTo(seg1, m))
/*
The chordFun function defines the behavior for the grand piano. It creates 
arpeggiated chords.
*/
    def chordFun:PartFun[AV, HypnoState] = (inState, seg1, seg2, hist, g) =>
        val s = seg1.chordCtxt.scale
        val (g1, pcs) = pickChordPCs(s, g)
        val d = seg1.segDur
        val (g2, pcsp) = permute(g1, pcs)
        val ps = pcsp.map(_ + 60)
        val mPat1 = chord(ps.zipWith(LazyList.from(0))((p, i) => rest(sn*Rat(i)) :+: note(d, (p, 80))))
        val mPat2 = rest(en) :+: mPat1
        val mPat3 = rest(den) :+: mPat1
        val (g3, mPat) = choose(g2, Seq(mPat1, mPat2, mPat3))
        val m = trimTo(seg1, mPat)
        (g3, inState, m)
/*
The randomLeadSheet function generates a randomized lead sheet
where there are contiguous groups of segements in the same 
randomly chosen key. Within each group, the chords are random
(at the Roman numeral level).
*/
    def randomLeadSheet[A](g0: StdGen): LazyList[Segment[A]] =
        val modes: Seq[Seq[PCNum]] =
            def modeRec(s: List[PCNum]): LazyList[Seq[PCNum]] =
                val (x::xs) = s
                s #:: modeRec(xs ++ List(x))
            modeRec(List(0,2,4,5,7,9,11)).take(7)
        def randomSegGroup(g0: StdGen, r: AbsPitch): LazyList[Segment[A]] =
            val (g1, i) = choose(g0, 0 to 5) // omitting locrian, because yuck
            val mo = modes(0)(i)
            val sp = modes(i).map(x => (x+mo+r) mod 12)
            val cctxt = ChordCtxt(sp.toString, sp)
            val (g2, d) = choose(g1, Seq(wn, wn, wn, wn, wn, wn, wn*2))
            val seg = Segment(cctxt, Regular, Seq.empty[(PartType, SegStyle[A])], (0,Rat(0)), d, TimeSig(4, 4))
            seg #:: randomSegGroup(g2, r)
        val (g1, n) = choose(g0, 1 to 4)
        val (g2, r) = choose(g1, 0 to 11)
        val (g3, g4) = g2.split
        val segs = randomSegGroup(g3, r).take(n)
        segs #::: randomLeadSheet(g4)
/*
Now we can generate a lead sheet with this function and 
run the jazz band on it.
*/
    val rls: LeadSheet[AV] = randomLeadSheet(StdGen.mkStdGen(6))

    val myJB: JazzBand[AV, HypnoState] = LazyList(
        JazzPart(Harmony, Celesta, celestaFun, nullState),
        JazzPart(Bass, AcousticBass, bassFun, nullState),
        JazzPart(Harmony, AcousticGrandPiano, chordFun, nullState))
    
    val hypnotize120bpm = runBand(myJB, LazyList.empty, rls, StdGen.mkStdGen(18))
    val hypnotize = tempo(Rat(6)/10, hypnotize120bpm)

    def main(args: Array[String]): Unit =
        val pf = perform(cut(20, hypnotize))
        val sequence = toMidi(pf)
        MidiSystem.write(sequence, 1, "hypnotize.mid")
        val sequencer = MidiSystem.sequencer
        sequencer.setSequence(sequence)
        sequencer.open()
        sequencer.start()
        while (sequencer.isRunning)
        Thread.sleep(1000L)
        sequencer.stop()
        sequencer.close()
/*
UTILITY FUNCTIONS
*/
/*
Trim a music value to the duration fo a segment.
*/
    def trimTo[A](seg: Segment[A], m: Music[A]): Music[A] =
        removeZeros(cut(seg.segDur, remove(seg.segOnset._2, m)))

        /* Permute a list of items. */
    def permute[A](g: StdGen, xs: Seq[A]): (StdGen, Seq[A]) =
        if xs.isEmpty then (g, Seq.empty) else
            val (g1, x) = choose(g, xs)
            val (g2, xsp) = permute(g1, xs.filter(_ != x))
            (g2, x +: xsp)
/*
Stagger/arpeggiate a chord with stochastic volumes and stochastic
addition of ornaments between notes.
*/
    def stagger(g: StdGen, td: Dur, ps: Seq[AbsPitch]): (StdGen, Music[AV]) =
        val (g1, g2) = g.split
        val vs = randoms[StdGen, Int](g1).map(v => 60 + (v mod 40))
        val pvs = ps.zip(vs)
        val (g3, ms) = randDursR(g2, td, pvs.toList)
        val (g4, msp) = ornaments(g3, ms)
        (g4, msp)
/*
Add ornaments to a list of musical values (used in stagger).
*/
    def ornaments(g: StdGen, ms: List[Music[AV]]): (StdGen, Music[AV]) =
        ms match
        case Nil => (g, rest(0))
        case List(x) => (g, x)
        case x1 :: x2 :: xs =>
            x2 match
            case Prim(Note(d, (p, v))) =>
                val (g1, r) = choose(g, Seq(true, false, false, false, false, false, false))
                val (g2, p2) = choose(g1, Seq(p-1, p+1))
                val v2 = 50
                val (g3, xsp) = ornaments(g2, xs)
                val newX =  if r && dur(x1) >= en
                            then chDur(-tn, x1) :+: note(tn, (p2, v2)) :+: x2
                            else x1 :+: x2
                (g3, newX :+: xsp)
            case _ =>
                val (g2, xsp) = ornaments(g, x2::xs)
                (g2, x1 :+: xsp)
    end ornaments

/* Add duration to a note or rest Music value. */
    def chDur[A](dp: Dur, m: Music[A]): Music[A] =
        m match
        case Prim(Note(d, x)) => note(d+dp, x)
        case Prim(Rest(d)) => rest(d+dp)
        case x => x
/*
Add random durations to a list of "a" types for music. These 
can be either pitches (AbsPitch) or pitch volume pairs.
*/
    def randDursR[A](g: StdGen, totalDur: Dur, xs: List[A]): (StdGen, List[Music[A]]) =
        def randDurs(g: StdGen, totalDur: Dur, as: List[A]): (StdGen, List[Music[A]]) =
            as match
            case Nil => (g, List(rest(0)))
            case List(x) => (g, List(note(totalDur, x)))
            case x::xs =>
                val durs = Seq(en, qn, dqn, hn).filter(_ <= totalDur - en * Rat(xs.length))
                val (g1, d) = choose(g, if durs.isEmpty then Seq(totalDur) else durs)
                val (g2, ms) = randDurs(g1, totalDur - d, xs)
                (g2, note(d, x) :: ms)
        end randDurs

        val durs = Seq(Rat(0), en, qn).filter(_ <= totalDur - en * Rat(xs.length))
        val (g1, rDur) = choose(g, durs)
        val (g2, m) = randDurs(g1, totalDur - rDur, xs)
        (g2, if rDur <= 0 then m else rest(rDur) :: m)
    end randDursR

/* Pick jazzy chord pitch classes from a scale. */
    def pickChordPCs(scale: Scale, g: StdGen): (StdGen, Seq[PCNum]) =
        val (g0, basePCInds) = choose(g, Seq(
            Seq(1,2,4,6),
            Seq(2,3,4),
            Seq(0,3,4),
            Seq(0,2,4,6)))
        val (g1, i) = choose(g, 1 to 5)
        (g1, (i +: basePCInds).map(ind => scale(ind)).sorted)

/* Another way of picking jazzy chord pitch classes. */
    def pickChordPCs2(scale: Scale, g: StdGen): (StdGen, Seq[PCNum]) =
        val (n, g1) = random[StdGen, Int](g)
        val np = 4 + (n mod 10)
        val (g2, inds) = chooseN(g1, np, Seq(0,0,0,1,2,2,3,4,4,4,5,5,6))
        val pcs = inds.map(ind => scale(ind))
        val (g3, pcsp) = permute(g2, pcs)
        (g3, pcsp)
/*
Find all combinations of pitches in a pitch space adhering to
a particular list of pitch classes.
*/
    def allPitchCombos(pSpace: PitchSpace, pcnums: Seq[PCNum]): Seq[Seq[AbsPitch]] =
        if pcnums.isEmpty then Seq(Seq.empty) else
            val (pc, pcs) = (pcnums.head, pcnums.tail)
            val xs =  pSpace.filter(x => (x mod 12) == pc)
            val ys = allPitchCombos(pSpace, pcs)
            for {
                x <- xs
                y <- ys
            } yield x +: y
/*
Given a scale and a pitch space, pick a chord of concrete pitches.
*/
    def pickChord(scale: Scale, pSpace: PitchSpace, g: StdGen): (StdGen, Seq[AbsPitch]) =
        val (g1, chordPCs) = pickChordPCs2(scale, g)
        val allPossibleChords = allPitchCombos(pSpace, chordPCs)
        choose(g, allPossibleChords)
end Hypnotize