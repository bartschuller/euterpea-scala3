import utils.{given _, _}
import Random._
import jazz.JazzTypes._
import jazz.Utils._
import euterpea.Music.{given _, _}
import euterpea.midi.MEvent._
import SegCat._
import PartType._
import InstrumentName._
import euterpea.midi.ToSmidi._
import smidi._

/*
Simple walking bass implementation
Donya Quick
Translation to Scala by Bart Schuller
*/
object SimpleWalkingBass:

    enum WalkingState:
        case NextRoot(p: AbsPitch)
        case NullState

/*
Taking a single step in the walking bass. Given a pitch space, the 
current pitch, and the destination pitch, we take a step between them
if possible. If they are too close together, we take a step nearby.
*/

    def makeStep(   pitchSpace: Seq[AbsPitch],
                    p1: AbsPitch,
                    p2: AbsPitch,
                    g: StdGen): (StdGen, AbsPitch) =
        val pH = p1 max p2
        val pL = p1 min p2
        val midPs = pitchSpace.filter(p=> p < pH && p > pL)
        val nearPs = pitchSpace.filter(p => p < pL+7 && p > pL-7 && p != pL && p != pH)
        val ps = if midPs.isEmpty then nearPs else midPs
        choose(g, ps)  
/*
The walk function iteratively applies makeStep to produce a walking bass
line spanning some number of beats. It takes the number of beats (i), 
a pitch space for the bass, and starting and ending pitches.
*/
    def walk(   i: Int,
                pSpace: Seq[AbsPitch],
                p1: AbsPitch,
                p2: AbsPitch,
                g: StdGen): (StdGen, List[AbsPitch]) =
        if i == 0 then (g, Nil) else
            val (g2, pMid) = makeStep(pSpace, p1, p2, g)
            val (g3, ps) = walk(i-1, pSpace, pMid, p2, g2)
            (g3, p1 :: ps)
/*
A pitch space for our bass:
*/
    val bassRange: Seq[AbsPitch] = 36 to 50
/*
The PartFun for the walking bass uses the walk function to fill the 
number of beats in the current segment (seg1). It also must choose the 
target root pitch for the next segment (seg2), which is ketp a part 
of the bass's state.
*/
    import WalkingState._

    def wBassFun: PartFun[AbsPitch, WalkingState] = (s, seg1, seg2opt, hist, g) =>
        (s, seg2opt) match
        case (NullState, seg2) =>
            val scale1 = seg1.chordCtxt.scale
            val pSpace = bassRange.filter(p => scale1.contains(p mod 12))
            val roots = pSpace.filter(p => (p mod 12) == scale1.head)
            val (g2, r) = choose(g, roots)
            // val beats = seg1.segDur.round.toInt
            wBassFun(NextRoot(r), seg1, seg2, hist, g)
        case (NextRoot(r), None) =>
            (g, NullState, note(seg1.segDur, r))
        case (NextRoot(r), Some(seg2)) =>
            val scale1 = seg1.chordCtxt.scale
            val scale2 = seg2.chordCtxt.scale
            val pSpace1 = bassRange.filter(p => scale1.contains(p mod 12))
            val pSpace2 = bassRange.filter(p => scale2.contains(p mod 12))
            val roots2 = pSpace2.filter(p => (p mod 12) == scale2.head)
            val (g1, nextR) = choose(g, roots2)
            val beats = (seg1.segDur*4).round
            val (g2, pitches) = walk(beats, pSpace1, r, nextR, g1)
            val bassLine = line(pitches.map(p => note(qn, p)))
            (g2, NextRoot(nextR), cut(seg1.segDur/4, bassLine))
/*
A very simple chord function that we can use along with our 
bassline (just so we can hear some chords):
*/
    def chordFun[S]: PartFun[AbsPitch, S] = (s, seg1, seg2, hist, g) =>
        val ps = Seq(0,2,4,6).map(i => seg1.chordCtxt.scale(i))
        val d = seg1.segDur / 4
        val m = chord(ps.map(p => note(d, p+60)))
        (g, s, m)

    
    val myJB: JazzBand[AbsPitch, WalkingState] = LazyList(
        JazzPart(Bass, AcousticBass, wBassFun, NullState),
        JazzPart(Bass, ElectricGrandPiano, chordFun, NullState))

    val cM7 = ChordCtxt("CM7", Seq(0,2,4,5,7,9,11)) // C major
    val dmM7 = ChordCtxt("DmM7", Seq(2,4,5,7,9,11, 0)) // Dorian
    val g7 = ChordCtxt("G7", Seq(7,9,11,0,2,4,5))

    def seg1[A] = Segment[A](dmM7, Regular, Nil, (0, Rat(0)), 4, TimeSig(4, 4))
    def seg2[A] = Segment[A](g7, Regular, Nil, (1, Rat(0)), 4, TimeSig(4, 4))
    def seg3[A] = Segment[A](cM7, Regular, Nil, (2, Rat(0)), 4, TimeSig(4, 4))
    def seg4[A] = Segment[A](cM7, Regular, Nil, (3, Rat(0)), 4, TimeSig(4, 4))

    import scala.language.implicitConversions
    def ls[A]: LeadSheet[A] = LazyList.continually(seg1 #:: seg2 #:: seg3 #:: LazyList(seg4)).flatten

    val m = runBand(myJB, LazyList.empty, ls, StdGen.mkStdGen(0))

    def main(args: Array[String]): Unit =
        val pf = perform(cut(10, m))
        val sequence = toMidi(pf)
        MidiSystem.write(sequence, 1, "simplewalkingbass.mid")
        val sequencer = MidiSystem.sequencer
        sequencer.setSequence(sequence)
        sequencer.open()
        sequencer.start()
        while (sequencer.isRunning)
        Thread.sleep(1000L)
        sequencer.stop()
        sequencer.close()
end SimpleWalkingBass