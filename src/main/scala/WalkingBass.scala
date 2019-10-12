/*
Walking bass with ornaments
Donya Quick
Scala translation by Bart Schuller

The implementation here is much like in SimpleWalkingBass.lhs, 
but it adds ornaments between some pitches.
*/
import spire.math.Rat
import spire.implicits.given
import utils.{given, _}
import Random._
import jazz.JazzTypes._
import jazz.Utils._
import euterpea.Music.{given, _}
import Music._
import euterpea.midi.MEvent._
import SegCat._
import PartType._
import InstrumentName._
import euterpea.midi.ToSmidi._
import smidi._
import SimpleWalkingBass._

/*
First, the ornaments function. This works by taking a list of 
pitches, assumed to be one per beat, and adding ornaments. The
result is a Music (AbsPitch, Volume) value.
*/

object WalkingBass
    def bassToMusic(aps: List[AbsPitch], g: StdGen): Music[(AbsPitch, Volume)] =
        aps match
        case Nil => rest(0)
        case List(x) => note(wn, (x, 120))
        case x1::x2::xs =>
            val (r, g1) = randomR((0.0, 1.0), g)
            val m = if r < 0.85 then note(qn, (x1, 120)) else
                    if r < 0.90 then note(sn*3, (x1, 120)) :+: note(sn, (x2+1, 80)) else
                    if r < 0.95 then note(sn*3, (x1, 120)) :+: note(sn, (x1, 80))
                    else note(sn*3, (x1, 120)) :+: note(sn, (x2-1, 80))
            m :+: bassToMusic(x2::xs, g1)
/*
Now the PartFun, which is basically like the simple walkig bass function.
*/
    import WalkingState._
    def wBassFun2: PartFun[(AbsPitch, Volume), WalkingState] = (s, seg1, seg2opt, hist, g) =>
        (s, seg2opt) match
        case (NullState, seg2) =>
            val scale1 = seg1.chordCtxt.scale
            val pSpace = bassRange.filter(p => scale1.contains(p mod 12))
            val roots = pSpace.filter(p => (p mod 12) == scale1.head)
            val (g2, r) = choose(g, roots)
            // val beats = seg1.segDur.round.toInt
            wBassFun2(NextRoot(r), seg1, seg2, hist, g)
        case (NextRoot(r), None) =>
            (g, NullState, note(seg1.segDur, (r, 100)))
        case (NextRoot(r), Some(seg2)) =>
            val scale1 = seg1.chordCtxt.scale
            val scale2 = seg2.chordCtxt.scale
            val pSpace1 = bassRange.filter(p => scale1.contains(p mod 12))
            val pSpace2 = bassRange.filter(p => scale2.contains(p mod 12))
            val roots2 = pSpace2.filter(p => (p mod 12) == scale2.head)
            val (g1, nextR) = choose(g, roots2)
            val beats = (seg1.segDur*4).round.toInt
            val (g2, pitches) = walk(beats, pSpace1, r, nextR, g1)
            val (g3, g4) = g2.split
            val bassLine = bassToMusic(pitches, g3)
            (g4, NextRoot(nextR), cut(seg1.segDur/4, bassLine))
/*
We can also convert the SimpleWalkingBass's chords function to match 
the Music (AbsPitch, Volume) type.
*/
    def chordFun2[S]: PartFun[(AbsPitch, Volume), S] = (s, seg1, seg2, hist, g) =>
        val ps = Seq(0,2,4,6).map(i => seg1.chordCtxt.scale(i))
        val d = seg1.segDur / 4
        val m = chord(ps.map(p => note(d, (p+60, 70))))
        (g, s, m)

/*
Now we can put both into a band.
*/

    val myJB: JazzBand[(AbsPitch, Volume), WalkingState] = LazyList(
        JazzPart(Bass, AcousticBass, wBassFun2, NullState),
        JazzPart(Harmony, ElectricGrandPiano, chordFun2, NullState))

    val m = runBand(myJB, LazyList.empty, ls, StdGen.mkStdGen(0))

    def main(args: Array[String]): Unit =
        val pf = perform(cut(20, m))
        val sequence = toMidi(pf)
        MidiSystem.write(sequence, 1, "walkingbass.mid")
        val sequencer = MidiSystem.sequencer
        sequencer.setSequence(sequence)
        sequencer.open()
        sequencer.start()
        while (sequencer.isRunning)
        Thread.sleep(1000L)
        sequencer.stop()
        sequencer.close()
end WalkingBass