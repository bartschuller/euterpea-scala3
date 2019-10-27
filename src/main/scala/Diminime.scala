/*
Diminime
(c) 2019 Bart Schuller
*/
import spire.math.Rat
import spire.implicits.given
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
import scala.language.implicitConversions
import Hypnotize.{pickChordPCs, permute, trimTo}

object Diminime
    case class DiminiState()
    val nullState = DiminiState()
    type AV = (AbsPitch, Volume)

    def chordFun:PartFun[AV, DiminiState] = (inState, seg1, seg2, hist, g) =>
        val s = seg1.chordCtxt.scale
        val d = seg1.segDur
        val pcsp = Seq(0, 2, 4, 6).map(i =>s(i))
        val ps = pcsp.map(_ + 60)
        val mPat1 = chord(ps.zipWith(LazyList.from(0))((p, i) => rest(tn*Rat(i)) :+: note(d, (p, 80))))
        val plusRoot = note(d, (s(0)+60-24, 90)) :=: mPat1
        val m = trimTo(seg1, plusRoot)
        (g, inState, m)

    case class Chord(root: PCNum, mode: String)

    def followMajor(r: PCNum, g: StdGen): (StdGen, PCNum, String) =
        val relMinor = ((r-3) mod 12, "m")
        val parMinor = (r, "m")
        val upHalfMinor = ((r+1) mod 12, "m")
        val four = ((r+5) mod 12, "M")
        val fourToFive = ((r+2) mod 12, "M")
        val (g1, r7) = choose(g, 0 to 11)
        val random7 = (r7, "7")
        val (g2, (newR, newM)) = choose(g1, Seq(four, fourToFive, relMinor, parMinor, upHalfMinor, random7))
        (g2, newR, newM)

    def followMinor(r: PCNum, g: StdGen): (StdGen, PCNum, String) =
        val relMajor = ((r+3) mod 12, "M")
        val parMajor = (r, "M")
        val downHalfMajor = ((r-1) mod 12, "M")
        val (g1, r7) = choose(g, 0 to 11)
        val random7 = (r7, "7")
        val (g2, (newR, newM)) = choose(g1, Seq(relMajor, parMajor, downHalfMajor, random7))
        (g2, newR, newM)

    def follow7(r: PCNum, g: StdGen): (StdGen, PCNum, String) =
        val (g1, mode) = choose(g, Seq("M", "m", "7"))
        val one = ((r-7) mod 12, mode)
        val (g2, r7) = choose(g1, 0 to 11)
        val random7 = (r7, "7")
        val (g3, (newR, newM)) = choose(g1, Seq(one, one, one, random7))
        (g1, newR, newM)
    
    def randomLeadSheet(g0: StdGen, onset: Onset, prevC: Chord): LeadSheet[AV] =
        val (m, b) = onset
        val (g1, root, mode) = prevC.mode match
                                case "M" => followMajor(prevC.root, g0)
                                case "m" => followMinor(prevC.root, g0)
                                case "7" => follow7(prevC.root, g0)
        val scale = mode match
                    case "M" => Seq(0,2,4,5,7,9,11)
                    case "m" => Seq(0,2,3,5,7,8,10)
                    case "7" => Seq(0,2,4,5,7,9,10)
        val scaleInRoot = scale.map(p=> (p+root) mod 12)
        val pcStr = Seq("C", "Db", "D", "Eb", "E", "F", "F#", "G", "Ab", "A", "Bb", "B")(root)
        val chordStr = pcStr + mode + (if mode == "7" then "" else "7")
        val ctxt = ChordCtxt(chordStr, scaleInRoot)
        Segment(ctxt, Regular, Nil, (m,b), 4, TimeSig(4, 4)) #:: randomLeadSheet(g1, (m+1, b), Chord(root, mode))
    
    val cMajor = Chord(0, "M")
    val rls = randomLeadSheet(StdGen.mkStdGen(0), (0, Rat(0)), cMajor)

    val myJB: JazzBand[AV, DiminiState] = LazyList(
            JazzPart(Harmony, AcousticGrandPiano, chordFun, nullState))
    
    val diminime = runBand(myJB, LazyList.empty, rls, StdGen.mkStdGen(42))

    def main(args: Array[String]): Unit =
        val pf = perform(cut(20, tempo(Rat(8), diminime)))
        val sequence = toMidi(pf)
        MidiSystem.write(sequence, 1, "diminime.mid")
        val sequencer = MidiSystem.sequencer
        sequencer.setSequence(sequence)
        sequencer.open()
        sequencer.start()
        while (sequencer.isRunning)
        Thread.sleep(1000L)
        sequencer.stop()
        sequencer.close()
end Diminime