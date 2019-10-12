
/*
Simple bossa nova implementation
Donya Quick
Translation to Scala by Bart Schuller

Load this file in GHCi and run "play m" to hear some music.
The lead sheet is finite, so the music will stop on its own.

This module is an example of a very simple, largely deterministic 
implementation of some bossa nova behavior using the JazzTypes framework. 
In this case, there is no use of State information in the bass and 
harmony, but the lead makes use of a very simplistic piece of state 
information (the last pitch played).
*/

import spire.math.Rat
import spire.implicits.given
import utils._
import euterpea.Music.{given, _}
import Music._
import euterpea.midi.MEvent._
import jazz.JazzTypes._
import jazz.Utils._
import SegCat._
import PartType._
import InstrumentName._
import euterpea.midi.ToSmidi._
import smidi._
import scala.language.implicitConversions

object SimpleBossa
/*
Utility function to cut a piece of music down to the duration 
of a segment:
*/
    def trimTo[A](seg: Segment[A], m: Music[A]): Music[A] =
        removeZeros(cut(seg.segDur / 4, remove(seg.segOnset._2 / 4, m)))
/*
Our state, which is only used by the soloing algorithm:
*/
    enum SimpleState
        case LastPitch(ap: AbsPitch)
        case NullState
    import SimpleState._
/*
Simple walking bass pattern following the bossa nova rhythm:
*/
    def bassFun[S]: PartFun[AbsPitch,S] = (s, seg1, seg2, hist, g) =>
        val p1 = 36 + seg1.chordCtxt.scale.head
        val p2 = p1 + 7
        val mPat = note(dqn, p1) :+: note(en, p2) :+: note(dqn, p2) :+: note(en, p1)
        val m = trimTo(seg1, forever(mPat))
        seg2 match
        case None => (g, s, note(seg1.segDur / 4, p1))
        case Some(_) => (g, s, m)
/*
Some simple chords following the bossa nova rhythm:
*/
    def chordFun[S]: PartFun[AbsPitch,S] = (s, seg1, seg2, hist, g) =>
        val ps = Seq(0, 2, 4, 6).map(i => seg1.chordCtxt.scale(i))
        def mkChord(d: Dur) = chord(ps.map(p => note(d, p+60)))
        val mPat = rest(qn) :+: mkChord(qn) :+: rest(en) :+: mkChord(en) :+: rest(qn)
        val m = trimTo(seg1, forever(mPat))
        seg2 match
            case None => (g, s, mkChord(seg1.segDur))
            case Some(_) => (g, s, m)
/*
Our solo pitch space:
*/
    val soloPSpace = 70 to 84
/*
The soloing algorithm does a random walk through the pitch space 
above. It uses the state to ensure smooth transitions across segment
boundaries.
*/
    def soloFun: PartFun[AbsPitch,SimpleState] = (s, seg1, seg2, hist, g) =>
        s match
        case NullState =>
            val (gp, p) = choose(g, soloPSpace)
            soloFun(LastPitch(p), seg1, seg2, hist, gp)
        case LastPitch(lp) =>
            val sPSpace = filterByScale(seg1.chordCtxt.scale)(soloPSpace)
            val n = (seg1.segDur * 2).round.toInt
            val (g1, g2) = g.split
            val ps = randMelody(g, sPSpace, lp).take(n)
            val mel = line(ps.map(p => note(en, p)))
            val lastP = pitches(mel).last
            seg2 match
                case None => (g2, LastPitch(ps.head), note(seg1.segDur, ps.head))
                case Some(_) => (g2, LastPitch(ps.last), mel)
    
    def randMelody(g0: StdGen, pSpace: PitchSpace, lastP: AbsPitch): LazyList[AbsPitch] =
        val nearPs = orderByNearest(pSpace, lastP).filter(_ != lastP)
        val (g1, p) = choose(g0, nearPs.take(5))
        p #:: randMelody(g1, pSpace, p)
/*
Putting it all together:
*/
    val myJB: JazzBand[AbsPitch, SimpleState] = LazyList(
        JazzPart(Bass, AcousticBass, bassFun, NullState),
        JazzPart(Bass, ElectricGrandPiano, chordFun, NullState),
        JazzPart(Bass, Marimba, soloFun, LastPitch(70)))
/*
Finally, we'll test it on a lead sheet.
*/
    val cM7 = ChordCtxt("CM7", Seq(0,2,4,5,7,9,11)) // C major
    val dm7 = ChordCtxt("DmM7", Seq(2,4,5,7,9,10, 0)) // D minor
    val g7 = ChordCtxt("G7", Seq(7,9,11,0,2,4,5)) // G mixolydian

    def seg1[A] = Segment[A](dm7, Regular, Nil, (0, Rat(0)), 4, TimeSig(4, 4))
    def seg2[A] = Segment[A](g7, Regular, Nil, (1, Rat(0)), 4, TimeSig(4, 4))
    def seg3[A] = Segment[A](cM7, Regular, Nil, (2, Rat(0)), 4, TimeSig(4, 4))
    def seg4[A] = Segment[A](cM7, Regular, Nil, (3, Rat(0)), 4, TimeSig(4, 4))
    def seg5[A] = Segment[A](dm7, Regular, Nil, (4, Rat(0)), 2, TimeSig(4, 4))
    def seg6[A] = Segment[A](g7, Regular, Nil, (4, Rat(2)), 2, TimeSig(4, 4))
    def seg7[A] = Segment[A](dm7, Regular, Nil, (5, Rat(0)), 2, TimeSig(4, 4))
    def seg8[A] = Segment[A](g7, Regular, Nil, (5, Rat(2)), 2, TimeSig(4, 4))
    def seg9[A] = Segment[A](cM7, Regular, Nil, (6, Rat(0)), 4, TimeSig(4, 4))
    def seg10[A] = Segment[A](g7, Regular, Nil, (7, Rat(0)), 4, TimeSig(4, 4))
    def seg11[A] = Segment[A](cM7, Regular, Nil, (8, Rat(0)), 4, TimeSig(4, 4))
    
    import scala.language.implicitConversions
    def ls[A]: LeadSheet[A] = LazyList(seg1, seg2, seg3, seg4, seg5, seg6, seg7, seg8, seg9, seg10, seg11)

    val m = runBand(myJB, LazyList.empty, ls, StdGen.mkStdGen(0))

    def main(args: Array[String]): Unit =
        val pf = perform(m)
        val sequence = toMidi(pf)
        MidiSystem.write(sequence, 1, "simplebossa.mid")
        val sequencer = MidiSystem.sequencer
        sequencer.setSequence(sequence)
        sequencer.open()
        sequencer.start()
        while (sequencer.isRunning)
        Thread.sleep(1000L)
        sequencer.stop()
        sequencer.close()
end SimpleBossa