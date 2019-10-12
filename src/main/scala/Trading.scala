import spire.math.Rat
import spire.implicits.given
import utils._
import Random._
import jazz.JazzTypes._
import jazz.Utils._
import euterpea.Music.{given, _}
import euterpea.midi.MEvent._
import SegCat._
import PartType._
import InstrumentName._
import euterpea.midi.ToSmidi._
import smidi._

/*
Minimal Solo Trading Example
Donya Quick
Translation to Scala by Bart Schuller
*/

/*
This is an extremely simple example of how history can be
used to send information from one soloist to another. For
the sake of isolating just that behavior, the lead sheet
segments will all be assumed to be C-major. The system is
also stateless. Soloist A will play something and then
soloist B will respond by recombining fragments of what
it just heard.

The purpose of this example is NOT to make amazing music.
It is just to show how solo trading and history usage can
be modeled in an extremely basic way where the behavior
of each algorithm can is easy to assess by ear.

Soloist A is simply going to play one of three possible
patterns repeated for the length of the segment.
*/
object Trading
    val aPats: Seq[Seq[AbsPitch]] = Seq(
        Seq(60,62,64,65,67,69,71,72),
        Seq(60,64,67,64),
        Seq(60,62,64,65,67,65,64,62))

    def soloFunA[S]: PartFun[AbsPitch,S] = (s, seg1, seg2, hist, g) =>
        val (g2, pat) = choose(g, aPats)
        val m = line(LazyList.continually(pat).flatten.map(p=>note(en, p)).take(100))
        if seg1.category == CustomSeg("A") then
            (g2, s, cut(seg1.segDur / 4, m))
        else
            (g, s, rest(seg1.segDur / 4))

    /*
    Soloist B will do recombinance in chunks of size 4 on
    what it heard from Soloist A.
    */

    def recombine[A](g: StdGen, xs: => LazyList[A]): LazyList[A] =
        if xs.isEmpty
        then LazyList.empty
        else
            val (i, g2) = randomR((0, 0 max (xs.length - 1 - 4)), g)
            val chunk = xs.drop(i).take(4)
            import scala.language.implicitConversions
            chunk #::: LazyList(recombine(g2, xs)).flatten

    def getSoloHist[A](hs: History[A]): Music[A] =
        hs match
        case (Solo, m) #:: h => m
        case x #:: h => getSoloHist(h)
        case LazyList() => sys.error("No history!")
    
    def soloFunB[S]: PartFun[AbsPitch, S] = (s, seg1, seg2, hist, g) =>
        val (g2, g3) = g.split
        def histSolo = getSoloHist(hist)
        def ps = pitches(histSolo)
        def m = line(recombine(g2, ps).map(p=>note(en, p)))
        if seg1.category == CustomSeg("B")
        then (g3, s, cut(seg1.segDur / 4, m))
        else (g, s, rest(seg1.segDur / 4))

    val cM7 = ChordCtxt("CM7", Seq(0,2,4,5,7,9,11)) // C major
    val cSeg1 = Segment[AbsPitch](cM7, CustomSeg("A"), Nil, (1, Rat(0)), 4, TimeSig(4, 4))
    val cSeg2 = Segment[AbsPitch](cM7, CustomSeg("B"), Nil, (2, Rat(0)), 4, TimeSig(4, 4))
    import scala.language.implicitConversions
    val cLS: LeadSheet[AbsPitch] = LazyList.continually(cSeg1 #:: LazyList(cSeg2)).flatten.take(100)

    val myJB: JazzBand[AbsPitch, Unit] = LazyList(
        JazzPart(Solo, Marimba, soloFunA, ()),
        JazzPart(Solo, Vibraphone, soloFunB, ()))

    val m = runBand(myJB, LazyList.empty, cLS, StdGen.mkStdGen(0))

    def main(args: Array[String]): Unit =
        val pf = perform(cut(10, m))
        val sequence = toMidi(pf)
        MidiSystem.write(sequence, 1, "trading.mid")
        val sequencer = MidiSystem.sequencer
        sequencer.setSequence(sequence)
        sequencer.open()
        sequencer.start()
        while (sequencer.isRunning)
        Thread.sleep(1000L)
        sequencer.stop()
        sequencer.close()

end Trading