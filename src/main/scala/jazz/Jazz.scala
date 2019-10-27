package jazz
import utils._
import euterpea.Music.{given, _}
import Music._
import spire.math.Rat

object JazzTypes

    /* A part name refers to a particular roll in improvisational jazz. We'll define
    four and allow an additional custom constructor for generality.
    */

    enum PartType
        case Solo, Harmony, Bass, Drums
        case PartType(name: String)
    // deriving Eq, Show, Ord

    /*
    A ChordCtxt, or chord context, is information given by notations like "C7"
    above a staff on a lead sheet. It has a string symbol that implies a particular
    scale. We will represent Scales as a list of pitch class numbers, or Ints.
    We will assume that the Scale's members are within the range [0,11] and that
    the list is organized in order of the scale's pitch class cycle. In other words,
    G-major would be [7,9,11,0,2,4,6].

    Note that we are not using Euterpea's PitchClass type here. This is to avoid
    issues of enharmonic equivalence, since Euterpea's PitchClass type has multiple
    constructors that map to the same numerical pitch class (for example, Cs for
    C sharp and Df for D flat).
    */

    type PCNum = Int
    type Scale = Seq[PCNum]
    case class ChordCtxt(sym: String, scale: Scale) // TODO: maybe include pitchSpace?

    /*
    A Segment is a portion of a lead sheet having a homogenous chord context.
    Segments may have fixed features for some parts, which typically chracterizes
    the lead sheet as a composition rather than as simply a chord progression.
    */

    enum SegStyle[+A]
        case Free
        case FixedPitch(piches: Seq[AbsPitch])
        case FixedMusic(music: Music[A])

    enum SegCat
        case Intro, Regular, Bridge, Ending, End
        case CustomSeg(name: String)

    type Measure = Int
    type Beat = Rat
    type Onset = (Measure, Beat)
    case class TimeSig(num: Int, denom: Int) // TimeSig 3 4 is 3/4, TimeSig 4 4 is 4/4, etc.

    case class Segment[A](
        chordCtxt: ChordCtxt,
        category: SegCat,
        styles: Seq[(PartType, SegStyle[A])],
        segOnset: Onset,
        segDur: Beat,
        timeSig: TimeSig
    )

    // Finally, a lead sheet is simply a list of segments.

    type LeadSheet[A] = LazyList[Segment[A]]

    /*
    A state is a collection of features that are tracked between generative iterations.
    This is left completely polymorphic. We denote is as the type variable s in the
    following definitions.

    On the performance side, a PartFun is a function from a part's State, the current
    Segment, the next Segment (if one exists), and what the band just played, to an
    updated State and newly emitted Music for the part.
    */

    type History[A] = LazyList[(PartType, Music[A])]
    type PartFun[A,S] = (S, Segment[A], Option[Segment[A]], History[A], StdGen) => (StdGen, S, Music[A])

    /*
    We will then define JazzPart to represent one performer in a group. It will
    have a PartType, an instrument, a PartFun defining its behavior, and a current
    State. A JazzPart is specific to a style an instrument.
    */

    case class JazzPart[A,S](
        partType: PartType,
        instr: InstrumentName,
        partFun: PartFun[A,S],
        state: S
    )

    // A JazzBand, then, is a simply a list of JazzParts.

    type JazzBand[A,S] = LazyList[JazzPart[A,S]]

    import scala.language.implicitConversions
    /*
    When we run the JazzBand, we need only supply a LeadSheet. Note that LeadSheet
    may potentially be infinite!
    */

    def runBand[A,S](jb: JazzBand[A,S], h: History[A], segs: LeadSheet[A], g: StdGen): Music[A] =
        (jb, segs) match
        case (Nil, _) => rest(0)
        case (_, LazyList()) => rest(0)
        case (_, seg1 #:: segs) =>
            val seg2 = segs.headOption
            def result = runSegment(jb, h, seg1, seg2, g)
            val (gs, states, ms) = result.unzip3
            val jb2 = states.lazyZip(jb).map((state, jp) => jp.copy(state=state))
            val h2 = jb.map(_.partType).zip(ms)
            ms.reduceRight(_ :=: _) :+: runBand(jb2, h2, segs, gs.last)
    end runBand

    def runSegment[A,S](jb: JazzBand[A,S],
                        h: History[A],
                        seg1: Segment[A],
                        seg2: Option[Segment[A]],
                        g: StdGen): LazyList[(StdGen, S, Music[A])] =
        jb match
        case Nil => LazyList.empty
        case jp #:: jps =>
            val (g2, st, m) = jp.partFun(jp.state, seg1, seg2, h, g)
            val m2 = instrument(jp.instr, m)
            
            (g2, st, m2) #:: runSegment(jps, h ,seg1, seg2, g2)
    end runSegment
end JazzTypes