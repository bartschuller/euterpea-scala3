package euterpea.midi
import smidi._
import spire.math.Rat
import spire.implicits.given
import MEvent._
import GeneralMidi._
import euterpea.Music.InstrumentName
import InstrumentName.Percussion
object ToSmidi
    type Channel = Int
    type ProgNum = Int
    type UserPatchMap = Map[InstrumentName, Channel]

    def makeGMMap(instruments: List[InstrumentName]): UserPatchMap =
        def mkGMMap(n: Int, ins: List[InstrumentName]): UserPatchMap =
            val chanList = (0 to 8) ++ (10 to 15)
            ins match
            case Nil => Map.empty
            case _ if n >= 15 => sys.error("makeGMMap: too many instruments.")
            case Percussion :: ins =>
                mkGMMap(n, ins) + (Percussion -> 9)
            case i :: ins =>
                mkGMMap(n+1, ins) + (i -> chanList(n))
        mkGMMap(0, instruments)

    def upmLookup(upm: UserPatchMap, iName: InstrumentName): (Channel, ProgNum) =
        val chan = upm.getOrElse(iName, sys.error(s"instrument $iName not in patch map"))
        (chan, toGM(iName))

    def toMidi(pf: Performance): Sequence = toMidiUPM(defUpm, pf)
    
    def toMidiUPM(upm: UserPatchMap, pf: Performance): Sequence =
        val split = splitByInst(pf)
        val insts = split.map(_._1)
        val rightMap =  if allValid(upm, insts) then upm
                        else makeGMMap(insts)
        val sequence = Sequence(Sequence.PPQ, division)
        split.foreach { ipf =>
            val track = sequence.createTrack
            mevsToMessages(rightMap, ipf).foreach { me =>
                track.add(me)
            }
        }
        sequence

    val division: Int = 96

    def allValid(upm: UserPatchMap, insts: List[InstrumentName]): Boolean =
        insts.forall(lookupB(upm))

    def lookupB(upm: UserPatchMap)(x: InstrumentName): Boolean =
        upm.isDefinedAt(x)

    def splitByInst(pf: List[MEvent]): List[(InstrumentName, Performance)] =
        if pf.isEmpty then Nil else
            val i = pf.head.eInst
            val (pf1, pf2) = pf.partition(_.eInst == i)
            (i, pf1) :: splitByInst(pf2)
        end if
    
    val defST = 500000

    def mevsToMessages(upm: UserPatchMap, ipf: (InstrumentName, Performance)): List[MidiEvent] =
        val (inm, pf) = ipf
        val (chan, progNum) = upmLookup(upm, inm)
        val setupInst = MidiEvent(ProgramChange(chan, progNum), 0)
        val setupTempo = MidiEvent(TempoChange(defST), 0)
        def loop(evs: Performance): List[MidiEvent] =
            evs match
            case Nil => Nil
            case e::es =>
                val (mev1, mev2) = mkMEvents(chan, e)
                mev1 :: mev2 :: loop(es)
        setupInst :: setupTempo :: loop(pf)

    def mkMEvents(mChan: Channel, mev: MEvent): (MidiEvent, MidiEvent) =
        val MEvent(t, _, p, d, v, _) = mev
        val vp = 0 max (127 min v)
        (MidiEvent(NoteOn(mChan, p, vp), toDelta(t)),
        MidiEvent(NoteOff(mChan, p, vp), toDelta(t+d)))
    
    def toDelta(t: Rat): Long = (t * Rat(2) * Rat(division)).round.toInt.toLong
    
    val defUpm: UserPatchMap =
        import InstrumentName._
        Map(
            AcousticGrandPiano ->1,
            Vibraphone -> 2,
            AcousticBass ->3,
            Flute -> 4,
            TenorSax -> 5,
            AcousticGuitarSteel -> 6,
            Viola -> 7,
            StringEnsemble1 -> 8,
            Percussion -> 9)
end ToSmidi