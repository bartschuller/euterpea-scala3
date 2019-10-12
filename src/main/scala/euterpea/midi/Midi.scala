package euterpea.midi

import utils._
import scodec.bits.ByteVector
import Midi._
// import spire.algebra._
// import spire.math._
// import spire.implicits._
import scala.Numeric.Implicits._
import scala.language.implicitConversions

object Midi
    case class Midi(fileType: FileType, timeDiv: TimeDiv, tracks: List[Track[Ticks]])
    sealed trait FileType
    case object SingleTrack extends FileType
    case object MultiTrack extends FileType
    case object MultiPattern extends FileType
    type Track[A] = List[(A, Message)]
    sealed trait TimeDiv
    case class TicksPerBeat(ticks: Int) extends TimeDiv
    case class TicksPerSecond(framesPerSecond: Int, ticksPerFrame: Int) extends TimeDiv

    type Ticks = Int
    type Channel = Int
    type Key = Int
    type Velocity = Int
    type Pressure = Int
    type Preset = Int
    type Bank = Int
    type PitchWheelValue = Int
    type Tempo = Int
    enum Message
        case NoteOff(channel: Channel, key: Key, velocity: Velocity)
        case NoteOn(channel: Channel, key: Key, velocity: Velocity)
        case KeyPressure(channel: Channel, key: Key, pressure: Pressure)
        case ControlChange(channel: Channel, controllerNumber: Int, controllerValue: Int)
        case ProgramChange(channel: Channel, preset: Preset)
        case ChannelPressure(channel: Channel, pressure: Pressure)
        case PitchWheel(channel: Channel, pitchWheel: PitchWheelValue)
        // Meta Messages
        case SequenceNumber(sn: Int)
        case Text(t: String)
        case Copyright(c: String)
        case TrackName(tn: String)
        case InstrumentName(in: String)
        case Lyrics(l: String)
        case Marker(m: String)
        case CuePoint(cp: String)
        case ChannelPrefix(c: Channel)
        case ProgramName(pn: String)
        case DeviceName(dn: String)
        case TrackEnd
        case TempoChange(t: Tempo)
        case SMPTEOffset(h: Int, m: Int, s: Int, f: Int, sf: Int)
        case TimeSignature(num: Int, den: Int, cpc: Int, npb: Int)
        case KeySignature(root: Int, mode: Int)
        case Reserved(i: Int, data: ByteVector)
        // System Exclusive Messages
        case Sysex(w: Int, bv: ByteVector) // 0xF0 or 0xF7

    def fromAbsTime[A: Numeric](trk: Track[A]): Track[A] =
        val (ts, ms) = trk.unzip
        val (_, tsp) = mapAccumL(Numeric[A].zero, ts)((acc, t) => (t, t - acc))
        tsp.zip(ms)

end Midi