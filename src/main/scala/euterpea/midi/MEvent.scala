package euterpea.midi
import utils.{given, _}
import euterpea.Music.{given, _}
import Music._
import Control._
import Primitive._
import NoteAttribute._
import PhraseAttribute._
import Dynamic._

object MEvent
    case class MEvent (
        eTime: PTime,
        eInst: InstrumentName,
        ePitch: AbsPitch,
        eDur: DurT,
        eVol: Volume,
        eParams: Seq[Double]
    )

    type Performance = List[MEvent]

    type PTime = Rat
    type DurT = Rat

    def merge(p1: Performance, p2: Performance): Performance =
        (p1, p2) match
            case (Nil, _) => p2
            case (_, Nil) => p1
            case (e1::es1, e2::es2) => if e1.eTime < e2.eTime then e1 :: merge(es1, p2)
                                                              else e2 :: merge(p1, es2)

    case class MContext(
        mcTime: PTime,
        mcInst: InstrumentName,
        mcDur: DurT,
        mcVol: Volume
    )

    def perform[A: ToMusic1](m: Music[A]): Performance =
        perform1(ToMusic1[A].toMusic1(m))
    
    def perform1(m: Music1): Performance = perform1Dur(m)._1

    def perform1Dur(m: Music1): (Performance, DurT) =
        def metro(setting: Int, dur: Dur): DurT = Rat(60) / (Rat(setting) * dur)
        val defCon = MContext(
            mcTime = 0,
            mcInst = InstrumentName.AcousticGrandPiano,
            mcDur = metro(120, qn),
            mcVol = 127
        )
        musicToMEvents(defCon, applyControls(m))

    def applyControls(m: Music1): Music1 =
        m match
            case Modify(Tempo(r), m) => scaleDurations(r, applyControls(m))
            case Modify(Transpose(k), m) => shiftPitches1(k, applyControls(m))
            case Modify(x, m) => Modify(x, applyControls(m))
            case :+:(lm) => :+:(lm.map(applyControls))
            case :=:(m1, m2) => applyControls(m1) :=: applyControls(m2)
            case _ => m

    def musicToMEvents(c: MContext, m: Music1): (Performance, DurT) =
        (c, m) match
        case (MContext(t, _, dt, _), Prim(Note(d, p))) =>
            (List(noteToMEvent(c, d, p)), d*dt)
        case (MContext(t, _, dt, _), Prim(Rest(d))) =>
            (List(), d*dt)
        case (MContext(t, _, dt, _), :+:(m1 #:: lm2)) =>
            val (evs1, d1) = musicToMEvents(c, m1)
            val (evs2, d2) = musicToMEvents(c.copy(mcTime = t+d1), lm2.head)
            (evs1 ++ evs2, d1+d2)
        case (MContext(t, _, dt, _), :=:(m1, m2)) =>
            val (evs1, d1) = musicToMEvents(c, m1)
            val (evs2, d2) = musicToMEvents(c, m2)
            (merge(evs1, evs2), d1 max d2)
        case (c, Modify(Instrument(i), m)) => musicToMEvents(c.copy(mcInst = i), m)
        case (c, Modify(Phrase(pas), m)) => phraseToMEvents(c, pas, m)
        case (c, Modify(KeySig(_, _), m)) => musicToMEvents(c, m)
        case (c, Modify(Custom(_), m)) => musicToMEvents(c, m)
        case (c, Modify(_, _)) => musicToMEvents(c, applyControls(m))

    def noteToMEvent(c: MContext, d: Dur, pnas: (Pitch, Seq[NoteAttribute])): MEvent =
        val (p, nas) = pnas
        val e0 = MEvent(
            eTime = c.mcTime,
            ePitch = absPitch(p),
            eInst = c.mcInst,
            eDur = d*c.mcDur,
            eVol = c.mcVol,
            eParams = Nil
        )
        nas.foldRight(e0) { (na, ev) =>
            na match
                case VolumeAttr(v) => ev.copy(eVol = v)
                case Params(pms) => ev.copy(eParams = pms)
                case _ => ev
        }
    end noteToMEvent

    def phraseToMEvents(c: MContext, pas: List[PhraseAttribute], m: Music1): (Performance, DurT) =
        (c, pas) match
        case (c, Nil) => musicToMEvents(c, m)
        case (MContext(t, i, dt, _), pa::pas) =>
            lazy val pfd@(pf, dur) = phraseToMEvents(c, pas, m)
            def loud(x: Rat) = phraseToMEvents(c, Dyn(Loudness(x))::pas, m)
            def stretch(x: Rat) =
                val t0 = pf.head.eTime
                val r = x/dur
                def upd(e: MEvent): MEvent =
                    e match
                    case MEvent(t, _, _, d, _, _) =>
                        val dt = t - t0
                        val tp = (dt*r+1)*dt + t0
                        val dp = ((dt*2+d)*r+1)*d
                        e.copy(eTime=tp, eDur=dp)
                (pf.map(upd), dur)
            def inflate(x: Rat) =
                val t0 = pf.head.eTime
                val r = x/dur
                def upd(e: MEvent): MEvent =
                    e.copy(eVol = (((t-t0)*r + 1) * Rat(e.eVol)).round)
                (pf.map(upd), dur)
            import TempoAttr._
            import Articulation._
            pa match
            case Dyn(Accent(x)) =>
                (pf.map(e => e.copy(eVol = (Rat(e.eVol)*x).round)), dur)
            case Dyn(StdLoudness(l)) =>
                import StdLoudnessId._
                l match
                case PPP => loud(40)
                case PP => loud(50)
                case P => loud(60)
                case MP => loud(70)
                case SF => loud(80)
                case MF => loud(90)
                case NF => loud(100)
                case FF => loud(110)
                case FFF => loud(120)
            case Dyn(Loudness(x)) =>
                phraseToMEvents(c.copy(mcVol= x.round), pas, m)
            case Dyn(Crescendo(x)) => inflate(x)
            case Dyn(Diminuendo(x)) => inflate(-x)
            case Tmp(Ritardando(x)) => stretch(x)
            case Tmp(Accelerando(x)) => stretch(-x)
            case Art(Staccato(x)) =>
                (pf.map(e => e.copy(eDur = x*e.eDur)), dur)
            case Art(Legato(x)) =>
                (pf.map(e => e.copy(eDur = x*e.eDur)), dur)
            case Art(Slurred(x)) =>
                val lastStartTime = pf.foldRight(Rat(0))((e,t)=> e.eTime max t)
                def setDur(e: MEvent) = if e.eTime < lastStartTime
                                        then e.copy(eDur = x * e.eDur)
                                        else e
                (pf.map(setDur), dur)
            case Art(_) => pfd
            case Orn(_) => pfd
    end phraseToMEvents
end MEvent