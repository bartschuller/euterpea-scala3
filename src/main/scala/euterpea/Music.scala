package euterpea
import spire.math.Rat
import spire.implicits.given
import scala.language.implicitConversions

object Music
    import scala.util.FromDigits
    given (given fdbi: FromDigits[BigInt]): FromDigits[Rat]
        def fromDigits(digits: String): Rat = Rat(fdbi.fromDigits(digits))

    type AbsPitch = Int
    type Octave = Int
    type Pitch = (PitchClass, Octave)
    type Dur = Rat
    enum PitchClass
        case Cff, Cf, C, Dff, Cs, Df, Css, D, Eff, Ds,
            Ef, Fff, Dss, E, Ff, Es, F, Gff, Ess, Fs,
            Gf, Fss, G, Aff, Gs, Af, Gss, A, Bff, As,
            Bf, Ass, B, Bs, Bss
    // deriving (Show, Eq, Ord, Read, Enum, Bounded)

    enum Primitive[A]
        case Note(dur: Dur, a: A)
        case Rest(dur: Dur)
        def pMap[B](f: Function1[A,B]): Primitive[B] =
            this match
            case Note(d, x) => Note(d, f(x))
            case Rest(d) => Rest(d)
    // deriving Show, Eq, Ord

    enum Music[A]
        def :=:(that: Music[A]): Music[A] = Music.:=:(this, that)
        case Prim(prim: Primitive[A])                 // primitive value
        case :+:(lm: LazyList[Music[A]])              // sequential composition
        case :=:(m1: Music[A], m2: Music[A])          // parallel composition
        case Modify(c: Control, m: Music[A])          // modifier
        def mMap[B](f: Function1[A,B]): Music[B] =
            this match
            case Prim(p) => Prim(p.pMap(f))
            case :+:(lm) => :+:(lm.map(_.mMap(f)))
            case :=:(m1, m2) => m1.mMap(f) :=: m2.mMap(f)
            case Modify(c, m) => Modify(c, m.mMap(f))
    object Music
        def (m1: Music[A]) :+:[A] (m2: => Music[A]): Music[A] = Music.:+:(m1 #:: LazyList(m2))

    enum Control
        case Tempo(r: Rat) // scale the tempo
        case Transpose(p: AbsPitch) // transposition
        case Instrument(i: InstrumentName) // instrument label
        case Phrase(pas: List[PhraseAttribute]) // phrase attributes
        case KeySig(pc: PitchClass, mode: Mode) // key signature and mode
        case Custom(name: String) // for user-specified controls

    enum Mode
        case Major, Minor,
             Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian
        case CustomMode(name: String)

    enum InstrumentName
        case AcousticGrandPiano, BrightAcousticPiano, ElectricGrandPiano,
            HonkyTonkPiano, RhodesPiano, ChorusedPiano,
            Harpsichord, Clavinet, Celesta,
            Glockenspiel, MusicBox, Vibraphone,
            Marimba, Xylophone, TubularBells,
            Dulcimer, HammondOrgan, PercussiveOrgan,
            RockOrgan, ChurchOrgan, ReedOrgan,
            Accordion, Harmonica, TangoAccordion,
            AcousticGuitarNylon, AcousticGuitarSteel, ElectricGuitarJazz,
            ElectricGuitarClean, ElectricGuitarMuted, OverdrivenGuitar,
            DistortionGuitar, GuitarHarmonics, AcousticBass,
            ElectricBassFingered, ElectricBassPicked, FretlessBass,
            SlapBass1, SlapBass2, SynthBass1,
            SynthBass2, Violin, Viola,
            Cello, Contrabass, TremoloStrings,
            PizzicatoStrings, OrchestralHarp, Timpani,
            StringEnsemble1, StringEnsemble2, SynthStrings1,
            SynthStrings2, ChoirAahs, VoiceOohs,
            SynthVoice, OrchestraHit, Trumpet,
            Trombone, Tuba, MutedTrumpet,
            FrenchHorn, BrassSection, SynthBrass1,
            SynthBrass2, SopranoSax, AltoSax,
            TenorSax, BaritoneSax, Oboe,
            Bassoon, EnglishHorn, Clarinet,
            Piccolo, Flute, Recorder,
            PanFlute, BlownBottle, Shakuhachi,
            Whistle, Ocarina, Lead1Square,
            Lead2Sawtooth, Lead3Calliope, Lead4Chiff,
            Lead5Charang, Lead6Voice, Lead7Fifths,
            Lead8BassLead, Pad1NewAge, Pad2Warm,
            Pad3Polysynth, Pad4Choir, Pad5Bowed,
            Pad6Metallic, Pad7Halo, Pad8Sweep,
            FX1Train, FX2Soundtrack, FX3Crystal,
            FX4Atmosphere, FX5Brightness, FX6Goblins,
            FX7Echoes, FX8SciFi, Sitar,
            Banjo, Shamisen, Koto,
            Kalimba, Bagpipe, Fiddle,
            Shanai, TinkleBell, Agogo,
            SteelDrums, Woodblock, TaikoDrum,
            MelodicDrum, SynthDrum, ReverseCymbal,
            GuitarFretNoise, BreathNoise, Seashore,
            BirdTweet, TelephoneRing, Helicopter,
            Applause, Gunshot, Percussion
        case CustomInstrument(name: String)

    enum PhraseAttribute
        case Dyn(dyn: Dynamic)
        case Tmp(tempo: TempoAttr)
        case Art(art: Articulation)
        case Orn(orn: Ornament)
    
    enum Dynamic
        case Accent(acc: Rat)
        case Crescendo(cres: Rat)
        case Diminuendo(dim: Rat)
        case StdLoudness(sl: StdLoudnessId)
        case Loudness(l: Rat)
    
    enum StdLoudnessId
        case PPP, PP, P, MP, SF, MF, NF, FF, FFF

    enum TempoAttr
        case Ritardando(rit: Rat)
        case Accelerando(acc: Rat)
    
    enum Articulation
        case Staccato(stac: Rat)
        case Legato(leg: Rat)
        case Slurred(slur: Rat)
        case Tenuto, Marcato, Pedal, Fermata, FermataDown, Breath,
             DownBow, UpBow, Harmonic, Pizzicato, LeftPizz,
             BartokPizz, Swell, Wedge, Thumb, Stopped
    
    enum Ornament
        case Trill, Mordent, InvMordent, DoubleMordent,
             Turn, TrilledTurn, ShortTrill,
             Arpeggio, ArpeggioUp, ArpeggioDown
        case Instruction(i: String)
        case Head(h: NoteHead)
        case DiatonicTrans(dt: Int)
    
    enum NoteHead
        case DiamondHead, SquareHead, XHead, TriangleHead,
             TremoloHead, SlashHead, ArtHarmonic, NoHead
    
    type Volume = Int

    def addVolume(v: Volume, m: Music[Pitch]): Music[(Pitch, Volume)] =
        m.mMap((_, v))
    
    enum NoteAttribute
        case VolumeAttr(v: Int)
        case Fingering(f: Int)
        case Dynamics(d: String)
        case Params(ps: Seq[Double])
    
    type Note1 = (Pitch, Seq[NoteAttribute])
    type Music1 = Music[Note1]

    /*
    A new type class to allow for musical polymorphism that ultimately
    must be converted to Music1 to be converted to MIDI format through
    the MEvent framework.
    */

    trait ToMusic1[A]
        type MA = Music[A]
        def toMusic1(m: MA): Music1

    object ToMusic1
        def apply[A](given ToMusic1[A]) = summon[ToMusic1[A]]

    given ToMusic1[Pitch]
        def toMusic1(m:MA) = m.mMap((_, Seq.empty))
    
    given toMusic1_Pitch_Volume: ToMusic1[(Pitch, Volume)]
        def toMusic1(m: MA) = m.mMap((p, v) => (p, Seq(NoteAttribute.VolumeAttr(v))))
    
    given ToMusic1[Note1]
        def toMusic1(m:MA) = m
    
    given ToMusic1[AbsPitch]
        def toMusic1(m:MA) = m.mMap(p => (pitch(p), Seq.empty))
    
    given toMusic1_AbsPitch_Volume: ToMusic1[(AbsPitch, Volume)]
        def toMusic1(m: MA) = m.mMap((p, v) => (pitch(p), Seq(NoteAttribute.VolumeAttr(v))))

    import Primitive._
    import Music._
    import Control._

    def note[A](d: Dur, p: A): Music[A] = Prim(Note(d, p))
    def rest[A](d: Dur): Music[A] = Prim(Rest(d))
    def tempo[A](r: Dur, m: Music[A]): Music[A] = Modify(Control.Tempo(r), m)
    def transpose[A](i: AbsPitch, m: Music[A]): Music[A] = Modify(Transpose(i), m)
    def instrument[A](i: InstrumentName, m: Music[A]): Music[A] = Modify(Instrument(i), m)

    val bn: Dur = 2
    val wn: Dur = 1
    val hn: Dur = wn/2
    val qn: Dur = wn/4
    val en: Dur = wn/8
    val sn: Dur = wn/16
    val tn: Dur = wn/32
    val sfn: Dur = wn/64
  
    val dwn: Dur = wn*3/2
    val dhn: Dur = wn*3/4
    val dqn: Dur = wn*3/8
    val den: Dur = wn*3/16

    def absPitch(p: Pitch): AbsPitch = p match
        case (pc, oct) => 12*(oct+1) + pcToInt(pc)
  
    import PitchClass._
    def pcToInt(pc: PitchClass): Int =
        pc match
        case Cff => -2
        case Cf => -1
        case C => 0
        case Dff => 0
        case Cs => 1
        case Df => 1
        case Css => 2
        case D => 2
        case Eff => 2
        case Ds => 3
        case Ef => 3
        case Fff => 3
        case Dss => 4
        case E => 4
        case Ff => 4
        case Es => 5
        case F => 5
        case Gff => 5
        case Ess => 6
        case Fs => 6
        case Gf => 6
        case Fss => 7
        case G => 7
        case Aff => 7
        case Gs => 8
        case Af => 8
        case Gss => 9
        case A => 9
        case Bff => 9
        case As => 10
        case Bf => 10
        case Ass => 11
        case B => 11
        case Bs => 12
        case Bss => 13
    end pcToInt

    import scala.language.implicitConversions
    import scala.math.Integral.Implicits._
    def pitch(ap: AbsPitch): Pitch =
        val (oct, n) = ap /% 12
        (IndexedSeq(C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B)(n), oct -1)

    def trans(i: Int)(p: Pitch): Pitch =
        pitch(absPitch(p)+i)

    def line[A](ms: Seq[Music[A]]): Music[A] =
        if ms.isEmpty then rest(0)
        else if ms.tail.isEmpty then ms.head
        else
            :+:(ms.head #:: LazyList(line(ms.tail)))

    def chord[A](ms: Seq[Music[A]]): Music[A] =
        ms.reduceRight((m1, m2) => :=:(m1,m2))

    def forever[A](m: Music[A]): Music[A] =
        m :+: forever(m)

    def dur[A](m: Music[A]): Dur =
        m match
        case Prim(Note(d, _)) => d
        case Prim(Rest(d)) => d
        case :+:(lm) => lm.foldLeft(Rat(0))((d, m2) => d + dur(m2))
        case :=:(m1, m2) => dur(m1) max (dur(m2))
        case Modify(Control.Tempo(r), m) => dur(m) / r
        case Modify(_, m) => dur(m)

    def cut[A](d: Dur, m: Music[A]): Music[A] =
        m match
        case _ if d <= 0 =>
            rest(0)
        case Prim(Note(oldD, p)) =>
            val d2 = (oldD min d) max 0
            if (d2 > 0) note(d2, p) else rest(0)
        case Prim(Rest(oldD)) =>
            rest((oldD min d) max 0)
        case :=:(m1, m2) =>
            cut(d, m1) :=: cut(d, m2)
        case :+:(m1 #:: m2 #:: _) =>
            val m1a = cut(d, m1)
            def m2a = cut(d - dur(m1a), m2)
            m1a :+: m2a
        case Modify(Control.Tempo(r), m) =>
            tempo(r, cut(d*r, m))
        case Modify(c, m) =>
            Modify(c, cut(d, m))
    end cut

    def remove[A](d: Dur, m: Music[A]): Music[A] =
        if d <= 0 then m
        else
            m match
            case Prim(Note(oldD, p)) =>
                val dp = (oldD - d) max 0
                if dp > 0 then note(dp, p) else rest(0)
            case Prim(Rest(oldD)) =>
                rest((oldD - d) max 0)
            case :=:(m1, m2) =>
                remove(d, m1) :=: remove(d, m2)
            case :+:(m1 #:: m2 #:: _) =>
                val mp1 = remove(d, m1)
                def mp2 = remove(d - dur(m1), m2)
                mp1 :+: mp2
            case Modify(Tempo(r), m) =>
                tempo(r, remove(d*r, m))
            case Modify(c, m) =>
                Modify(c, remove(d, m))
    end remove

    def removeZeros[A](m: Music[A]): Music[A] =
        m match
        case Prim(p) => Prim(p)
        case :+:(m1 #:: m2 #:: _) =>
            val mp1 = removeZeros(m1)
            val mp2 = removeZeros(m2)
            (mp1, mp2) match
            case (Prim(Note(0, _)), m) => m
            case (Prim(Rest(0)), m) => m
            case (m, Prim(Note(0, _))) => m
            case (m, Prim(Rest(0))) => m
            case (m1, m2) => m1 :+: m2
        case :=:(m1, m2) =>
            val mp1 = removeZeros(m1)
            val mp2 = removeZeros(m2)
            (mp1, mp2) match
            case (Prim(Note(0, _)), m) => m
            case (Prim(Rest(0)), m) => m
            case (m, Prim(Note(0, _))) => m
            case (m, Prim(Rest(0))) => m
            case (m1, m2) => m1 :=: m2
        case Modify(c, m) => Modify(c, removeZeros(m))
    end removeZeros

    def mFold[A,B]( f: Primitive[A] => B,
                    `+:`: (B, B) => B,
                    `=:`: (B, B) => B,
                    g: (Control, B) => B)
                   (m: Music[A]): B =
        val rec = mFold(f, `+:`, `=:`, g)
        m match
        case Prim(p) => f(p)
        case :+:(m1 #:: lm2) => `+:`(rec(m1), rec(lm2.head))
        case :=:(m1, m2) => `=:`(rec(m1), rec(m2))
        case Modify(c, m) => g(c, rec(m))
    
    /*
    Sometimes we may wish to alter the internal structure of a Music value
    rather than wrapping it with Modify. The following functions allow this.
    */

    def shiftPitches(ap: AbsPitch, m:Music[Pitch]): Music[Pitch] =
        m.mMap(trans(ap))

    def shiftPitches1[B](ap: AbsPitch, m:Music[(Pitch, B)]): Music[(Pitch, B)] =
        m.mMap((p, xs) => (trans(ap)(p), xs))

    def scaleDurations[A](r: Rat, m: Music[A]): Music[A] =
        m match
        case Prim(Note(d, p)) => note(d/r, p)
        case Prim(Rest(d)) => rest(d/r)
        case :+:(lm) => :+:(lm.map(m2 => scaleDurations(r, m2)))
        case :=:(m1, m2) => scaleDurations(r, m1) :=: scaleDurations(r, m2)
        case Modify(c, m) => Modify(c, scaleDurations(r, m))

    def changeInstrument[A](i: InstrumentName, m: Music[A]): Music[A] =
        Modify(Instrument(i), removeInstruments(m))
    
    def removeInstruments[A](m: Music[A]): Music[A] =
        m match
        case Modify(Instrument(_), m) => removeInstruments(m)
        case Modify(c, m) => Modify(c, removeInstruments(m))
        case :+:(m1 #:: lm2) => :+:(removeInstruments(m1) #:: lm2.map(removeInstruments))
        case :=:(m1, m2) => removeInstruments(m1) :=: removeInstruments(m2)
        case _ => m

end Music