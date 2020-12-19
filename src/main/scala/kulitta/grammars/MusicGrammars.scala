package kulitta.grammars

/*
Musical Grammars
Donya Quick
Scala translation by Bart Schuller
*/

import utils.{given, _}
import kulitta.PTGG._
import Term._
import scala.language.experimental.genericNumberLiterals

object MusicGrammars:
/* ==================================
   TYPE SYNONYMS & CONSTANTS
*/
    type Dur = Rat
    type AbsPitch = Int

    val wn: Dur = 1
    val hn: Dur = wn/2
    val qn: Dur = wn/4
    val en: Dur = wn/8
    val sn: Dur = wn/16
    val tn: Dur = wn/32

/* ==================================
   ALPHABETS FOR BASE SYMBOLS

   Alphabet 1: Roman numerals for chords
*/
    enum CType:
        case I, II, III, IV, V, VI, VII
    object CType:
        def i[B](p:B) = NT(I, p)
        def ii[B](p:B) = NT(II, p)
        def iii[B](p:B) = NT(III, p)
        def iv[B](p:B) = NT(IV, p)
        def v[B](p:B) = NT(V, p)
        def vi[B](p:B) = NT(VI, p)
        def vii[B](p:B) = NT(VII, p)
    given Enum[CType] with
        def fromEnum(c: CType): Int = c.ordinal
        def toEnum(i: Int): CType = CType.values(i)
        def enumFrom(c: CType): Seq[CType] =
            CType.values.toList.dropWhile(_ != c)

/* Alphabet 1: from Rohrmeier's paper "Towards a generative syntax of tonal harmony" */

    enum RTerm:
        case Piece, P // piece/phrase (or P=Plagal for Kulitta's reuse of P)
        case TR, DR, SR // regions
        case T, D, S, TP, TCP, SP, DP // chord functions
        case C(ctype: CType) // Roman numerals

/* ==================================
   ALPHABETS FOR PARAMETERS

Many finite base symbol alphabets can use the same potentially infinite
alphabet of parameter symbols. Here we define a general "music parameter"
or MP for many tonal applications. It will store the current duration 
of a symbol, and the symbol's tonal context as a mode and scale root.
Finally, there is allowance for keeping track of the onset of the symbol
as well as the total duration of the sentence to which it belongs. This
allows for checking things like whether the symbols is the LAST in a 
sentence, at the midpoint, etc.
*/
    import Mode._
    case class MP(dur: Dur, mode: Mode, key: Int, onset: Dur, sDur: Dur):
        // It is also useful to have tests for MP values and modifiers for them.
        def isMaj: Boolean = mode == Major
        def isMin: Boolean = mode == Minor
/*
Modifiers on duration can be used to succinctly write transformations.
For example, to halve the duration of a parameter p::MP, one need only
write (h p) rather than something like p{dur=(dur p)/2}
*/
        def dFac(x: Rat): MP = copy(dur = dur * x)
        def h = dFac(Rat(1,2))
        def q = dFac(Rat(1,4))
        def e = dFac(Rat(1,8))
/*
Similarly, we have some shorthands for adjusting the onsets and 
durations at the same time. NOTE: offsets should be changed 
before the duration is changed.
*/
        def q2 = copy(onset = onset + dur / 4) // "beat" 2 (second quarter)
        def q3 = copy(onset = onset + dur / 2) // "beat" 3 (third quarter)
        def q4 = copy(onset = onset + dur * 3 / 4) // "beat" 4 (fourth quarter)
/* We can also do shorthands that do both things. */
        def ho = q3.h
        def qo2 = q2.q
        def qo3 = q3.q
        def qo4 = q4.q
/*
The following alter Rules to do a duration test. Each has a 
"rejection condition" that will be the condition for an ID rule.

The rejection condition in this case tests the left-hand-side
symbol's duration.
*/
    def toRelDur[A](f: Dur => Boolean, r: Rule[A,MP]): Rule[A,MP] =
        Rule(r.lhs, r.prob, p => if f(p.dur) then List(NT(r.lhs, p)) else r.rfun(p))
/*
This one is pickier - it will check whether applying the rule will
produce symbols that satisfy the rejection condition. So, if there
is ANY bad symbol on the right-hand-side, an ID rule will be applied
instead.
*/
    def toRelDur2[A](f: Dur => Boolean)(r: Rule[A,MP]): Rule[A,MP] =
        Rule(r.lhs, r.prob, p => {
            val xs = toPairs(expand(Nil, r.rfun(p))).map(_._2.dur)
            if xs.map(f).exists(_ == true) then List(NT(r.lhs, p)) else r.rfun(p)
        })
/*
For mode/key changes:

                       C      D      E      F      G      A      B
*/
    val majModes = Seq(Major, Minor, Minor, Major, Major, Minor, Minor)
    val minModes = Seq(Minor, Minor, Major, Minor, Minor, Major, Major)

    val majScale = Seq(0,2,4,5,7,9,11)
    val minScale = Seq(0,2,3,5,7,8,10)
    val dorScale = Seq(0, 2, 3, 5, 7, 9, 10)
    val phrScale = Seq(0, 1, 3, 5, 7, 8, 10)
    val lydScale = Seq(0, 2, 4, 6, 7, 9, 11)
    val mixScale = Seq(0, 2, 4, 5, 7, 9, 10)
    val locScale = Seq(0, 1, 3, 5, 6, 8, 10)

    def getScale(m: Mode): Seq[AbsPitch] =
        m match
        case Major => majScale
        case Minor => minScale
        case Dorian => dorScale
        case Phrygian => phrScale
        case Lydian => lydScale
        case Mixolydian => mixScale
        case Locrian => locScale
        case m => sys.error(s"(getScale) Scale not defined for mode $m")
    
    def modMajMin(i: Int)(p: MP): MP =
        val k0 = p.key
        if p.mode == Major then p.copy(mode = majModes(i), key = (k0 + majScale(i)) mod 12)
        else p.copy(mode = minModes(i), key = (k0 + minScale(i)) mod 12)
/*
Basic modulations on scale degrees for Major and Minor systems
*/
    val Seq(m2, m3, m4, m5, m6, m7) = (1 to 6).map(modMajMin)
/*
Modes include the seven usual derivatives of the C-major scale along with
chromatic and custom options. Note that Major=Ionian and Minor=Aeoloean.
*/
    enum Mode:
        case Major, Dorian, Phrygian, Lydian, Mixolydian, Minor, Locrian, Chromatic
        case Custom(aps: Seq[AbsPitch])
/*
A partial Enum instance is supplied for the modes with seven-note scales.
The enumFrom function is defined to loop around. For example:

    enumFrom Dorian ==> [Dorian, Phrygian, ..., Locrian, Major]
*/
    val allEnumModes = Seq(Major, Dorian, Phrygian, Lydian, Mixolydian, Minor, Locrian)

    given Enum[Mode] with
        def toEnum(i: Int): Mode = if i >= 0 && i <= 6 then allEnumModes(i)
                                   else sys.error("Only modes 0-6 are enumerable.")
        def fromEnum(a: Mode): Int = a match
            case Chromatic => sys.error("Chromatic mode is not part of Enum instance")
            case Custom(_) => sys.error("Cannot enumerate a Custom mode")
            case _ =>
                val i = allEnumModes.indexOf(a)
                if i >= 0 then i
                else sys.error(s"Cannot enumerate unknknown mode $a")
        def enumFrom(a: Mode): Seq[Mode] = ???
/*
A default MP value is one measure long (in 4/4) in the key of C-major.
*/
    val defMP = MP(1, Major, 0, 0, 1)
/*
Grammar from dissertation chapter 4, table 4.2 with optional let 
statements added.
*/
    import CType._
    def rRules1(minDur: Dur, useLets: Boolean): Seq[Rule[CType, MP]] =
        def letRules = Enum[CType].enumFrom(I).flatMap(ct => Seq(letRule1(ct), letRule2(ct)))
        def letRule1(ct: CType) = Rule[CType,MP](ct, 0.1, p =>
            List(Let("x", List(NT(ct, p.h)), List(Var("x"), Var("x")))))
        def letRule2(ct: CType) = Rule[CType,MP](ct, 0.1, p =>
            List(Let("x", List(NT(ct, p.q)), List(Var("x"), v(p.h), Var("x")))))
        def rawRules = List[Rule[CType,MP]](
            // -- Rules for I --
            Rule(I, 0.187, p => List(if p.isMaj then ii(p.q) else iv(p.q), v(p.q), i(p.h))),
            Rule(I, 0.187, p => List(i(p.q), iv(p.q), v(p.q), i(p.q))),
            Rule(I, 0.187, p => List(v(p.h), i(p.h))),
            Rule(I, 0.187, p => List(i(p.q), if p.isMaj then ii(p.q) else iv(p.q), v(p.q), i(p.q))),
            Rule(I, 0.252, p => List(i(p))),
            // -- Rules for II --
            Rule(II, 0.40, p => List(if p.isMaj then ii(p) else iv(p))),
            Rule(II, 0.40, p => List(if p.isMaj then (if p.dur > qn then ii(p) else i(m2(p))) else ii(p))),
            Rule(II, 0.20, p => if p.isMaj then List(vi(p.h), ii(p.h)) else List(vi(p.h), iv(p.h))),
            // -- Rules for III --
            Rule(III, 0.90, p => List(iii(p))),
            Rule(III, 0.10, p => List(i(m3(p)))),
            // -- Rules for IV --
            Rule(IV, 0.90, p => List(iv(p))),
            Rule(IV, 0.10, p => List(i(m4(p)))),
            // -- Rules for V --
            Rule(V, 0.10, p => List(v(p))),
            Rule(V, 0.15, p => List(iv(p.h), v(p.h))),
            Rule(V, 0.10, p => List(iii(p.h), vi(p.h))),
            Rule(V, 0.10, p => List(i(p.q), iii(p.q), vi(p.q), v(p.q))),
            Rule(V, 0.10, p => List(v(p.q), vi(p.q), vii(p.q), v(p.q))),
            Rule(V, 0.10, p => List(v(p.h), vi(p.h))),
            Rule(V, 0.10, p => List(iii(p))),
            Rule(V, 0.05, p => List(v(p.h), v(p.h))),
            Rule(V, 0.10, p => List(vii(p.h), v(p.h))),
            Rule(V, 0.10, p => List(i(m5(p)))),
            // -- Rules for VI --
            Rule(VI, 0.70, p => List(vi(p))),
            Rule(VI, 0.30, p => List(i(m6(p)))),
            // -- Rules for VII --
            Rule(VII, 0.50, p => List(if p.dur > qn then vii(p) else i(m7(p)))),
            Rule(VII, 0.50, p => List(i(p.h), iii(p.h)))
        ) ++ (if useLets then letRules else Nil)
        normalize(rawRules.map(toRelDur2(_ < minDur)))
        
end MusicGrammars