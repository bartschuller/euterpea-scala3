package kulitta
package foregrounds
/*
Classical Foreground Module
Donya Quick
Scala translation by Bart Schuller
*/
import utils.{given, _}
import QuotientSpaces._
import chordspaces.OPTIC._
import PostProc._
import Search._
import Constraints._
import grammars.MusicGrammars._
import Random.{given, _}
import euterpea.Music.{Music, Pitch, InstrumentName}

object ClassicalFG
    case class CConstants(
        ntLimC: Int, // limit for neighboring tone distance
        ptLimC: Int, // limit for passing tone distance
        pHalfC: Double, // probability of dividing a note's duration in half (alternative is x-en and en)
        pTieC: Double, // probability of tying two identical notes
        rootBassThreshC: Double, // probability of enforcing that the bass be the root
        noCPLThreshC: Int // voice-leading maximum, setting to 0 forces nearest neighbor fallback
    )
// A set of default constants that work pretty well in most cases.
    val defConsts = CConstants(2, 3, 0.5, 0.5, 0.8, 7)
/*
Get all pitches shared between the scales of two TNotes
*/
    def allPs(t1: TNote, t2: TNote): Seq[AbsPitch] =
        def baseScale(key: Key): Seq[AbsPitch] =
            val (k, m) = key
            normOP(t(k)(getScale(m)))
        val o1 = tnP(t1) / 12
        val o2 = tnP(t2) / 12
        val Seq(oMin, oMax) = Seq(o1, o2).sorted
        val offs = Seq(oMin-1, oMin, oMax, oMax+1).map(_ * 12)
        val s1 = baseScale(tnK(t1))
        val s2 = baseScale(tnK(t2))
        offs.flatMap(o => t(o)(s1.filter(s => s2.exists(_ == s)))).distinct

/*
A foreground function, ForeFun, is a stochastic operation on two notes that 
may or may not add an additional pitch between them. Duration selection 
happens as a second step later since it involves altering the durations of 
the surrounding notes.
*/
    type ForeFun = (StdGen, TNote, TNote) => (StdGen, Option[AbsPitch])
/*
A passing tone is a note between two chodal tones. Here, the definition is a
litle broader than what would be assumed in standard music theory since the 
chordal tones need not be close together. As a result, the "passing tones" 
chosen by pickPT below may end up being other categories of non-chordal 
tones in music theory.
*/
    def pickPT(lim: AbsPitch)(g: StdGen, t1: TNote, t2: TNote): (StdGen, Option[AbsPitch]) =
        val Seq(pMin, pMax) = Seq(tnP(t1), tnP(t2)).sorted
        def f(x: AbsPitch) = x > pMin && x < pMax && (x - pMin <= lim || pMax -x <= lim)
        val psp = allPs(t1, t2).filter(f)
        if pMin == pMax || psp.isEmpty then (g, None) else
            val (iNew, gp) = randomR((0, psp.length - 1), g)
            (gp, Some(psp(iNew)))
/*
Neighboring tones are handled similarly to passing tones. Again, the 
definition here is fairly broad and may produce other categorie of non-
chordal tones as a result.
*/
    def pickNT(lim: AbsPitch)(g: StdGen, t1: TNote, t2: TNote): (StdGen, Option[AbsPitch]) =
        val Seq(pMin, pMax) = Seq(tnP(t1), tnP(t2)).sorted
        def f(x: AbsPitch) = (x < pMin && pMin - x <= lim) || (x > pMax && x - pMax <= lim)
        val psp = allPs(t1, t2).filter(f)
        val (iNew, gp) = randomR((0, psp.length - 1), g)
        if pMin == pMax || psp.isEmpty then (g, None)
        else (gp, Some(psp(iNew)))
/*
Functions for adding anticipations and repetitions are simple to define. 
A "do nothing" operations is also a useful option to have.
*/
    def anticip(g: StdGen, t1: TNote, t2: TNote): (StdGen, Option[AbsPitch]) = (g, Some(tnP(t2)))
    def rept(g: StdGen, t1: TNote, t2: TNote): (StdGen, Option[AbsPitch]) = (g, Some(tnP(t1)))
    def doNothing(g: StdGen, t1: TNote, t2: TNote): (StdGen, Option[AbsPitch]) = (g, None)
/*
Some of the functions above need access to constants. We use the 
CConstants type for this, yielding a collection of functions of 
type CConstants -> ForeFun. Finally, these are each bundled with 
a probability of application for each voice.
*/
    def f1(c: CConstants) = pickPT(c.ptLimC)
    def f2(c: CConstants) = pickNT(c.ntLimC)
    def f3(c: CConstants) = anticip
    def f4(c: CConstants) = rept
    def f5(c: CConstants) = doNothing

    def allFFs(c: CConstants): LazyList[List[(Double, ForeFun)]] =
        LazyList(
            List((0.3, f1(c)), (0.1, f2(c)), (0.6, f5(c))), // S (soprano)
            List((0.3, f1(c)), (0.7, f5(c))), // A (alto)
            List((0.1, f1(c)), (0.9, f5(c))) // T (tenor)
        ) ++ LazyList.continually(
            List((1.0, f5(c))) // B (bass) and lower
        )

    def splitP(consts: CConstants, g: StdGen, newP: AbsPitch, t: TNote): (StdGen, List[TNote]) =
        val (r, gp) = randomR((0.0, 1.0), g)
        val dNew = if r < consts.pHalfC then tnD(t) / 2 else en
        (gp, List((tnK(t), tnD(t) - dNew, tnP(t)), (tnK(t), dNew, newP)))
/*
Here we assume that the contexts include the voice in question. The |i| argument is the voice number.
The function only returns modifications of the first chord.
*/
    def addFgToVoice(   c: CConstants, foreFuns: List[(Double, ForeFun)],
                        g: StdGen, tnotes: List[TNote]): (StdGen, List[TNote]) =
        tnotes match
        case t1 :: t2 :: ts =>
            def chooseFF(j: Double, foreFuns: List[(Double, ForeFun)]): ForeFun =
                foreFuns match
                case List(x) => x._2
                case (p,x) :: t => if j < p then x else chooseFF(j-p, t)
                case Nil => sys.error("(chooseFF) Nothing to choose from!")
            def applyForeFun(   c: CConstants, g: StdGen, t1: TNote, t2: TNote,
                                fFun: ForeFun): (StdGen, List[TNote]) =
                val (g1, newP) = fFun(g, t1, t2)
                newP.map(splitP(c, g1, _, t1)).getOrElse((g1, List(t1)))
            val (j, g1) = randomR((0.0, 1.0), g)
            val fFun = chooseFF(j, foreFuns)
            val (g2, t1p) = applyForeFun(c, g1, t1, t2, fFun)
            val (g3, tRest) = addFgToVoice(c, foreFuns, g2, t2 :: ts)
            (g3, t1p ++ tRest)
        case _ => (g, tnotes)
/*
After adding foreground elements, ties can be considered. The following |stochTie| 
function stochastically ties notes in a voice. 
*/
    def stochTie(consts : CConstants, g: StdGen, tss: List[TNote]): (StdGen, List[TNote]) =
        tss match
        case t1 :: t2 :: ts =>
            val (r, g1) = randomR((0.0, 1.0), g)
            val (g2, (t2p :: tsp)) = stochTie(consts, g1, t2 :: ts)
            val d1 = tnD(t1)
            val d2p = tnD(t2p)
            if tnP(t1) == tnP(t2p) && r < consts.pTieC
            then (g2, (tnK(t1), d1+d2p, tnP(t1)) :: tsp)
            else (g2, t1 :: t2p :: tsp)
        case _ => (g, tss)
/*
Finally, the |addFG| function puts all of these elements together.
*/
    def addFG(c: CConstants, g: StdGen, vs: Seq[List[TNote]]): (StdGen, List[List[TNote]]) =
        def fgRec(c: CConstants, g: StdGen, i: Int, vs: Seq[List[TNote]]):
                    (StdGen, Seq[List[TNote]]) =
            if i >= vs.length || i < 0 then (g, vs) else
                val (gp, vp) = addFgToVoice(c, allFFs(c)(i), g, vs(i))
                val vsp = vs.take(i) ++ List(vp) ++ vs.drop(i+1)
                fgRec(c, gp, i+1, vsp)
        def tieRec(c: CConstants, g: StdGen, vss: Seq[List[TNote]]): (StdGen, List[List[TNote]]) =
            vss match
            case Nil => (g, Nil)
            case v +: vs =>
                val (g1, vp) = stochTie(c, g, v)
                val (g2, vsp) = tieRec(c, g1, vs)
                (g2, vp :: vsp)
        val (gp, vsp) = fgRec(c, g, 0, vs)
        tieRec(c, gp, vsp)

    def classicalFRG(g: StdGen, rcs: List[RChord], consts: Constraints): (StdGen, (Music[Pitch], Music[Pitch])) =
        val (g1, csChords) = classicalCS(g, rcs, consts)
        classicalFGp(g1, csChords)

    def classicalFGp(g: StdGen, aChordsp: List[TChord]): (StdGen, (Music[Pitch], Music[Pitch])) =
        val (g4, csFG) = addFG(defConsts, g, toVoices(aChordsp).reverse)
        import InstrumentName._
        val is = List(Bassoon, EnglishHorn, Clarinet, Oboe, SopranoSax)
        val fgM = vsToMusicI(is, csFG.reverse)
        val csM = vsToMusicI(is, toVoices(aChordsp))
        (g4, (csM, fgM))
/*
Similarly, there are instances when we may want to use a classical chord space, but
not add a classical foreground. This can be useful for mixing styles.
*/
    def classicalCS(g: StdGen, rcs: List[RChord], consts: Constraints): (StdGen, List[TChord]) =
        classicalCS2(g, rcs.map(toAbsChord), consts)

    def classicalCS2(g: StdGen, aChords: List[TChord], consts: Constraints): (StdGen, List[TChord]) =
        val justChords = aChords.map(_._3)
        val (g1, g2) = g.split
        val (g3, eqs) = classBass(0.8, g2, justChords.map(eqClass(satbOP, opcEq)))
        val csChords = greedyLet(noCPL(7), nearFall, consts, eqs, g3)
        val aChordsp = aChords.zipWith(csChords){ case ((a,b,c), d) => (a,b,d) }
        (g3, aChordsp.toList)
/*
The classicalCS2 function uses a stochastic filter over equivalence classes.
This filter enforces that the bass holds the root with a certain probability 
(the "thresh" value). If the constraints can't be met, the bass is allowed 
to deviate from this rule for the sake of producing a result.
*/
    def classBass(thresh: Double, g: StdGen, ess: List[EqClass[AbsChord]]): (StdGen, List[EqClass[AbsChord]]) =
        ess match
        case Nil => (g, Nil)
        case e :: es =>
            def rootFilter(x: AbsChord): Boolean =
                Seq(Seq(0,0,4,7), Seq(0,0,3,7), Seq(0,0,3,6)).exists(opcEq(x, _))
            val (r, gp) = randomR((0.0, 1.0), g)
            val ep = if r > thresh then e else e.filter(rootFilter)
            val epp = if ep.isEmpty then e else ep
            val (gpp, esp) = classBass(thresh, gp, es)
            (gpp, epp :: esp)
end ClassicalFG