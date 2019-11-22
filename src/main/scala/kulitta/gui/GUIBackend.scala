package kulitta
package gui

import utils.{given, _}
import Random.{given, _}
import foregrounds.JazzFG._
import foregrounds.ClassicalFG._
import grammars.MusicGrammars._
import CType._
import PostProc._
import PTGG._
import Term._
import Search._
import euterpea.Music.{Music, Control, Pitch, Volume, addVolume}
import Music._
import Control.Instrument
import java.io.File

object GUIBackend
    enum Style
        case Chorale // in style of JS Bach
        case PianoChorale 
        case JazzChorale // grammar, jazz chords, chorale foreground
        case WeirdChorale // grammar through OPT-space
        case JazzChords // grammar with jazz foreground
        case BossaNova // grammar with bossa foreground
        case PianoEtude1 
        case PianoEtude2
    import Style._
    enum Form
        case Phrase, AABA
    import Form._
    enum GramType
        case HandBuilt, Learned
    import GramType._
    case class Info(
        style: Style,
        form: Form,
        gram: GramType,
        mode: Mode,
        lets: Boolean,
        randKey: Boolean,
        probs: File
    )
/*
ABSTRACT PHRASE GENERATION

This function creates chorale phrases using a hand-built grammar. 
*/
    def makeRPhraseH(g: StdGen, m: Mode, durs: (Dur,Dur), lets: Boolean,
        iters: Int, len: Dur, partB: Boolean): (Constraints, List[RChord]) =
        val (minD, maxD) = durs
        val tSeed = List(NT(I, MP(len, m, 0, 0, 4)))
        val tVal = doGen(rRules1(minD, lets), iters, g, m, tSeed, maxD)
        val kVal = findInds(Nil, tVal)
        (kVal, toChords(expand(Nil, tVal)))

    def makeRPhraseB(g: StdGen, m: Mode, durs: (Dur,Dur),
        iters: Int, len: Dur, partB: Boolean, pFile: File): (Constraints, List[RChord]) = ???
/*
The doGen function adds a maximum duration constraint to get a more consistent 
distribution of durations. No more than searchLimit generative steps will be tried
to avoid infinite loops caused by generative fixed points.
*/
    val searchLimit = 1000

    def doGen[A](theRules: Seq[Rule[A,MP]], i: Int, g: StdGen, ctxt: Mode,
                t: Sentence[A,MP], maxDur: Dur): Sentence[A,MP] =
        def recCheck(i: Int, ts: LazyList[Sentence[A,MP]]): Sentence[A,MP] =
            val tc = ts(i)
            if (goodDurs(tc) && goodDursp(tc)) || i>searchLimit then tc else recCheck(i+1, ts)
        def goodDurs(tss: Sentence[A,MP]): Boolean =
            tss match
            case Nil => true
            case Let(x, a, e) :: ts => goodDurs(a) && goodDurs(e) && goodDurs(ts)
            case Var(_) :: ts => true
            case NT(c, p) :: ts => p.dur <= maxDur && goodDurs(ts)
        recCheck(i, gen(theRules, g, t).map(_._2))

    def goodDursp[A](t: Sentence[A,MP]): Boolean =
        val x = getDurs(t)
        x.filter(_ >= hn).length < x.length / 2

    def getDurs[A](tss: Sentence[A,MP]): List[Dur] =
        tss match
        case Nil => Nil
        case (t@Let(x,a,e)) :: ts => getDurs(expand(Nil, List(t))) ++ getDurs(ts)
        case Var(x) :: ts => sys.error("getDurs can't handle variables.")
        case NT(x,p) :: ts => p.dur :: getDurs(ts)
/*
===============================

FOREGROUND GENERATION

The following code configures Kulitta in different ways to generate different 
styles of music according to the user's specifications.
*/
    def makePiece(g: StdGen, i: Info, assignInstruments: Boolean):
        (Music[(Pitch, Volume)], Seq[(Constraints, Seq[RChord])]) =
        val LazyList(gStruct, gFG) = splitN(g).take(2)
        val genVals = if Seq(BossaNova, PianoEtude1).contains(i.style)
                        then (5, wn, wn, Rat(8)) else (5, qn, hn, Rat(4))
        val absStructs = makeStructure(gStruct, i, genVals)
        val theMusic = makeMusic(gFG, i, absStructs)
        (procInstrs(assignInstruments, theMusic), absStructs)

    def procInstrs[A](b: Boolean, mus: Music[A]): Music[A] =
        if b then mus else
            mus match
            case :=:(m1,m2) => procInstrs(false, m1) :=: procInstrs(false, m2)
            case :+:(LazyList(m1, m2)) => procInstrs(false, m1) :+: procInstrs(false, m2)
            case Modify(Instrument(_), m) => procInstrs(false, m)
            case m => m

    def makeSubStruct(gAbs: StdGen, i: Info, genVals: (Int, Dur, Dur, Dur),
                        partB: Boolean): (Constraints, List[RChord]) =
        val (iters, minD, maxD, len) = genVals
        if i.gram == HandBuilt then
            makeRPhraseH(gAbs, i.mode, (minD, maxD), i.lets, iters, len, partB)
        else
            makeRPhraseB(gAbs, i.mode, (minD, maxD), iters, len, partB, null)

    def makeStructure(gStruct: StdGen, i: Info, genVals: (Int, Dur, Dur, Dur)):
        List[(Constraints, List[RChord])] =
        val (g1, g2) = gStruct.split
        if i.form == Phrase then List(makeSubStruct(g1, i, genVals, false)) else
            List(makeSubStruct(g1, i, genVals, false),
                makeSubStruct(g2, i, genVals, true))

    def chorale(s: Style): Boolean = Seq(Chorale, JazzChorale, WeirdChorale, PianoChorale).contains(s)

    def makeMusic(g: StdGen, i: Info, absStructs: List[(Constraints, List[RChord])]) =
        val (k2, gFG) = randomR((0, 11), g)
        val k = if i.randKey then k2 else 0
        val fg = i.style match
            case Chorale => addVolume(127, buildChorale(gFG, absStructs, k, i.mode))
            case JazzChorale => addVolume(127, buildJChorale(gFG, absStructs, k, i.mode))
            case JazzChords => addVolume(127, buildJazzChords(gFG, absStructs, k, i.mode))
            case BossaNova => buildBossaNova(gFG, absStructs, k, i.mode)
            case x => sys.error(s"makeMusic: style $x not implemented")
        fg
/*
A chorale is pretty straightforward, using the ClassicalFG.lhs implementation.
*/
    def buildChorale(g: StdGen, absStructs: List[(Constraints, List[RChord])], k: Int, m: Mode): Music[Pitch] =
        absStructs match
        case List((cons, x)) => classicalFRG(g, ctTrans(k)(x), cons)._2._2
        case List((cons1, a), (cons2, b)) =>
            val LazyList(g1, g2, g3, g4, g5) = splitN(g).take(5)
            val aChords = classicalCS(g1, ctTrans(k)(a), cons1)._2
            val bChords = classicalCS(g2, ctTrans(k)(b), cons2)._2
            val partA = classicalFGp(g3, aChords)._2._2
            val partAp = classicalFGp(g4, aChords)._2._2
            val partB = classicalFGp(g5, bChords)._2._2
            partA :+: partAp :+: partB :+: partA

/*
A "jazz chorale" ads an extra step in the foreground generation, converting 
numerals to jazz chords before running the classical algorithms.
*/
    def buildJChorale(g: StdGen, absStructs: List[(Constraints, List[RChord])], k: Int, m: Mode): Music[Pitch] =
        absStructs match
        case List((cons, x)) =>
            val (g1, g2) = g.split
            val jChords = atTrans(k)(jazzChords(g1, x, cons)._2)
            classicalFGp(g2, jChords)._2._2
        case List((cons1, a), (cons2, b)) =>
            val LazyList(g1, g2, g3, g4, g5) = splitN(g).take(5)
            val aChords = atTrans(k)(jazzChords(g1, a, cons1)._2)
            val bChords = atTrans(k)(jazzChords(g2, b, cons2)._2)
            val partA = classicalFGp(g3, aChords)._2._2
            val partAp = classicalFGp(g4, aChords)._2._2
            val partB = classicalFGp(g5, bChords)._2._2
            partA :+: partAp :+: partB :+: partA
/*
Jazz foregrounds are created using the two algorithms in JazzFG.lhs.
*/
    def buildJazzChords = buildJazz(jazzFG1)
    def buildBossaNova = buildJazz(jazzFG2)

    def buildJazz[A](f: (g: StdGen, chords: List[(Key,Dur,CType)],
                    consts: Constraints) => (StdGen, Music[A])
                    )(g: StdGen, absStructs: List[(Constraints, List[RChord])],
                        k: Int, m: Mode): Music[A] =
        absStructs match
        case List((cons, x)) => f(g, ctTrans(k)(x), Nil)._2
        case List((cons1, a), (cons2, b)) =>
            val LazyList(g1, g2, g3) = splitN(g).take(3)
            val jA = f(g1, ctTrans(k)(a), Nil)._2
            val jAp = f(g2, ctTrans(k)(a), Nil)._2
            val jB = f(g3, ctTrans(k)(b), Nil)._2
            jA :+: jAp :+: jB :+: jA
end GUIBackend