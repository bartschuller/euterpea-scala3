package kulitta
package examples
import PTGG._
import grammars.MusicGrammars._
import CType._
import Mode._
import foregrounds.ClassicalFG._
import PostProc._
import utils.{given _, _}
import StdGen.mkStdGen
import euterpea.Music.{line, rest, cut}
import euterpea.midi.MEvent._
import euterpea.midi.ToSmidi._
import smidi._

object Example2:

    val rules = rRules1(en, true)

    val startSym = List(i(MP(wn*32, Major, 0, 0, wn*32)))
    val g1 = mkStdGen(42)

    @main def ex2 =
        val (g2, absStruct) = gen(rules, g1, startSym)(10)

        println(absStruct)
        println()

        val (g3, chords) = classicalCS2(g2, toAbsChords(absStruct), Nil)

        println(chords)
        println()

        val (g4, (justChords, finalMusic)) = classicalFGp(g3, chords)

        val pf = perform(line(Seq(cut(wn*2, justChords), rest(wn), finalMusic)))
        val sequence = toMidi(pf)
        MidiSystem.write(sequence, 1, "example2.mid")
        val sequencer = MidiSystem.sequencer
        sequencer.setSequence(sequence)
        sequencer.open()
        sequencer.start()
        while (sequencer.isRunning)
        Thread.sleep(1000L)
        sequencer.stop()
        sequencer.close()
end Example2