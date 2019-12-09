package kulitta
package gui
import utils.{given, _}
import StdGen._
import GUIBackend._
import Style._
import Form._
import GramType._
import grammars.MusicGrammars._
import Mode._
import euterpea.midi.MEvent._
import euterpea.midi.ToSmidi._
import smidi._

object GUI
    @main def interactive(): Unit =
        //val i = Info(JazzChorale, AABA, HandBuilt, Major, false, false, null)
        //automated(i, 1L, "jazzchorale-aaba-hand-major-1.mid")
        val i = Info(PianoEtude2, AABA, HandBuilt, Major, false, false, null)
        automated(i, 1L, "pianoetude2-aaba-hand-major-1.mid")

    def automated(i: Info, seed: Long, outFile: String, assignInstruments: Boolean = true): Unit =
        val (m, abst) = makePiece(mkStdGen(seed), i, assignInstruments)
        val pf = perform(m)
        val sequence = toMidi(pf)
        MidiSystem.write(sequence, 1, outFile)
        val sequencer = MidiSystem.sequencer
        sequencer.setSequence(sequence)
        sequencer.open()
        sequencer.start()
        while (sequencer.isRunning)
        Thread.sleep(1000L)
        sequencer.stop()
        sequencer.close()
end GUI