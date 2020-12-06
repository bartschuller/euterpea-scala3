package kulitta
package examples
import PTGG._
import grammars.MusicGrammars._
import CType._
import Mode._
import foregrounds.ClassicalFG._
import PostProc._
import utils.{given, _}
import StdGen.mkStdGen
import euterpea.Music.{line, rest}
import euterpea.midi.MEvent._
import euterpea.midi.ToSmidi._
import smidi._
import scala.language.experimental.genericNumberLiterals

object Example1:
/*
Let's start by creating a very simple musical grammar. Kulitta comes with
a few built-in, but it is possible to define new ones. We'll use some of
the datatypes defined in MusicGrammars.lhs for Roman numeral symbols. 

data CType = I | II | III | IV | V | VI | VII
    deriving (Eq, Show, Ord, Enum, Read)


We'll encode just a few simple rules and give them probabilities by hand,
assuming that duration is divided in half for rules with two symbols on the right:

1. (0.3) I -> V I
2. (0.6) I -> I I
3. (0.1) I -> I
4. (0.5) V -> IV V
5. (0.4) V -> V V
6. (0.1) V -> V
7. (0.8) IV -> IV IV
8. (0.2) IV -> IV

We'll also use the MP type ("music parameter") from MusicGrammars.lhs to store
the duration for each symbol. We can now write these rules in Haskell as follows. 
*/
    val r1: Rule[CType,MP] = Rule(I, 0.3, p => List(v (p.h), i (p.h)))
    val r2: Rule[CType,MP] = Rule(I, 0.3, p => List(i (p.h), i (p.h)))
    val r3: Rule[CType,MP] = Rule(I, 0.1, p => List(i (p)))
    val r4: Rule[CType,MP] = Rule(V, 0.5, p => List(iv (p.h), v (p.h)))
    val r5: Rule[CType,MP] = Rule(V, 0.4, p => List(v (p.h), v (p.h)))
    val r6: Rule[CType,MP] = Rule(V, 0.1, p => List(v (p)))
    val r7: Rule[CType,MP] = Rule(IV, 0.8, p => List(iv (p.h), iv (p.h)))
    val r8: Rule[CType,MP] = Rule(IV, 0.2, p => List(iv (p)))
/*
The argument "p" to the anonymous function ("\p -> ...", read as
"given some value, p, do ... to it") in the code below refers to the parameter
associated with the rules. In MusicGrammars.lhs, the function "h" divides the
duration of the symbol in half. The file also provides lower-case versions of
the Roman numerals that are functions to create appropriately-typed data
structures for the grammar.

Now, one problem with using the rules as-is above is that they will exhibit
L-System like behavior of unbalanced durations. Kulitta fixes this by allowing
conditional rules, such as:

I -> if not enough duration then do nothing, otherwise (normal righthand side)

This results in a more even distribution of durations, since they cannot become
infinitely small as generation progresses. We can convert all the rules to
this format as follows.
*/
    val rules = Seq(r1, r2, r3, r4, r5, r6, r7, r8).map(toRelDur2(_ < en))
/*
Now we can generate some music with the grammar. First, we will create a
start symbol and a random generator to work with. The start symbol will be
a 4-measure long I-chord (4 times a wholenote, wn) in C-major (written below
as "Major" with root pitch class 0, or C).
*/
    val startSym = List(i(MP(wn*4, Major, 0, 0, wn*4)))
    val g1 = mkStdGen(42)

    @main def doStuff =

/*
The "gen" function creates an infinite list of sequential generative
iterations. We will call it on the start symbol with a random number seed
and then take the 5th generative iteration. This step returns a new random
generator, g2, in addition to the abstract structure of the music.
*/
        val (g2, absStruct) = gen(rules, g1, startSym)(7)

        println(absStruct)
        println()
/*
Now we can map pitches to these chords with a classical chord space.
We will impose no additional search constraints and just use Kulitta's
defaults. This step also returns a new generator, g3, that we can use for
the final step.
*/
        val (g3, chords) = classicalCS2(g2, toAbsChords(absStruct), Nil)

        println(chords)
        println()
/*
And finally, we put a simple foreground on top.
*/
        val (g4, (justChords, finalMusic)) = classicalFGp(g3, chords)

        val pf = perform(line(Seq(justChords, rest(wn), finalMusic)))
        val sequence = toMidi(pf)
        MidiSystem.write(sequence, 1, "example1.mid")
        val sequencer = MidiSystem.sequencer
        sequencer.setSequence(sequence)
        sequencer.open()
        sequencer.start()
        while (sequencer.isRunning)
        Thread.sleep(1000L)
        sequencer.stop()
        sequencer.close()
end Example1