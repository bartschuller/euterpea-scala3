package kulitta
package foregrounds
/*
Simple Jazz Foreground Algorithms
Donya Quick
Scala translation by Bart Schuller
*/
object JazzFG
/*
First, we need to find the modes for Roman numerals interpreted
in a particular key/mode. The type JTriple is actually a synonym 
for TChord, but it is used for clarity to indicate that the pitch 
information represents a mode rather than a chord.
*/
    //def rotateModes(i: Int) = allModes.drop(i) ++ allModes.take(i)
//end JazzFG