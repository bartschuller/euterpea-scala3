package kulitta
/*
Search algoritm for use with chord spaces
Donya Quick and Paul Hudak
Scala translation by Bart Schuller

Implementation of a search algorithm for traversing chord spaces using
let-in constraints as well as progression level predicates.
*/
import utils._
import QuotientSpaces._
import chordspaces.OPTIC._

object Search
    type Constraints = Seq[Seq[(Int, Int)]]
    type Index = Int
    type Bound = Int

    type Fallback[A] = (EqClass[A], StdGen, A) => (StdGen, A)
/*
And also a nearest neighbor fallback function that would always 
succeed if the equivalence class has at least one element (which 
should always be the case).

> nearFall ::  EqClass AbsChord -> StdGen -> AbsChord -> (StdGen, AbsChord)
> nearFall e g x = 
>     let ds = map (simpleDist x) e :: [Double]
>         y = e !! (head $ findIndices (==minimum ds) ds)
>     in  (g, y)
*/
    def nearFall(e: EqClass[AbsChord], g: StdGen, x: AbsChord): (StdGen, AbsChord) =
        ???

    def greedyLet[A](p: Predicate[(A,A)], f: Fallback[A], k: Constraints,
                    es: Seq[EqClass[A]], g: StdGen): Seq[A] =
        ???
end Search