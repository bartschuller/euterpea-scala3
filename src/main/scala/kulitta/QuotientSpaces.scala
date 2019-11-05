package kulitta

object QuotientSpaces
    type EqClass[A] = Seq[A] // equivalence class
    type QSpace[A] = Seq[EqClass[A]] // quotient space
    type Predicate[A] = A => Boolean
    type Norm[A] = A => A // normalizations
    type EqRel[A] = (A, A) => Boolean // equivalence relations
/*
========= QUOTIENT SPACE IMPLEMENTATION =========

First we define the "slash" operator for S/R. 

> (//) :: (Eq a) => [a] -> EqRel a -> QSpace a
> [] // r = []
> xs // r = 
>     let sx = [y | y <- xs, r y (head xs)] 
>     in  sx : [z | z <- xs, not (elem z sx)] // r
*/
    def [A](xs: Seq[A]) \ (r: EqRel[A]): QSpace[A] =
        if xs.isEmpty then Seq.empty else
            val sx = xs.filter(r(_, xs.head))
            sx +: xs.filterNot(sx.contains) \ r
/*
The eqClass function is used to find the equivalence class of an
element, x, given a quotient space and the relation used to form it.
We need to know the relation used, because we do not require that
x is in the quotient space, qs.

> eqClass :: (Eq a, Show a) => QSpace a -> EqRel a -> a -> EqClass a
> eqClass qs r x = 
>     let ind = findIndex (\e -> r x (head e)) qs
>     in  maybe (error ("(eqClass) No class for "++show x)) (qs !!) ind 
*/
    def eqClass[A](qs: QSpace[A], r: EqRel[A])(x: A): EqClass[A] =
        ???
end QuotientSpaces