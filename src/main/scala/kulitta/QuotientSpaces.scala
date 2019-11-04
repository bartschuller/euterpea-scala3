package kulitta

object QuotientSpaces
    type EqClass[A] = Seq[A] // equivalence class
    type QSpace[A] = Seq[EqClass[A]] // quotient space
    type Predicate[A] = A => Boolean
    type Norm[A] = A => A // normalizations
    type EqRel[A] = (A, A) => Boolean // equivalence relations
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