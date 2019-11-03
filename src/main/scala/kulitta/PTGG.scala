package kulitta

import utils._
import Random._

/*
Probabilistic Temporal Graph Grammar Implementation
Donya Quick
Scala translation by Bart Schuller
*/
object PTGG
/*
The Term data structure has three constructors: 
    1. NT - a nonterminal that has a symbol and a parameter.
    2. Let - a means of capturing variable instantiation in a Term.
    3. Var - a variable	
*/
    enum Term[A,B]
        case NT(a: A, b: B)
        case Let(x: String, a: Sentence[A,B], e: Sentence[A,B])
        case Var(x: String)
    import Term._
/*
    A Sentence is a list of Terms.
*/
    type Sentence[A,B] = List[Term[A,B]]
/*
A rule has a left and righthand side. The lefthand side has
an un-parameterized symbol and a probability for application of
the rule. The righthand side is a function from a parameter to
a Sentence.
*/
    type Prob = Double
    // In Haskell a Rule looks like:  (a, Prob) :-> RuleFun a b
    case class Rule[A,B](lhs: A, prob: Prob, rfun: RuleFun[A,B])
    type RuleFun[A,B] = B => Sentence[A,B]
/*
A function to rewrite one Sentence to another using an 
L-System-like approach to generation where all symbols are 
updated from left to right.
*/
    def update[A,B](rules: Seq[Rule[A,B]])(g: StdGen, sent: Sentence[A,B]): (StdGen, Sentence[A,B]) =
        sent match
        case Nil => (g, Nil)
        case t :: ts => t match
            case NT(c, d) =>
                val (g1, tp) = applyRule(rules, g, c, d)
                val (g2, tsp) = update(rules)(g1, ts)
                (g2, tp ++ tsp)
            case Let(x, a, e) =>
                val (g1, ap) = update(rules)(g, a)
                val (g2, ep) = update(rules)(g1, e)
                val (g3, tsp) = update(rules)(g2, ts)
                (g3, Let(x, ap, ep) :: tsp)
            case x =>
                val (g1, tsp) = update(rules)(g, ts)
                (g1, x :: tsp)
/*
Function to update a single symbol:
*/
    def applyRule[A,B](rules: Seq[Rule[A,B]], g: StdGen, c: A, d: B): (StdGen, Sentence[A,B]) =
        val rs = rules.filter(_.lhs == c)
        val (p, gp) = randomR((0.0, 1.0), g)
        def choose(rules: Seq[Rule[A,B]], p: Prob): RuleFun[A,B] =
            rules match
            case Nil => sys.error("Nothing to choose from!")
            case Rule(c, pp, rf) :: rs =>
                if p <= pp || rs.isEmpty then rf else choose(rs, p-pp)
        if rs.isEmpty then (g, List(NT(c, d))) else (gp, choose(rs, p)(d))
/*
Note: we assume the value is a NT (nonterminal) because applyRule can only
be called from NT in update. We want to leave these as NTs rather than forcing
them to terminals in case the user wishes to apply different rule sets later.

User-level generation:
*/
    def gen[A,B](rules: Seq[Rule[A,B]], g: StdGen, t: Sentence[A,B]): LazyList[(StdGen, Sentence[A,B])] =
        LazyList.iterate((g, t))(update(rules))
/*
The expand function eliminates Lets and Vars from a generated Term a.
It allows for nested Let expressions for variables with the same name
with lexical scoping. For example:

expand [] [Let "x" t1 [Let "x" t2 (Var "x")]] ==> t2

> expand :: [(String, Sentence a b)] -> Sentence a b -> Sentence a b
> expand e [] = []
> expand e (t:ts) = case t of 
>     Let x a exp -> expand ((x, expand e a) : e) exp ++ expand e ts
>     Var x       -> (maybe (error (x ++ " is undefined")) id $ lookup x e) ++ expand e ts
>     x           -> x : expand e ts
*/
    def expand[A,B](e: List[(String, Sentence[A,B])], sentence: Sentence[A,B]): Sentence[A,B] =
        sentence match
        case Nil => Nil
        case t :: ts => t match
            case Let(x, a, exp) => expand((x, expand(e, a)) :: e, exp) ++ expand(e, ts)
            case Var(x) => e.find(_._1 == x).map(_._2).getOrElse(sys.error(s"$x is undefined")) ++ expand(e, ts)
            case x => x :: expand(e, ts)
/*
------------------------------------------------------------------------

Additional manipulations

Map defined over Term to convert pairs of one type to pairs of another.

> tMap :: ((a,b) -> (c,d)) -> Sentence a b -> Sentence c d
> tMap f [] = []
> tMap f (t:ts) = 
>     let t' = case t of
>                  Let x a exp -> Let x (tMap f a) (tMap f exp)
>                  Var x -> Var x
>                  NT x -> NT $ f x
>     in  t' : tMap f ts
*/
/*
Flattening completely to a list
*/
    def toPairs[A,B](sentence: Sentence[A,B]): List[(A,B)] =
        def f(term: Term[A,B]): (A,B) = term match
            case NT(ct, d) => (ct, d)
            case _ => sys.error("(toPairs) Variable or Let expression encountered")
        expand(Nil, sentence).map(f)

end PTGG