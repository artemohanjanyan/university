import Predicate.Types
import Predicate.Base
import Predicate.Logic

import Data.Maybe

main = do
    input <- getLine
    let [a, b] = map (read :: String -> Int) $ words input
    putStr $ show $ makeProof a b

intToTerm n = foldr ($) Zero $ replicate n Succ

(>>>) = BinStmt Impl
infixr 6 >>>
(+++) = BinTerm Add
infixl 8 +++

makeProof :: Int -> Int -> Proof
makeProof a b = base <++> generate a b

generate :: Int -> Int -> Proof

generate n 0 = substZeroAdd [intToTerm n]

generate a b = generate a (b-1)
        <++> substSuccEq [aTerm +++ bTerm, cTerm]
        <:>  (Succ (aTerm +++ bTerm) :=: lc)
        <++> substSuccAdd [aTerm, bTerm]
        <++> substSym [aTerm +++ Succ bTerm, Succ (aTerm +++ bTerm)]
        <:>  (Succ (aTerm +++ bTerm) :=: aTerm +++ Succ bTerm)
        <++> substTrans [la, lb, lc]
        <:>  (la :=: lc >>> lb :=: lc)
        <:>  (lb :=: lc)
  where
    aTerm = intToTerm a
    bTerm = intToTerm (b - 1)
    cTerm = intToTerm (a + b - 1)
    la = Succ (aTerm +++ bTerm)
    lb = aTerm +++ Succ bTerm
    lc = Succ cTerm

substSym = subst $ getTarget sym
substSuccEq = subst $ getTarget succEq
substSuccAdd = subst $ getTarget succAdd
substZeroAdd = subst $ getTarget zeroAdd
substTrans = subst $ getTarget trans

getTarget (Proof _ target _) = target

subst :: Stmt -> [Term] -> Proof
subst stmt@(QuantStmt All var subStmt) (x:xs) = Proof [] impl [impl] <:> newStmt <++> subst newStmt xs
  where
    impl = stmt >>> newStmt
    newStmt = fromJust $ substIfFree var x subStmt
subst stmt _ = Proof [] stmt []

base = trans <++> sym <++> succEq <++> succAdd <++> zeroAdd

zeroAdd = Proof [] (parseStmt "@a(a+0=a)") $ map parseStmt
        [ "a+0=a"
        , "(a+0=a)->(0=0->0=0->0=0)->(a+0=a)"
        , "(0=0->0=0->0=0)->(a+0=a)"
        , "(0=0->0=0->0=0)->@a(a+0=a)"
        , "@a(a+0=a)"
        ]

succAdd = Proof [] (parseStmt "@a@b(a+b'=(a+b)')") $ map parseStmt
    [ "a+b'=(a+b)'"
    , "(a+b'=(a+b)')->(0=0->0=0->0=0)->(a+b'=(a+b)')"
    , "(0=0->0=0->0=0)->(a+b'=(a+b)')"
    , "(0=0->0=0->0=0)->@b(a+b'=(a+b)')"
    , "(0=0->0=0->0=0)->@a@b(a+b'=(a+b)')"
    , "@a@b(a+b'=(a+b)')"
    ]

succEq = Proof [] (parseStmt "@a@b(a=b->a'=b')") $ map parseStmt
    [ "a=b-> a'=b'"
    , "(a=b->a'=b')->(0=0->0=0->0=0)->(a=b->a'=b')"
    , "(0=0->0=0->0=0)->(a=b->a'=b')"
    , "(0=0->0=0->0=0)->@b(a=b->a'=b')"
    , "(0=0->0=0->0=0)->@a@b(a=b->a'=b')"
    , "@a@b(a=b->a'=b')"
    ]

sym = Proof [] (parseStmt "@a@b(a=b->b=a)") $ map parseStmt
    [ "@a@b@c(a=b->a=c->b=c)->@b@c(a+0=b->a+0=c->b=c)"
    , "@b@c(a+0=b->a+0=c->b=c)"
    , "@b@c(a+0=b->a+0=c->b=c)->@c(a+0=a->a+0=c->a=c)"
    , "@c(a+0=a->a+0=c->a=c)"
    , "@c(a+0=a->a+0=c->a=c)->(a+0=a->a+0=a->a=a)"
    , "a+0=a->a+0=a->a=a"
    , "a+0=a"
    , "a+0=a->a=a"
    , "a=a"
    , "a=a->a=b->a=a"
    , "a=b->a=a"
    , "(a=b->a=a)->(a=b->a=a->b=a)->(a=b->b=a)"
    , "(a=b->a=a->b=a)->(a=b->b=a)"
    , "a=b->b=a"
    , "(a=b->b=a)->(0=0->0=0->0=0)->(a=b->b=a)"
    , "(0=0->0=0->0=0)->(a=b->b=a)"
    , "(0=0->0=0->0=0)->@b(a=b->b=a)"
    , "(0=0->0=0->0=0)->@a@b(a=b->b=a)"
    , "@a@b(a=b->b=a)"
    ]

trans = Proof [] (parseStmt "@a@b@c(a=b->a=c->b=c)") $ map parseStmt
    [ "0=0->0=0->0=0"
    , "a=b->a=c->b=c"
    , "(a=b->a=c->b=c)->(0=0->0=0->0=0)->(a=b->a=c->b=c)"
    , "(0=0->0=0->0=0)->(a=b->a=c->b=c)"
    , "(0=0->0=0->0=0)->@c(a=b->a=c->b=c)"
    , "(0=0->0=0->0=0)->@b@c(a=b->a=c->b=c)"
    , "(0=0->0=0->0=0)->@a@b@c(a=b->a=c->b=c)"
    , "@c(a=b->a=c->b=c)"
    , "@c(a=b->a=c->b=c)->(a=b->a=a->b=a)"
    , "a=b->a=a->b=a"
    , "@a@b@c(a=b->a=c->b=c)"
    ]
