module Homework.Rat

import Setoid
import Homework.MyInt

%access public export

data Rat =
    ||| MkRat a b represents a divided by b
    MkRat MyInt Nat

implementation Num Rat where
    -- a/b + c/d = (a * d + c * b) / (b * d)
    (+) (MkRat a b) (MkRat c d) = MkRat (multNat a d + multNat c b) (b * d)
    -- a/b * c/d = a * c / (b * d)
    (*) (MkRat a b) (MkRat c d) = MkRat (a * c) (b * d)

    fromInteger a = MkRat (fromInteger a) 1

implementation Neg Rat where
    negate (MkRat a b) = MkRat (negate a) b
    a - b = a + negate b
    abs (MkRat a b) = MkRat (abs a) b

implementation Fractional Rat where
    (/) = ?undefined
    recip = ?undefined

||| Equality on Rat
data RatEq : Rat -> Rat -> Type where
    ||| a/b == c/d  <===>  a * d == b * c
    RatRefl : (eq : multNat a d = multNat c b) -> RatEq (MkRat a b) (MkRat c d)

ratReflx : Reflx RatEq
ratReflx (MkRat a b) = RatRefl Refl

ratSym : Sym RatEq
ratSym (MkRat a b) (MkRat c d) (RatRefl eq) = RatRefl $ sym eq

ratTrans : Trans RatEq
ratTrans (MkRat a b) (MkRat c d) (MkRat e f) (RatRefl eq1) (RatRefl eq2) = ?ratTrans_rhs

||| Setoid of Rat
RatSetoid : Setoid
RatSetoid = MkSetoid Rat RatEq $ EqProof RatEq ratReflx ratSym ratTrans
