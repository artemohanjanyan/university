module Simple.Combinators where

import qualified Data.Set          as Set

import           Simple.Church
import           Simple.Expression
import           Simple.Reduction

s :: Expression
k :: Expression
i :: Expression
y :: Expression
fact :: Expression
s = L "x" $ L "y" $ L "z" $ V "x" :$: V "z" :$: (V "y" :$: V "z")
k = L "x" $ L "y" $ V "x"
i = s $$ k $$ k
y = L "f" $ (L "x" $ V "f" :$: (V "x" :$: V "x")) :$: (L "x" $ V "f" :$: (V "x" :$: V "x"))
fact = y :$: fact'
  where
    fact' = L "f" $ L "n" $ isZero :$: V "n" :$: fromInt 1 :$: (mul :$: V "n" :$: (V "f" :$: (dec :$: V "n")))

data CombExpr
    = S | K | I
    | CL Var CombExpr
    | CombExpr :$$: CombExpr
    | CV Var
    deriving (Eq, Ord)

latexShow :: CombExpr -> String
latexShow S                     = "\\comb S"
latexShow K                     = "\\comb K"
latexShow I                     = "\\comb I"
latexShow (CV x)                = x
latexShow (a :$$: b@(_ :$$: _)) = latexShow a ++ " \\left(" ++ latexShow b ++ "\\right)"
latexShow (a :$$: b)            = latexShow a ++ " " ++ latexShow b
latexShow _                     = undefined

cGetFreeVars :: CombExpr -> Set.Set Var
cGetFreeVars (CL var expr)      = Set.delete var $ cGetFreeVars expr
cGetFreeVars (expr1 :$$: expr2) = Set.union (cGetFreeVars expr1) (cGetFreeVars expr2)
cGetFreeVars (CV var)           = Set.singleton var
cGetFreeVars _                  = Set.empty

convert :: Expression -> CombExpr
convert (V x)     = CV x
convert (L x e)   = CL x $ convert e
convert (a :$: b) = convert a :$$: convert b

trans :: CombExpr -> CombExpr
trans x@(CV _)          = x
trans (a :$$: b)        = trans a :$$: trans b
trans (CL x p)          | Set.notMember x $ cGetFreeVars p = K :$$: trans p
trans (CL x (CV w))     | x == w = I
trans (CL x (a :$$: b)) = S :$$: trans (CL x a) :$$: trans (CL x b)
trans (CL x (CL w a))   = trans $ CL x $ trans $ CL w $ a
trans comb              = comb

transBack :: CombExpr -> Expression
transBack (CL _ _)   = undefined
transBack S          = s
transBack K          = k
transBack I          = i
transBack (a :$$: b) = transBack a :$: transBack b
transBack (CV x)     = V x

combTransBack :: CombExpr -> Expression
combTransBack (CL _ _)   = undefined
combTransBack S          = V "s"
combTransBack K          = V "k"
combTransBack I          = V "i"
combTransBack (a :$$: b) = combTransBack a :$: combTransBack b
combTransBack (CV x)     = V x

translate :: Expression -> Expression
translate expr = transBack $ trans $ convert expr

combTranslate :: Expression -> Expression
combTranslate expr = combTransBack $ trans $ convert expr
