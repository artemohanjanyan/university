module Church where

import Prelude hiding (not, and, or)

import Types
import Reduction

($$) :: Expression -> Expression -> Expression
expr1 $$ expr2 = normalize $ expr1 :$: expr2

true = expr "\\a.\\b.a"
--true = L "a" $ L "b" $ V "a"
false = expr "\\a.\\b.b"
--false = L "a" $ L "b" $ V "b"
not = L "a" $ V "a" :$: false :$: true
and = L "a" $ L "b" $ V "a" :$: V "b" :$: false
or = L "a" $ L "b" $ V "a" :$: true :$: V "b"

fromInt :: Int -> Expression
fromInt n = L "f" $ L "x" $ fromInt' n
  where
    fromInt' 0 = V "x"
    fromInt' n = V "f" :$: fromInt' (n - 1)

toInt :: Expression -> Int
toInt (L _ (L _ e)) = toInt' e
  where
    toInt' (V _) = 0
    toInt' (_ :$: x) = 1 + toInt' x
