module Church where

import           Prelude    hiding (and, not, or)

import           Expression
import           Reduction

true = expr "\\a.\\b.a"
--true = L "a" $ L "b" $ V "a"
false = expr "\\a.\\b.b"
--false = L "a" $ L "b" $ V "b"
not = L "a" $ V "a" :$: false :$: true
and = L "a" $ L "b" $ V "a" :$: V "b" :$: false
or = L "a" $ L "b" $ V "a" :$: true :$: V "b"

fromInt :: Int -> Expression
fromInt n = L "f" $ L "x" $ foldr ($) (V "x") $ replicate n (V "f" :$:)

toInt :: Expression -> Int
toInt (L _ (L _ e)) = toInt' e
  where
    toInt' (V _)     = 0
    toInt' (_ :$: x) = 1 + toInt' x

inc = L "n" $ L "f" $ L "x" $ V "f" $$ (V "n" $$ V "f" $$ V "x")
isZero = L "n" $ V "n" $$ (L "x" false) $$ true
isEven = L "n" $ V "n" $$ not $$ true
add = L "a" $ L "b" $ L "f" $ L "x" $ V "a" $$ V "f" $$ (V "b" $$ V "f" $$ V "x")
mul = L "a" $ L "b" $ V "a" $$ (add $$ V "b") $$ fromInt 0
pow = L "a" $ L "b" $ V "b" $$ V "a"
pow' = L "a" $ L "b" $ V "b" $$ (mul $$ V "a") $$ fromInt 1

pair = expr $ "\\a.\\b.\\f. f a b"
first = L "p" $ V "p" $$ true
second = L "p" $ V "p" $$ false
dec = L "n" $ first $$ (V "n" $$ (L "p" $ pair $$ (second $$ V "p") $$ (inc $$ (second $$ V "p"))) $$ (pair $$ fromInt 0 $$ fromInt 0))
sub = L "a" $ L "b" $ V "b" $$ dec $$ V "a"
