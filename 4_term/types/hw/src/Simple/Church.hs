module Simple.Church
    ( true
    , false
    , not
    , and
    , or

    , fromInt
    , toInt

    , inc
    , isZero
    , isEven
    , add
    , mul
    , pow

    , pair
    , first
    , second
    , dec
    , sub
    ) where

import           Prelude           hiding (and, not, or)
import           Text.Parsec

import           Simple.Expression
import           Simple.Reduction

readExpression :: String -> Expression
readExpression str = case parse expressionParser "" str of
    Right expr -> expr
    Left  _    -> error "no parse"

l :: Var -> Expression -> Expression
l = Lambda

v :: Var -> Expression
v = Var

true :: Expression
false :: Expression
not :: Expression
and :: Expression
or :: Expression
true = readExpression "\\a.\\b.a"
--true = l "a" $ l "b" $ v "a"
false = readExpression "\\a.\\b.b"
--false = l "a" $ l "b" $ v "b"
not = l "a" $ v "a" :$: false :$: true
and = l "a" $ l "b" $ v "a" :$: v "b" :$: false
or = l "a" $ l "b" $ v "a" :$: true :$: v "b"

fromInt :: Int -> Expression
fromInt n = l "f" $ l "x" $ foldr ($) (v "x") $ replicate n (v "f" :$:)

toInt :: Expression -> Int
toInt (Lambda _ (Lambda _ e)) = toInt' e
  where
    toInt' (Var _)   = 0
    toInt' (_ :$: x) = 1 + toInt' x
    toInt' _         = error "not a Church literal"
toInt _ = error "not a Church literal"

inc :: Expression
isZero :: Expression
isEven :: Expression
add :: Expression
mul :: Expression
pow :: Expression
--pow' :: Expression
inc = l "n" $ l "f" $ l "x" $ v "f" $$ (v "n" $$ v "f" $$ v "x")
isZero = l "n" $ v "n" $$ (l "x" false) $$ true
isEven = l "n" $ v "n" $$ not $$ true
add = l "a" $ l "b" $ l "f" $ l "x" $ v "a" $$ v "f" $$ (v "b" $$ v "f" $$ v "x")
mul = l "a" $ l "b" $ v "a" $$ (add $$ v "b") $$ fromInt 0
pow = l "a" $ l "b" $ v "b" $$ v "a"
--pow' = l "a" $ l "b" $ v "b" $$ (mul $$ v "a") $$ fromInt 1

pair :: Expression
first :: Expression
second :: Expression
dec :: Expression
sub :: Expression
pair = readExpression $ "\\a.\\b.\\f. f a b"
first = l "p" $ v "p" $$ true
second = l "p" $ v "p" $$ false
dec = l "n" $ first $$ (v "n" $$ (l "p" $ pair $$ (second $$ v "p") $$ (inc $$ (second $$ v "p"))) $$ (pair $$ fromInt 0 $$ fromInt 0))
sub = l "a" $ l "b" $ v "b" $$ dec $$ v "a"
