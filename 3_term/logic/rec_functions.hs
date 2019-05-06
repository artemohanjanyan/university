{-# LANGUAGE FlexibleContexts #-}

import           Control.Applicative ((*>), (<*))
import           Data.Char           (isDigit)
import           Text.Parsec

data N0
    = Zero
    | Succ N0

toInt :: N0 -> Int
toInt Zero     = 0
toInt (Succ x) = 1 + toInt x

fromInt x = foldr (const Succ) Zero [1..x]
--fromInt x = foldr ($) Zero $ replicate x Succ

instance Show N0 where
    show = show . toInt

z :: [N0] -> N0
z [x] = Zero

n :: [N0] -> N0
n [x] = Succ x

p :: Int -> Int -> [N0] -> N0
p n j xs = xs !! (j - 1)

s :: ([N0] -> N0) -> [([N0] -> N0)] -> [N0] -> N0
s f gs xs = f $ map ($ xs) gs

r :: ([N0] -> N0) -> ([N0] -> N0) -> [N0] -> N0
r f g args =
  case y of
    Zero     -> f xs
    (Succ p) -> g $ xs ++ [p] ++ [r f g $ xs ++ [p]]
  where
    xs = init args
    y = last args

functionFrom s =
  case parse function "" s of
    Right f -> f

add = r (p 1 1) (s n [p 3 3])
mul = r z (s add [p 3 1, p 3 3])
--pow = r (s n [z]) (s mul [p 3 1, p 3 3])
pow = functionFrom "R<S<N, Z>, S<mul, Pi 3 1, Pi 3 3>>"
iff = s (r (p 2 1) (p 2 2)) [p 3 2, p 3 3, p 3 1]

customOperations =
    [ (add, symb "add" *> return add)
    , (mul, symb "mul" *> return mul)
    , (pow, symb "pow" *> return pow)
    , (iff, symb "if" *> return iff)
    ]

------------
-- Parser --
------------

number :: Parsec String () N0
number = (<?> "number") $
  do
    cs <- many1 $ satisfy isDigit
    many space
    return $ fromInt $ read cs

expression :: Parsec String () N0
expression = many space *>
  do
    f <- function
    symb "("
    xs <- number `sepBy` comma
    symb ")"
    return $ f xs

function :: Parsec String () ([N0] -> N0)
--many space *>
function = foldr1 (<|>) $
    (   do
        symb "Z"
        return z
    <|> do
        symb "N"
        return n
    <|> do
        symb "Pi"
        n <- many1 (satisfy isDigit) <* many space
        j <- many1 (satisfy isDigit) <* many space
        return $ p (read n) (read j)
    <|> do
        symb "S"
        symb "<"
        f <- function
        comma
        gs <- function `sepBy` comma
        symb ">"
        return (s f gs)
    <|> do
        symb "R"
        symb "<"
        f <- function
        comma
        g <- function
        symb ">"
        return (r f g)
    ) : map snd customOperations

symb s = string s <* many space
comma = symb ","

main = do
  s <- getLine
  let Right x = parse expression "" s
  putStrLn $ show x
