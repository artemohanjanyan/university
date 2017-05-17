module Main where

import           Text.Parsec

import           Simple.Expression

main :: IO ()
main = interact (unlines . map f . lines)
  where
    f str = case parse (expressionParser <* eof) "input" str of
        Left parseError -> show parseError
        Right expr      -> show expr
