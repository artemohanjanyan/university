module Main where

import           InputParser
import           LL1Printer

import           Text.Megaparsec
import           Text.Megaparsec.Error (parseErrorPretty)

main :: IO ()
main = do
    contents <- getContents
    case runParser inputParser "STDIN" contents of
        Left  e     -> putStr $ parseErrorPretty e
        Right input -> printParser input
