module Main where

import           Text.Parsec

import           Simple.Expression
import           Simple.Reduction

main :: IO ()
main = do
    content <- getLine
    case parse expressionParser "input" content of
        Left parseError -> putStrLn $ show parseError
        Right expr -> do
            putStrLn $ show $ normalize expr
