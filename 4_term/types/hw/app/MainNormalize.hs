module Main where

import           Text.Parsec

import           Expression
import           Reduction

main = do
    --content <- getContents
    content <- getLine
    case parse expressionParser "input" content of
        Left error -> putStrLn $ show error
        Right expr -> do
            putStrLn $ show $ normalize expr
