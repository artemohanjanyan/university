module Main where

import           Control.Applicative
import           Text.Parsec

import           Simple.Expression
import           Simple.TypeInference

main = do
    content <- getLine
    case parse (expressionParser <* eof)"input" content of
        Left error -> putStrLn $ show error
        Right expr -> case inferType expr of
            Just (exprType, context) -> do
                putStrLn $ show exprType
                mapM_ (\(var, varType) -> putStrLn $ var ++ ":" ++ show varType) context
            Nothing -> putStrLn "Лямбда-выражение не имеет типа."
