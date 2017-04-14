module Main where

import           Text.Parsec

import           Simple.Expression
import           Simple.TypeInference

main :: IO ()
main = do
    content <- getLine
    case parse (expressionParser <* eof)"input" content of
        Left parseError -> putStrLn $ show parseError
        Right expr -> case inferType expr of
            Just (exprType, context) -> do
                putStrLn $ show exprType
                mapM_ (\(var, varType) -> putStrLn $ var ++ ":" ++ show varType) context
            Nothing -> putStrLn "Лямбда-выражение не имеет типа."
