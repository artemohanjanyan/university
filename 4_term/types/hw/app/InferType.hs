module Main where

import           Text.Parsec

import           Simple.Expression
import           Simple.TypeInference

main :: IO ()
main = interact (unlines . map f . lines)
  where
    f str = case parse (expressionParser <* eof) "input" str of
        Left parseError -> show parseError
        Right expr -> case inferType expr of
            Just (exprType, context) -> let
                varTypes = map (\(var, varType) -> var ++ ":" ++ show varType) context
              in
                init $ unlines (show exprType : varTypes)
            Nothing -> "Лямбда-выражение не имеет типа."
