module Main where

import           Text.Parsec

import           HM.Expression
import           HM.TypeInference

main :: IO ()
main = interact ((++ "\n") . f)
  where
    f str = case parse (hmParser <* eof) "input" str of
        Left parseError -> show parseError
        Right expr -> case hmInferType expr of
            Just t  -> show t
            Nothing -> "Выражение не имеет типа."
