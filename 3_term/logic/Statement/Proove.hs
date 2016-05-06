import Text.Printf
import Text.Parsec
import Control.Applicative ((*>), (<*))
import Data.Char (isSpace)
import Data.List (intercalate)

import Statement.Logic
import Statement.Types

process :: Either Text.Parsec.ParseError Stmt -> IO ()
process (Left error) = print error
process (Right stmt)
    | Just counterExample <- findCounterExample stmt =
            putStrLn $ "Высказывание ложно при " ++ intercalate ", "
                (map (\(name, value) -> name ++ "=" ++ if value then "И" else "Л") counterExample)
    | otherwise = putStr $ show $ prooveCorrect stmt

main = do
    content <- getContents
    process $ parse (statementParser <* eof) "input" $ filter (not . isSpace) content
