import Text.Printf
import Text.Parsec
import Control.Applicative ((*>), (<*))
import Data.Char (isSpace)

import Statement.Logic
import Statement.Types

main = do
    content <- getContents
    case parse (proofParser <* eof) "input" $
            filter (\x -> not (isSpace x) || x == '\n') content of
        Left error -> putStrLn $ show error
        Right proof -> do
            putStr $ show $ annotate proof
