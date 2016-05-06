import Text.Printf
import Text.Parsec
import Control.Applicative ((*>), (<*))
import Data.Char (isSpace)
import Data.List
import Data.Maybe

import Predicate.Types
import Predicate.Logic

main = do
    content <- getContents
    case parse (proofParser <* eof) "input" $
            filter (\x -> not (isSpace x) || x == '\n') content of
        Left error -> putStrLn $ show error
        Right proof -> do
            let notProoved = getNotProoved $ annotate proof
            if notProoved == Nothing
                then if hasNoAssumps proof
                    then putStr $ show proof
                    else putStr $ show $ deduction proof
                else case fromJust notProoved of
                    (n, NotProoved str) -> putStr $
                        "Вывод некорректен начиная с формулы номер " ++ show n
                        ++ str

hasNoAssumps (Proof [] _ _) = True
hasNoAssumps _ = False

getNotProoved (AnnotProof _ _ stmts) =
        find (pred . snd) $ zip [1..] $ map (\(AnnotStmt _ annot) -> annot) stmts
  where
    pred (NotProoved _) = True
    pred _              = False
