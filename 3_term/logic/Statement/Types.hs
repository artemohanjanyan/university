module Statement.Types
    ( Stmt(..)
    , Oprtr(..)
    , runNeg
    , runOprtr

    , Annot(..)
    , AnnotStmt(..)

    , Proof(..)
    , (<++>)
    , AnnotProof(..)

    , parse
    , statementParser
    , proofParser
    , parseStmt
    ) where

import Text.Parsec
import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (intersperse)
import Control.Applicative ((*>), (<*))
import Data.Monoid

-----------
-- Types --
-----------

data Stmt
    = BinOp { oprtr :: Oprtr, left :: Stmt, right :: Stmt }
    | Neg Stmt
    | Var String
    deriving (Eq, Ord)

data Oprtr = Impl | Disj | Conj
    deriving (Eq, Ord)

runNeg :: Bool -> Bool
runNeg = not

runOprtr :: Oprtr -> Bool -> Bool -> Bool
runOprtr Impl a b = not a || b
runOprtr Disj a b = a || b
runOprtr Conj a b = a && b

data Assoc = AAssoc | ALeft | ARight
    deriving (Eq, Ord)

assoc Impl = ARight
--assoc _    = AAssoc
assoc _    = ALeft

prior Impl = 1
prior Disj = 2
prior Conj = 3

data Annot
    = Schema Int
    | Assumption Int
    | MP Int Int
    | NotProoved
    deriving (Eq, Ord)

data AnnotStmt
    = AnnotStmt { getIndex :: Int, getStmt :: Stmt, getAnnot :: Annot }
    deriving (Eq, Ord)

data Proof = Proof [Stmt] Stmt [Stmt]

(<++>) :: Proof -> Proof -> Proof
Proof assumps _ stmts1 <++> Proof _ target stmts2 =
        Proof assumps target (stmts1 ++ stmts2)

data AnnotProof = AnnotProof [Stmt] Stmt [AnnotStmt]

----------
-- Show --
----------

instance Show Oprtr where
    show Impl = "->"
    show Disj = "|"
    show Conj = "&"

instance Show Stmt where
    show (Var s)   = s
    show (Neg arg@(BinOp _ _ _)) = "!(" ++ show arg ++ ")"
    show (Neg arg) = "!" ++ show arg
    show (BinOp oprtr left right) =
      showArg ARight left ++ "" ++ show oprtr ++ "" ++ showArg ALeft right
        where
            showArg aassoc arg@(BinOp aoprtr _ _)
                | prior oprtr > prior aoprtr ||   -- Lower priority
                  prior oprtr == prior aoprtr &&  -- Mismatches associativity
                  assoc oprtr == assoc aoprtr &&  -- |
                  assoc oprtr == aassoc           -- |
                    = "(" ++ show arg ++ ")"
            showArg _ arg = show arg

instance Show Annot where
    show (Schema n) = "Сх. акс. " ++ show n
    show (Assumption n) = "Предп. " ++ show n
    show (MP a b) = "M.P. " ++ show a ++ ", " ++ show b
    show NotProoved = "Не доказано"

instance Show AnnotStmt where
    show (AnnotStmt n stmt annot) =
        "(" ++ show n ++ ") " ++ show stmt ++ " (" ++ show annot ++ ")"

instance Show AnnotProof where
    show (AnnotProof assumptions target statements) =
            concat (intersperse "," (map show assumptions)) ++ "|-" ++ show target ++ "\n" ++
            unlines (map show statements)

instance Show Proof where
    show (Proof assumptions target statements) =
            concat (intersperse "," (map show assumptions)) ++ "|-" ++ show target ++ "\n" ++
            unlines (map show statements)

------------
-- Parser --
------------

statementParser :: Parsec String () Stmt
statementParser = chainr1 disjunction implOp <?> "statement"
  where
    implOp = do
        string "->"
        return $ BinOp Impl

    disjunction = chainl1 conjunction disjOp
    disjOp = try $ do
        string "|"
        notFollowedBy $ string "-"
        return $ BinOp Disj

    conjunction = chainl1 negation conjOp
    conjOp = do
        string "&"
        return $ BinOp Conj

    negation =
        (   do
            l <- satisfy isAlpha <?> "variable"
            ds <- many $ satisfy isDigit <|> satisfy isAlpha
            return (Var (l:ds))
        <|> do
            string "!"
            arg <- negation
            return (Neg arg)
        <|> string "(" *> statementParser <* string ")"
        )

proofParser :: Parsec String () Proof
proofParser =
  do
    assumptions <- statementParser `sepBy` string ","
    string "|-"
    target <- statementParser <* string "\n"
    statements <- many $ statementParser <* string "\n"
    return $ Proof assumptions target statements

parseStmt :: String -> Stmt
parseStmt string =
  case parse statementParser "" $ filter (not . isSpace) string of
    Right stmt -> stmt
