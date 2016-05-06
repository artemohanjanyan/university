module Predicate.Types
    ( Stmt(..)
    , Var(..)
    , Term(..)
    , Quant(..)
    , SOprtr(..)
    , TOprtr(..)

    , Proof(..)
    , (<++>)
    , (<:>)

    , Annot(..)
    , AnnotStmt(..)
    , AnnotProof(..)

    , parse
    , statementParser
    , proofParser
    , parseStmt
    ) where

import Text.Parsec
import Data.Char (isDigit, isSpace, isUpper, isLower)
import Data.List (intersperse, intercalate)
import Control.Applicative ((*>), (<*))

-----------
-- Types --
-----------

data Stmt
    = BinStmt { sOprtr :: SOprtr, left :: Stmt, right :: Stmt }
    | Neg Stmt
    | Predicate String [Term]
    | Term :=: Term
    | QuantStmt Quant Var Stmt
    deriving (Eq, Ord)
infix 7 :=:

data Var = Var String
    deriving (Eq, Ord)

data Term
    = BinTerm TOprtr Term Term
    | FuncTerm String [Term]
    | VarTerm Var
    | Zero
    | Succ Term
    deriving (Eq, Ord)

data Quant = All | Exists
    deriving (Eq, Ord)

data Assoc = AAssoc | ALeft | ARight
    deriving (Eq, Ord)

class Operator op where
    assoc :: op -> Assoc
    prior :: op -> Int

data SOprtr = Impl | Disj | Conj
    deriving (Eq, Ord)

instance Operator SOprtr where
    assoc Impl = ARight
    assoc _    = ALeft

    prior Impl = 1
    prior Disj = 2
    prior Conj = 3

data TOprtr = Add | Mult
    deriving (Eq, Ord)

instance Operator TOprtr where
    assoc _ = ALeft

    prior Add = 1
    prior Mult = 2

data Proof = Proof [Stmt] Stmt [Stmt]

(<++>) :: Proof -> Proof -> Proof
Proof assumps _ stmts1 <++> Proof _ target stmts2 =
        Proof assumps target (stmts1 ++ stmts2)

(<:>) :: Proof -> Stmt -> Proof
Proof assumps _ stmts1 <:> stmt2 =
        Proof assumps stmt2 (stmts1 ++ [stmt2])

data Annot
    = Assumption
    | Axiom
    | MP Stmt Stmt
    | Quantification Quant
    | NotProoved String
    deriving (Eq, Ord)

data AnnotStmt = AnnotStmt { getStmt :: Stmt, getAnnot :: Annot }

data AnnotProof = AnnotProof [Stmt] Stmt [AnnotStmt]

----------
-- Show --
----------

instance Show Annot where
    show Assumption = "Предп."
    show Axiom = "Сх. акс."
    show (MP _ _) = "M.P."
    show (Quantification quant) = show quant
    show (NotProoved str) = str

instance Show AnnotStmt where
    show (AnnotStmt stmt annot) =
        show stmt ++ " (" ++ show annot ++ ")"

instance Show AnnotProof where
    show (AnnotProof assumptions target statements) =
            concat (intersperse "," (map show assumptions)) ++ "|-" ++ show target ++ "\n" ++
            unlines (map show statements)

instance Show SOprtr where
    show Impl = "->"
    show Disj = "|"
    show Conj = "&"

instance Show Quant where
    show All = "@"
    show Exists = "?"

instance Show Var where
    show (Var s) = s

instance Show TOprtr where
    show Add = "+"
    show Mult = "*"

instance Show Term where
    show (BinTerm op a b) = show a ++ show op ++ showArg b
      where
        showArg arg@(BinTerm _ _ _) = "(" ++ show arg ++ ")"
        showArg arg = show arg

    show (FuncTerm name [])   = name
    show (FuncTerm name args) = name ++
            "(" ++ intercalate "," (map show args) ++ ")"
    show (VarTerm var) = show var
    show Zero = "0"
    show (Succ term@(BinTerm _ _ _)) = "(" ++ show term ++ ")'"
    show (Succ arg)            = show arg ++ "'"

showUnArg arg@(BinStmt _ _ _) = "(" ++ show arg ++ ")"
showUnArg arg = show arg

instance Show Stmt where
    show (Neg arg) = "!" ++ showUnArg arg

    show (BinStmt sOprtr left right) =
      showArg ARight left ++ "" ++ show sOprtr ++ "" ++ showArg ALeft right
        where
            showArg aassoc arg@(BinStmt asOprtr _ _)
                | prior sOprtr > prior asOprtr ||   -- Lower priority
                  prior sOprtr == prior asOprtr &&  -- Mismatches associativity
                  assoc sOprtr == assoc asOprtr &&  -- |
                  assoc sOprtr == aassoc            -- |
                    = "(" ++ show arg ++ ")"
            showArg _ arg = show arg

    show (Predicate name [])   = name
    show (Predicate name args) = name ++
            "(" ++ intercalate "," (map show args) ++ ")"
    show (a :=: b) = show a ++ "=" ++ show b

    show (QuantStmt quant var stmt) = show quant ++ show var ++ "(" ++ show stmt ++ ")"

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
        return $ BinStmt Impl

    disjunction = chainl1 conjunction disjOp
    disjOp = try $ do
        string "|"
        notFollowedBy $ string "-"
        return $ BinStmt Disj

    conjunction = chainl1 negation conjOp
    conjOp = do
        string "&"
        return $ BinStmt Conj

    negation =
        (   do
            string "!"
            arg <- negation
            return (Neg arg)
        <|> do
            quant <- (string "@" <|> string "?")
            var <- variable
            stmt <- negation
            return $ QuantStmt (case quant of "@" -> All; "?" -> Exists) var stmt
        <|> try (string "(" *> statementParser <* string ")")
        <|> predicate
        )

    variable = do
        l <- satisfy isLower <?> "variable"
        ds <- many $ satisfy isDigit
        return (Var (l:ds))

    predicate =
        (   do
            l <- satisfy isUpper <?> "predicate"
            ds <- many $ satisfy isDigit
            args <- optionMaybe $
                string "(" *> sepBy1 term (string ",") <* string ")"
            let
              args' = case args of
                Just list -> list
                Nothing   -> []
            return $ Predicate (l:ds) args'
        <|> do
            a <- term
            string "="
            b <- term
            return (a :=: b)
        )
    
    term = chainl1 mult addOp
    addOp = do
        string "+"
        return (BinTerm Add)

    mult = chainl1 primTerm multOp
    multOp = do
        string "*"
        return (BinTerm Mult)

    primTerm = do
        body <-
            (   do
                string "0"
                return Zero
            <|> string "(" *> term <* string ")"
            <|> try function
            <|> do
                var <- variable
                return $ VarTerm var
            )
        succs <- many $ string "'"
        return $ foldr (const Succ) body succs

    function = do
        l <- satisfy isLower <?> "function"
        ds <- many $ satisfy isDigit
        string "("
        args <- sepBy1 term $ string ","
        string ")"
        return $ FuncTerm (l:ds) args

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
