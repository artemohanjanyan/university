module Predicate.CopyPaste
    ( match11SchemaWeak
    , match12SchemaWeak
    ) where

import Predicate.Types
import Predicate.Base

import qualified Data.Maybe as Maybe

substWeak :: Var -> Term -> Stmt -> Maybe Stmt
substWeak var term stmt = substVar stmt
  where
    substVar :: Stmt -> Maybe Stmt
    substVar (BinStmt op left right)
        | Just newLeft <- (substVar left)
        , Just newRight <- (substVar right)
            = Just $ BinStmt op newLeft newRight
    substVar (Neg stmt)
        | Just newStmt <- substVar stmt
            = Just $ Neg newStmt
    substVar (Predicate name terms)
        | newTerms <- map substVar' terms
        , all Maybe.isJust newTerms
            = Just $ Predicate name $ map Maybe.fromJust newTerms
    substVar (l :=: r)
        | Just newL <- substVar' l
        , Just newR <- substVar' r
            = Just $ newL :=: newR
    substVar stmt@(QuantStmt quant qVar qStmt)
        | var == qVar = Just stmt
        | Just newStmt <- substVar qStmt
            = Just $ QuantStmt quant qVar newStmt
    substVar _ = Nothing

    substVar' :: Term -> Maybe Term
    substVar' (BinTerm op l r)
        | Just newL <- substVar' l
        , Just newR <- substVar' r
            = Just $ BinTerm op newL newR
    substVar' (FuncTerm name terms)
        | newTerms <- map (substVar') terms
        , all Maybe.isJust newTerms
            = Just $ FuncTerm name $ map Maybe.fromJust newTerms
    substVar' varTerm@(VarTerm var')
        | var == var'
            = Just term
        | var /= var'
            = Just varTerm
    substVar' (Succ term)
        | Just newTerm <- substVar' term
            = Just $ Succ newTerm
    substVar' Zero = Just Zero
    substVar' _ = Nothing

matchSubstitutionWeak :: Var -> Stmt -> Stmt -> Bool
matchSubstitutionWeak var stmt1 stmt2
    | Just term <- findSubstitution var stmt1 stmt2
        = if Maybe.isNothing term then True else
            case substWeak var (Maybe.fromJust term) stmt1 of
                Just newStmt -> newStmt == stmt2
                _            -> False
    | otherwise        = False

match11SchemaWeak :: Stmt -> Bool
match11SchemaWeak (BinStmt Impl
        (QuantStmt All var stmt1)
        stmt2)
    | matchSubstitutionWeak var stmt1 stmt2 = True
match11SchemaWeak _                         = False

match12SchemaWeak :: Stmt -> Bool
match12SchemaWeak (BinStmt Impl
        stmt1
        (QuantStmt Exists var stmt2))
    | matchSubstitutionWeak var stmt2 stmt1 = True
match12SchemaWeak _                         = False
