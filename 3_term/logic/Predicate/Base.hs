module Predicate.Base
    ( getFreeVars
    , findSubstitution
    , schemas
    , axioms
    , matchSchema
    , matchInduction
    , match11Schema
    , match12Schema
    , substIfFree
    ) where

import Predicate.Types

import Data.Ord (comparing)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Control.Monad

getFreeVars :: Stmt -> Set.Set Var
getFreeVars (BinStmt _ l r) = Set.union (getFreeVars l) (getFreeVars r)
getFreeVars (Neg arg) = getFreeVars arg
getFreeVars (Predicate _ terms) =
        foldl Set.union Set.empty $ map getFreeVars' terms
getFreeVars (l :=: r) = Set.union (getFreeVars' l) (getFreeVars' r)
getFreeVars (QuantStmt _ var arg) = Set.delete var (getFreeVars arg)

getFreeVars' :: Term -> Set.Set Var
getFreeVars' (BinTerm _ l r) = getFreeVars' l `Set.union` getFreeVars' r
getFreeVars' (FuncTerm _ terms) =
        foldl Set.union Set.empty $ map getFreeVars' terms
getFreeVars' (VarTerm var) = Set.singleton var
getFreeVars' (Succ term) = getFreeVars' term
getFreeVars' _ = Set.empty

rawSubstVar :: Var -> Term -> Stmt -> Stmt
rawSubstVar var term (BinStmt op left right) =
        BinStmt op (rawSubstVar var term left) (rawSubstVar var term right)
rawSubstVar var term (Neg stmt) = Neg $ rawSubstVar var term stmt
rawSubstVar var term (Predicate name terms) =
        Predicate name $ map (rawSubstVar' var term) terms
rawSubstVar var term (l :=: r) = rawSubstVar' var term l :=: rawSubstVar' var term r
rawSubstVar var term stmt@(QuantStmt quant qVar qStmt)
    | var == qVar = stmt
    | otherwise   = QuantStmt quant qVar $ rawSubstVar var term qStmt

rawSubstVar' :: Var -> Term -> Term -> Term
rawSubstVar' var term (BinTerm op l r) =
        BinTerm op (rawSubstVar' var term l) (rawSubstVar' var term r)
rawSubstVar' var term (FuncTerm name terms) =
        FuncTerm name $ map (rawSubstVar' var term) terms
rawSubstVar' var term (VarTerm var')
    | var == var' = term
rawSubstVar' var term (Succ term') = Succ $ rawSubstVar' var term term'
rawSubstVar' _ _ term = term

schemas :: [Stmt]
schemas = map parseStmt $
    [ "A -> B -> A"
    , "(A -> B) -> (A -> B -> C) -> (A -> C)"
    , "A -> B -> (A & B)"
    , "A & B -> A"
    , "A & B -> B"
    , "A -> A | B"
    , "B -> A | B"
    , "(A -> C) -> (B -> C) -> (A | B -> C)"
    , "(A -> B) -> (A -> !B) -> !A"
    , "!!A -> A"
    ]

axioms = map parseStmt $
    [ "a = b -> a' = b'"
    , "a = b -> a = c -> b = c"
    , "a' = b' -> a = b"
    , "!a' = 0"
    , "a + b' = (a + b)'"
    , "a + 0 = a"
    , "a * 0 = 0"
    , "a * b' = a * b + a"
    ]

matchSchema :: Stmt -> Stmt -> Bool
matchSchema schema stmt = Maybe.isJust $ dfs schema stmt Map.empty
  where
    dfs :: Stmt -> Stmt -> Map.Map String Stmt -> Maybe (Map.Map String Stmt)
    dfs (BinStmt oprtr1 left1 right1) (BinStmt oprtr2 left2 right2) map
        | oprtr1 == oprtr2 =
          do
            map1 <- dfs left1 left2 map
            dfs right1 right2 map1
    dfs (Neg a) (Neg b) map = dfs a b map
    dfs (Predicate s []) stmt map
        | not $ Map.member s map = Just $ Map.insert s stmt map
        | map Map.! s == stmt    = Just map
    dfs _ _ _ = Nothing

matchInduction :: Stmt -> Bool
matchInduction (BinStmt Impl
        (BinStmt Conj
            a
            (QuantStmt All x
                (BinStmt Impl b c)))
        d)
    | b == d
    , freeVars <- getFreeVars b
    , Set.member x freeVars
    , a == rawSubstVar x Zero b
    , c == rawSubstVar x (Succ $ VarTerm x) b
    = True
matchInduction _ = False

findSubstitution :: Var -> Stmt -> Stmt -> Maybe (Maybe Term)
findSubstitution var (BinStmt op1 l1 r1) (BinStmt op2 l2 r2)
    | op1 == op2 =
      case findSubstitution var l1 l2 of
        ans@(Just (Just _)) -> ans
        Just Nothing -> findSubstitution var r1 r2
        Nothing -> Nothing
findSubstitution var (Neg a) (Neg b) = findSubstitution var a b
findSubstitution var (Predicate name1 terms1) (Predicate name2 terms2)
    | name1 == name2
    , l <- zipWith (findSubstitution' var) terms1 terms2
    , all (Maybe.isJust) l
        = if null l then Just Nothing else
            case List.find (Maybe.isJust . Maybe.fromJust) l of
                Just term -> term
                _         -> Just Nothing
findSubstitution var (l1 :=: r1) (l2 :=: r2)
    | maybe@(Just term) <- findSubstitution' var l1 l2
        = if Maybe.isJust term then maybe else findSubstitution' var r1 r2
findSubstitution var (QuantStmt q1 var1 stmt1) (QuantStmt q2 var2 stmt2)
    | q1 == q2 && var1 == var2
        = if var /= var1
            then findSubstitution var stmt1 stmt2
            else if (stmt1 == stmt2)
                then Just Nothing
                else Nothing
findSubstitution _ _ _ = Nothing

findSubstitution' :: Var -> Term -> Term -> Maybe (Maybe Term)
findSubstitution' var (BinTerm op1 l1 r1) (BinTerm op2 l2 r2)
    | op1 == op2 =
      case findSubstitution' var l1 l2 of
        ans@(Just (Just _)) -> ans
        Just Nothing -> findSubstitution' var r1 r2
        Nothing -> Nothing
findSubstitution' var (FuncTerm name1 terms1) (FuncTerm name2 terms2)
    | name1 == name2 
    , l <- zipWith (findSubstitution' var) terms1 terms2
    , all (Maybe.isJust) l
    , maybeTerm <- List.find (Maybe.isJust . Maybe.fromJust) l
        = case maybeTerm of
            Just term -> term
            Nothing   -> Just Nothing
findSubstitution' var (VarTerm var1) term
    | var == var1 = Just $ Just term
    | VarTerm var2 <- term
    , var1 == var2
        = Just Nothing
findSubstitution' var (Succ term1) (Succ term2) = findSubstitution' var term1 term2
findSubstitution' _ Zero Zero = Just Nothing
findSubstitution' _ _ _ = Nothing

substIfFree :: Var -> Term -> Stmt -> Maybe Stmt
substIfFree var term stmt = substVar Set.empty stmt
  where
    termVars = getFreeVars' term

    substVar :: Set.Set Var -> Stmt -> Maybe Stmt
    substVar bound (BinStmt op left right)
        | Just newLeft <- (substVar bound left)
        , Just newRight <- (substVar bound right)
            = Just $ BinStmt op newLeft newRight
    substVar bound (Neg stmt)
        | Just newStmt <- substVar bound stmt
            = Just $ Neg newStmt
    substVar bound (Predicate name terms)
        | newTerms <- map (substVar' bound) terms
        , all Maybe.isJust newTerms
            = Just $ Predicate name $ map Maybe.fromJust newTerms
    substVar bound (l :=: r)
        | Just newL <- substVar' bound l
        , Just newR <- substVar' bound r
            = Just $ newL :=: newR
    substVar bound stmt@(QuantStmt quant qVar qStmt)
        | var == qVar = Just stmt
        | Just newStmt <- substVar (Set.insert qVar bound) qStmt
            = Just $ QuantStmt quant qVar newStmt
    substVar _ _ = Nothing

    substVar' :: Set.Set Var -> Term -> Maybe Term
    substVar' bound (BinTerm op l r)
        | Just newL <- substVar' bound l
        , Just newR <- substVar' bound r
            = Just $ BinTerm op newL newR
    substVar' bound (FuncTerm name terms)
        | newTerms <- map (substVar' bound) terms
        , all Maybe.isJust newTerms
            = Just $ FuncTerm name $ map Maybe.fromJust newTerms
    substVar' bound varTerm@(VarTerm var')
        | var == var'
        , Set.null $ Set.intersection termVars bound
            = Just term
        | var /= var'
            = Just varTerm
    substVar' bound (Succ term)
        | Just newTerm <- substVar' bound term
            = Just $ Succ newTerm
    substVar' _ Zero = Just Zero
    substVar' _ _ = Nothing

matchSubstitution :: Var -> Stmt -> Stmt -> Bool
matchSubstitution var stmt1 stmt2
    | Just term <- findSubstitution var stmt1 stmt2
        = if Maybe.isNothing term then True else
            case substIfFree var (Maybe.fromJust term) stmt1 of
                Just newStmt -> newStmt == stmt2
                _            -> False
    | otherwise        = False

match11Schema :: Stmt -> Bool
match11Schema (BinStmt Impl
        (QuantStmt All var stmt1)
        stmt2)
    | matchSubstitution var stmt1 stmt2 = True
match11Schema _                         = False

match12Schema :: Stmt -> Bool
match12Schema (BinStmt Impl
        stmt1
        (QuantStmt Exists var stmt2))
    | matchSubstitution var stmt2 stmt1 = True
match12Schema _                         = False
