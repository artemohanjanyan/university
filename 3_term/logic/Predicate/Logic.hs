module Predicate.Logic where

import Predicate.Types
import Predicate.Base
import Predicate.CopyPaste

import Data.Ord (comparing)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Control.Monad

----------------
-- Homework 4 --
----------------

annotate :: Proof -> AnnotProof
annotate (Proof assumps target stmts) = AnnotProof assumps target annotStmts
  where
    annotStmts = reverse $ snd $ List.foldl' insert (Set.empty, []) stmts

    freeVars = if null assumps then Set.empty else getFreeVars $ last assumps

    assumpSet = Set.fromList assumps

    insert :: (Set.Set Stmt, [AnnotStmt]) -> Stmt -> (Set.Set Stmt, [AnnotStmt])
    insert (stmtsS, stmts') stmt = (Set.insert stmt stmtsS, annotated:stmts')
      where
        annotated = AnnotStmt stmt $ annotStmt stmt
        annotStmt stmt
            | Just _ <- List.find (flip matchSchema stmt) schemas = Axiom
            | Just _ <- List.find (== stmt)               axioms  = Axiom
            | matchInduction stmt
                = Axiom
            | Set.member stmt assumpSet
                = Assumption
            | Just (AnnotStmt impl@(BinStmt Impl premise _) _)
                    <- List.find isCons stmts'
                = MP premise impl
            | (BinStmt Impl premise (QuantStmt All var cons)) <- stmt
            , Set.member (BinStmt Impl premise cons) stmtsS
                = if Set.member var (getFreeVars premise)
                    then NotProoved $
                        ": переменная " ++ show var ++ " входит свободно в формулу " ++ show premise
                    else if Set.member var freeVars
                    then NotProoved $
                        ": используется правило с квантором по переменной " ++ show var
                        ++ ", входящей свободно в допущение " ++ show (last assumps)
                    else Quantification All
            | (BinStmt Impl (QuantStmt Exists var premise) cons) <- stmt
            , Set.member (BinStmt Impl premise cons) stmtsS
                = if Set.member var (getFreeVars cons)
                    then NotProoved $
                        ": переменная " ++ show var ++ " входит свободно в формулу " ++ show cons
                    else if Set.member var freeVars
                    then NotProoved $
                        ": используется правило с квантором по переменной " ++ show var
                        ++ ", входящей свободно в допущение " ++ show (last assumps)
                    else Quantification Exists
            | match11Schema stmt
            , (BinStmt Impl (QuantStmt All var stmt1) stmt2) <- stmt
                = Axiom
            | match12Schema stmt
            , (BinStmt Impl stmt1 (QuantStmt Exists var stmt2)) <- stmt
                = Axiom
            | not (match11Schema stmt) && match11SchemaWeak stmt
            , (BinStmt Impl (QuantStmt All var stmt1) stmt2) <- stmt
            , Just (Just substitution) <- findSubstitution var stmt1 stmt2
                = NotProoved $ ": терм " ++ show substitution
                    ++ " не свободен для подстановки в формулу " ++ show stmt1
                    ++ " вместо переменной " ++ show var ++ "."
            | not (match12Schema stmt) && match12SchemaWeak stmt
            , (BinStmt Impl stmt1 (QuantStmt Exists var stmt2)) <- stmt
            , Just (Just substitution) <- findSubstitution var stmt2 stmt1
                = NotProoved $ ": терм " ++ show substitution
                    ++ " не свободен для подстановки в формулу " ++ show stmt2
                    ++ " вместо переменной " ++ show var ++ "."
            | otherwise
                = NotProoved ""

        isCons (AnnotStmt (BinStmt Impl premise cons) _)
            | stmt == cons && Set.member premise stmtsS = True
        isCons _                                        = False

check :: Proof -> Bool
check proof = target == getStmt (last stmts) && all (prooved . getAnnot) stmts
  where
    AnnotProof _ target stmts = annotate proof
    prooved (NotProoved _) = False
    prooved _              = True

deduction :: Proof -> Proof
deduction proof@(Proof assumps target _) =
        Proof (init assumps) (BinStmt Impl alpha target) stmts
  where
    alpha = last assumps
    alphaStr = "(" ++ show alpha ++ ")"
    AnnotProof _ _ annotStmts = annotate proof

    stmts = concatMap iterate annotStmts
    iterate (AnnotStmt stmt annot)
        | stmt == alpha = map (substitute $ Map.singleton 'A' alphaStr) helper
        | MP premise impl <- annot
        , a <- BinStmt Impl alpha premise
        , b <- BinStmt Impl alpha impl
        , c <- BinStmt Impl alpha stmt =
                [ BinStmt Impl a (BinStmt Impl b c)
                , BinStmt Impl b c
                , c
                ]
        | Quantification All <- annot
        , (BinStmt Impl b (QuantStmt All x c)) <- stmt
            = map (substitute $ Map.fromList
                [('A', alphaStr), ('B', "(" ++ show b ++ ")"), ('C', "(" ++ show c ++ ")"), ('x', show x)])
                allProof
        | Quantification Exists <- annot
        , (BinStmt Impl (QuantStmt Exists x b) c) <- stmt
            = map (substitute $ Map.fromList
                [('A', alphaStr), ('B', "(" ++ show b ++ ")"), ('C', "(" ++ show c ++ ")"), ('x', show x)])
                existsProof
        | otherwise =
                [ BinStmt Impl stmt (BinStmt Impl alpha stmt)
                , stmt
                , BinStmt Impl alpha stmt
                ]

    helper = map parseStmt
        [ "A -> (A -> A) -> A"
        , "A -> A -> A"
        , "(A -> (A -> A)) -> (A -> (A -> A) -> A) -> (A -> A)"
        , "(A -> (A -> A) -> A) -> (A -> A)"
        , "(A -> A)"
        ]

    existsProof = map parseStmt
        [ "B->A->B"
        , "A->B->C"
        , "(A->B->C)->B->A->B->C"
        , "B->A->B->C"
        , "(A->B)->(A->B->C)->A->C"
        , "((A->B)->(A->B->C)->A->C)->B->(A->B)->(A->B->C)->A->C"
        , "B->(A->B)->(A->B->C)->A->C"
        , "(B->A->B)->(B->(A->B)->(A->B->C)->A->C)->B->(A->B->C)->A->C"
        , "(B->(A->B)->(A->B->C)->A->C)->B->(A->B->C)->A->C"
        , "B->(A->B->C)->A->C"
        , "(B->A->B->C)->(B->(A->B->C)->A->C)->B->A->C"
        , "(B->(A->B->C)->A->C)->B->A->C"
        , "B->A->C"
        , "?xB->A->C"
        , "A->?xB->A"
        , "?xB->A->C"
        , "(?xB->A->C)->A->?xB->A->C"
        , "A->?xB->A->C"
        , "(?xB->A)->(?xB->A->C)->?xB->C"
        , "((?xB->A)->(?xB->A->C)->?xB->C)->A->(?xB->A)->(?xB->A->C)->?xB->C"
        , "A->(?xB->A)->(?xB->A->C)->?xB->C"
        , "(A->?xB->A)->(A->(?xB->A)->(?xB->A->C)->?xB->C)->A->(?xB->A->C)->?xB->C"
        , "(A->(?xB->A)->(?xB->A->C)->?xB->C)->A->(?xB->A->C)->?xB->C"
        , "A->(?xB->A->C)->?xB->C"
        , "(A->?xB->A->C)->(A->(?xB->A->C)->?xB->C)->A->?xB->C"
        , "(A->(?xB->A->C)->?xB->C)->A->?xB->C"
        , "A->?xB->C"
        ]

    allProof = map parseStmt
        [ "A->B->C"
        , "A&B->A"
        , "A&B->B"
        , "A->B->C"
        , "(A->B->C)->A&B->A->B->C"
        , "A&B->A->B->C"
        , "(A&B->A)->(A&B->A->B->C)->A&B->B->C"
        , "(A&B->A->B->C)->A&B->B->C"
        , "A&B->B->C"
        , "(A&B->B)->(A&B->B->C)->(A&B->C)"
        , "(A&B->B->C)->(A&B->C)"
        , "A&B->C"
        , "A&B->@xC"
        , "A->B->A&B"
        , "A&B->@xC"
        , "(A&B->@xC)->A->A&B->@xC"
        , "A->A&B->@xC"
        , "(A&B->@xC)->B->A&B->@xC"
        , "((A&B->@xC)->B->A&B->@xC)->A->(A&B->@xC)->B->A&B->@xC"
        , "A->(A&B->@xC)->B->A&B->@xC"
        , "(A->A&B->@xC)->(A->((A&B->@xC)->B->A&B->@xC))->A->B->A&B->@xC"
        , "(A->((A&B->@xC)->B->A&B->@xC))->A->B->A&B->@xC"
        , "A->B->A&B->@xC"
        , "(B->A&B)->(B->A&B->@xC)->B->@xC"
        , "((B->A&B)->(B->A&B->@xC)->B->@xC)->A->(B->A&B)->(B->A&B->@xC)->B->@xC"
        , "A->(B->A&B)->(B->A&B->@xC)->B->@xC"
        , "(A->B->A&B)->(A->(B->A&B)->(B->A&B->@xC)->B->@xC)->A->(B->A&B->@xC)->B->@xC"
        , "(A->(B->A&B)->(B->A&B->@xC)->B->@xC)->A->(B->A&B->@xC)->B->@xC"
        , "A->(B->A&B->@xC)->B->@xC"
        , "(A->B->A&B->@xC)->(A->(B->A&B->@xC)->B->@xC)->A->B->@xC"
        , "(A->(B->A&B->@xC)->B->@xC)->A->B->@xC"
        , "A->B->@xC"
        ]

substitute :: Map.Map Char String -> Stmt -> Stmt
substitute subs stmt = parseStmt $ concatMap f $ show stmt
  where
    f s
        | Just newS <- Map.lookup s subs = newS
        | otherwise                      = [s]
