module Statement.Logic
    ( annotate
    , deduction
    , findCounterExample
    , prooveCorrect
    ) where

import Statement.Types
import Statement.Proofs

import Data.Ord (comparing)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Control.Monad

----------------
-- Homework 1 --
----------------

schemas :: [(Int, Stmt)]
schemas = zip [1..] $ map parseStmt l
  where
    l =
        [ "a -> b -> a"
        , "(a -> b) -> (a -> b -> c) -> (a -> c)"
        , "a -> b -> (a & b)"
        , "a & b -> a"
        , "a & b -> b"
        , "a -> a | b"
        , "b -> a | b"
        , "(a -> c) -> (b -> c) -> (a | b -> c)"
        , "(a -> b) -> (a -> !b) -> !a"
        , "!!a -> a"
        ]

matchSchema :: Stmt -> Stmt -> Bool
matchSchema schema stmt = Maybe.isJust $ dfs schema stmt Map.empty
  where
    dfs :: Stmt -> Stmt -> Map.Map String Stmt -> Maybe (Map.Map String Stmt)
    dfs (Var s) stmt map
        | not $ Map.member s map = Just $ Map.insert s stmt map
        | map Map.! s == stmt    = Just map
        | otherwise              = Nothing
    dfs (Neg a) (Neg b) map = dfs a b map
    dfs (BinOp oprtr1 left1 right1) (BinOp oprtr2 left2 right2) map
        | oprtr1 == oprtr2 =
          do
            map1 <- dfs left1 left2 map
            dfs right1 right2 map1
    dfs _ _ _ = Nothing

annotate :: Proof -> AnnotProof
annotate (Proof assumps target stmts) = AnnotProof assumps target annotStmts
  where
    annotStmts = reverse $ snd $ List.foldl' insert (Map.empty, []) $ zip [1..] stmts

    assumpMap = Map.fromList $ zip assumps [1..]

    --        Annotated statements                     Statement to annotate
    insert :: (Map.Map Stmt AnnotStmt, [AnnotStmt]) -> (Int, Stmt) ->
            (Map.Map Stmt AnnotStmt, [AnnotStmt])
    insert (stmts, stmtsL) (n, stmt) = (Map.insert stmt annotated stmts, annotated:stmtsL)
      where
        annotated = AnnotStmt n stmt $ annotStmt stmt
        annotStmt stmt
            | (Just (AnnotStmt _ _ annot)) <- Map.lookup stmt stmts,
              annot /= NotProoved = annot
            | Just (n, _) <-
                    List.find (flip matchSchema stmt . snd) schemas = Schema n
            | Just n <- Map.lookup stmt assumpMap = Assumption n
            | (n1, n2):_ <-
                    map Maybe.fromJust $
                    filter Maybe.isJust $
                    map findPremise $
                    filter (compCons stmt) $ stmtsL =
                MP n1 n2
            | otherwise = NotProoved

        findPremise (AnnotStmt n2 (BinOp Impl premise _) _)
            | (Just (AnnotStmt n1 _ _)) <- Map.lookup premise stmts =
                    Just (n1, n2)
        findPremise _ = Nothing
        compCons stmt (AnnotStmt _ (BinOp Impl _ cons) _) = stmt == cons
        compCons _    _                                   = False

check :: Proof -> Bool
check proof = target == getStmt (last stmts) && all ((/= NotProoved) . getAnnot) stmts
  where
    AnnotProof _ target stmts = annotate proof

----------------
-- Homework 2 --
----------------

substitute :: Map.Map String Stmt -> Stmt -> Stmt
substitute subs (BinOp op left right) =
        BinOp op (substitute subs left) (substitute subs right)
substitute subs (Neg stmt) = Neg $ substitute subs stmt
substitute subs var@(Var name)
    | Just to <- Map.lookup name subs = to
    | otherwise                       = var

deduction :: Proof -> Proof
deduction proof@(Proof assumps target _) =
        Proof (init assumps) (BinOp Impl alpha target) stmts
  where
    alpha = last assumps
    AnnotProof _ _ annotStmts = annotate proof
    stmtMap = Map.fromList $ map (\(AnnotStmt n stmt _) -> (n, stmt)) annotStmts

    stmts = concat $ reverse $ List.foldl' iterate [] annotStmts
    iterate tmp (AnnotStmt _ stmt annot)
        | stmt == alpha = map (substitute $ Map.singleton "A" alpha) helper : tmp
        | MP n1 n2 <- annot
        , Just premise <- Map.lookup n1 stmtMap
        , Just impl <- Map.lookup n2 stmtMap
        , a <- BinOp Impl alpha premise
        , b <- BinOp Impl alpha impl
        , c <- BinOp Impl alpha stmt =
                [ BinOp Impl a (BinOp Impl b c)
                , BinOp Impl b c
                , c
                ] : tmp
        | otherwise =
                [ BinOp Impl stmt (BinOp Impl alpha stmt)
                , stmt
                , BinOp Impl alpha stmt
                ] : tmp

    helper = map parseStmt
        [ "A -> (A -> A) -> A"
        , "A -> A -> A"
        , "(A -> (A -> A)) -> (A -> (A -> A) -> A) -> (A -> A)"
        , "(A -> (A -> A) -> A) -> (A -> A)"
        , "(A -> A)"
        ]

----------------
-- Homework 3 --
----------------

proofSubstitute :: Map.Map String Stmt -> Proof -> Proof
proofSubstitute subs (Proof assumps target stmts) = Proof assumps' target' stmts'
  where
    assumps' = map (substitute subs) assumps
    target' = substitute subs target
    stmts' = map (substitute subs) stmts

value :: Stmt -> Map.Map String Bool -> Bool
value (Var name)               values = values Map.! name
value (Neg stmt)               values = runNeg $ value stmt values
value (BinOp oprtr left right) values = runOprtr oprtr lValue rValue
  where
    lValue = value left values
    rValue = value right values

getVars :: Stmt -> Set.Set String
getVars = getVars' Set.empty
  where
    getVars' vars (Var name) = Set.insert name vars
    getVars' vars (Neg stmt) = getVars' vars stmt
    getVars' vars (BinOp oprtr left right) = getVars' (getVars' vars left) right

findCounterExample :: Stmt -> Maybe [(String, Bool)]
findCounterExample stmt = List.find (not . value stmt . Map.fromList) values
  where
    vars = Set.toList $ getVars stmt
    values = map (zip vars) $ replicateM (length vars) [False, True]

valuedProof :: Stmt -> Map.Map String Bool -> (Bool, Proof)
valuedProof (BinOp oprtr left right) values =
        (bValue, lProof <++> rProof <++> bProof)
  where
    (lValue, lProof) = valuedProof left values
    (rValue, rProof) = valuedProof right values
    (bValue, bProof) =
            fmap (proofSubstitute (Map.fromList [("A", left), ("B", right)])) $
            baseProof oprtr lValue rValue
valuedProof (Neg stmt) values = (bValue, sProof <++> bProof)
  where
    (sValue, sProof) = valuedProof stmt values
    (bValue, bProof) =
            fmap (proofSubstitute (Map.fromList [("A", stmt)])) $
            negProof sValue
valuedProof stmt@(Var name) values = (isTrue, Proof assumps target [target])
  where
    assumps = map (\(name, value) -> f name value) $ Map.toList values
    isTrue = values Map.! name
    target = f name isTrue
    f name value = (if value then id else Neg) $ Var name

removeAssumption :: [Proof] -> Proof
removeAssumption [proof1@(Proof a1 target _), proof2@(Proof a2 _ _)] =
        deduction proof1 <++> deduction proof2 <++> 
        proofSubstitute (Map.singleton "A" premise) excludedMiddle <++>
        proofSubstitute (Map.fromList [("P", premise), ("A", target)]) removalEnding
  where
    intersect (Neg a) b | a == b = a
    intersect a (Neg b) | a == b = a
    premise = intersect (last a1) (last a2)

prooveCorrect :: Stmt -> Proof
prooveCorrect stmt = head (foldl (flip ($)) valuedProofs (replicate (length vars) step))
  where
    vars = Set.toList $ getVars stmt
    values = map (zip vars) $ replicateM (length vars) [False, True]
    valuedProofs :: [Proof]
    valuedProofs = map (snd . valuedProof stmt . Map.fromList) values

    excludedMiddles = map (flip proofSubstitute excludedMiddle . Map.singleton "A" . Var) vars

    step :: [Proof] -> [Proof]
    step proofs = map removeAssumption pairs
      where
        pairs = List.groupBy (\(Proof a _ _) (Proof b _ _) -> init a == init b) proofs
