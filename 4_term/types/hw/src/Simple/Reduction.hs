module Simple.Reduction where

import           Control.Monad.State.Lazy
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (mapMaybe)
import qualified Data.Set                 as Set

import           Simple.Expression

getFreeVars :: Expression -> Set.Set Var
getFreeVars (Lambda var expr) = Set.delete var $ getFreeVars expr
getFreeVars (expr1 :$: expr2) = Set.union (getFreeVars expr1) (getFreeVars expr2)
getFreeVars (Var var)         = Set.singleton var

getBoundVars :: Expression -> Var -> Set.Set Var
getBoundVars = get' Set.empty
  where
    get' :: Set.Set Var -> Expression -> Var -> Set.Set Var
    get' acc (Lambda var expr) free
        | var == free = Set.empty
        | otherwise   = get' (Set.insert var acc) expr free
    get' acc (expr1 :$: expr2) free = Set.union (get' acc expr1 free) (get' acc expr2 free)
    get' acc (Var var) free
        | var == free = acc
        | otherwise   = Set.empty

naiveSubstitute :: Expression -> Var -> Expression -> Either [Char] Expression
naiveSubstitute expr1 var expr2
    | Set.null intersection = Right $ subst expr1 var expr2
    | otherwise             = Left $ Set.findMin intersection
  where
    freeVars = getFreeVars expr2
    boundVars = getBoundVars expr1 var
    intersection = Set.intersection freeVars boundVars

    subst :: Expression -> Var -> Expression -> Expression
    subst expr@(Lambda var' expr') free otherExpr
        | var' == free = expr
        | otherwise    = Lambda var' $ subst expr' free otherExpr
    subst (expr1' :$: expr2') free otherExpr = subst expr1' free otherExpr :$: subst expr2' free otherExpr
    subst expr@(Var var') free otherExpr
        | var' == free = otherExpr
        | otherwise    = expr

rename :: Map.Map Var Var -> Expression -> Expression
rename = rename' Map.empty
  where
    rename' :: Map.Map Var Var -> Map.Map Var Var -> Expression -> Expression
    rename' act dict (Lambda var expr)
        | Map.member var dict = Lambda newVar $ rename' (Map.insert var newVar act) (Map.delete var dict) expr
        | Map.member var act  = Lambda var    $ rename' (Map.delete var act)        dict                  expr
        | otherwise           = Lambda var    $ rename' act                         dict                  expr
      where
        newVar = dict Map.! var
    rename' act dict (expr1 :$: expr2) = rename' act dict expr1 :$: rename' act dict expr2
    rename' act _ (Var var) = Var $ Map.findWithDefault var var act

applyRedex :: Expression -> Expression -> Expression
applyRedex (Lambda var expr1) expr2 = newExpr
  where
    freeVars = getFreeVars expr2
    boundVars = getBoundVars expr1 var

    makeRenaming :: Var -> State (Set.Set Var) (Maybe (Var, Var))
    makeRenaming name = do
        namespace <- get
        if Set.member name namespace
            then do
                let newName = make' name namespace
                modify $ Set.insert newName
                return $ Just (name, newName)
            else return Nothing
      where
        make' name' namespace
            | Set.member newName namespace = make' newName namespace
            | otherwise                    = newName
          where
            newName = name' ++ "'"

    renamings = Map.fromList $ mapMaybe id $ evalState (mapM makeRenaming $ Set.toList boundVars) freeVars

    Right newExpr = naiveSubstitute (rename renamings expr1) var expr2
applyRedex expr1 expr2 = expr1 :$: expr2

normalStep :: Expression -> Maybe Expression
normalStep (Lambda var expr)
    | Just step <- normalStep expr = Just $ Lambda var step
    | otherwise                    = Nothing
normalStep (expr1@(Lambda _ _) :$: expr2) = Just $ applyRedex expr1 expr2
normalStep (expr1 :$: expr2)
    | Just step <- normalStep expr1 = Just (step :$: expr2)
    | Just step <- normalStep expr2 = Just (expr1 :$: step)
    | otherwise                     = Nothing
normalStep (Var _) = Nothing

normalize :: Expression -> Expression
normalize expr = case normalStep expr of
    Just expr' -> normalize expr'
    Nothing    -> expr

($$) :: Expression -> Expression -> Expression
expr1 $$ expr2 = normalize $ expr1 :$: expr2
