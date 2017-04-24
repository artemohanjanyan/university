module Simple.TypeInference where

import           Control.Monad
import           Control.Monad.State.Lazy
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (isJust)
import qualified Data.Set                 as Set

import           Simple.Expression
import           Simple.Reduction

data Equation = Type :=: Type deriving (Eq, Ord)
infix 8 :=:
type System = [Equation]
type Substitution = (TypeName, Type)
type ResolvedSystem = [Substitution]

instance Show Equation where
    show (t1 :=: t2) = show t1 ++ " = " ++ show t2

getBaseTypes :: Type -> Set.Set TypeName
getBaseTypes (BaseType name) = Set.singleton name
getBaseTypes (t1 :>: t2)     = Set.union (getBaseTypes t1) (getBaseTypes t2)

typeSubstitute :: Substitution -> Type -> Type
typeSubstitute (subName, subType) t@(BaseType name)
    | name == subName = subType
    | otherwise       = t
typeSubstitute sub (t1 :>: t2) =
        typeSubstitute sub t1 :>: typeSubstitute sub t2

applySystem :: ResolvedSystem -> Type -> Type
applySystem system t = foldl (flip typeSubstitute) t system

data ResolveResult a
    = Inconsistent
    | Unmodified a
    | Modified a
    deriving (Show)

instance Functor ResolveResult where
    fmap _  Inconsistent  = Inconsistent
    fmap f (Unmodified x) = Unmodified (f x)
    fmap f (  Modified x) =   Modified (f x)

instance Applicative ResolveResult where
    pure = Unmodified
    Inconsistent   <*> _              = Inconsistent
    _              <*> Inconsistent   = Inconsistent
    (  Modified f) <*> (  Modified x) =   Modified (f x)
    (  Modified f) <*> (Unmodified x) =   Modified (f x)
    (Unmodified f) <*> (  Modified x) =   Modified (f x)
    (Unmodified f) <*> (Unmodified x) = Unmodified (f x)

instance Monad ResolveResult where
    Inconsistent   >>= _ = Inconsistent
    (Unmodified x) >>= f = f x
    (  Modified x) >>= f = case f x of
        Inconsistent   -> Inconsistent
        (Unmodified y) -> Modified y
        my             -> my

resolveReverseStep :: System -> ResolveResult System
resolveReverseStep system = mapM reverseEquation system
  where
    reverseEquation :: Equation -> ResolveResult Equation
    reverseEquation e@(t :=: x@(BaseType _)) = case t of
        BaseType _ -> Unmodified e
        _          -> Modified (x :=: t)
    reverseEquation e = Unmodified e

resolveTautologyStep :: System -> ResolveResult System
resolveTautologyStep system = filterM isNotTautology system
  where
    isNotTautology :: Equation -> ResolveResult Bool
    isNotTautology (a :=: b)
        | a == b    =   Modified False
        | otherwise = Unmodified True

resolveUnfoldStep :: System -> ResolveResult System
resolveUnfoldStep system = fmap concat $ mapM unfold system
  where
    unfold :: Equation -> ResolveResult [Equation]
    unfold ((t1 :>: t2) :=: (t3 :>: t4)) = Modified [t1 :=: t3, t2 :=: t4]
    unfold e                             = Unmodified [e]

resolveSubstituteStep :: System -> ResolveResult System
resolveSubstituteStep system = do
    mapM_ checkRecursion system
    case filter isJust $ map canSubstWith system of
        (Just sub):_ -> Modified $ map (subst sub) system
        _            -> Unmodified system
  where
    checkRecursion :: Equation -> ResolveResult ()
    checkRecursion (BaseType x :=: t)
        | not $ Set.member x (getBaseTypes t) = Unmodified ()
        | otherwise                           = Inconsistent
    checkRecursion _ = Unmodified ()

    canSubstWith :: Equation -> Maybe Substitution
    canSubstWith eq@(BaseType x :=: t) = if any canSubst system
        then Just (x, t)
        else Nothing
      where
        canSubst eq'@(t1 :=: t2) = eq /= eq' && any (Set.member x) [getBaseTypes t1, getBaseTypes t2]
    canSubstWith _ = Nothing

    subst :: Substitution -> Equation -> Equation
    subst sub@(x, t) eq@(t1 :=: t2) = if eq /= (BaseType x :=: t)
        then typeSubstitute sub t1 :=: typeSubstitute sub t2
        else eq

resolveStep :: System -> ResolveResult System
resolveStep system = do
    system1 <- resolveReverseStep    system
    system2 <- resolveTautologyStep  system1
    system3 <- resolveUnfoldStep     system2
    resolveSubstituteStep system3

resolveSystem :: System -> Maybe ResolvedSystem
resolveSystem system = case resolveStep system of
    Inconsistent       -> Nothing
    Modified   system' -> resolveSystem system'
    Unmodified system' -> Just $ map eqToSubst system'
  where
    eqToSubst (BaseType x :=: t) = (x, t)
    eqToSubst _                  = error "unmodified system is not resolved"

makeSystem :: Expression -> (System, Type)
makeSystem expr = (rawSystem, exprType)
  where
    renameAbstractions :: Expression -> State (Int, Map.Map Var Var) Expression
    renameAbstractions (V var) = do
        (_, varMap) <- get
        pure $ V $ Map.findWithDefault var var varMap
    renameAbstractions (expr1 :$: expr2) = do
        newExpr1 <- renameAbstractions expr1
        newExpr2 <- renameAbstractions expr2
        pure $ newExpr1 :$: newExpr2
    renameAbstractions (L var expr') = do
        (n, varMap) <- get
        let newVar = show n
        put (n + 1, Map.insert var newVar varMap)
        newExpr <- renameAbstractions expr'
        modify (\(n', _) -> (n', varMap))
        pure $ L newVar newExpr

    makeSystem' :: Expression -> State (Int, System) Type
    makeSystem' (V var) = pure $ BaseType $ "t" ++ var
    makeSystem' (expr1 :$: expr2) = do
        type1 <- makeSystem' expr1
        type2 <- makeSystem' expr2
        (n, system) <- get
        let typeName = "e" ++ show n
        let newType = BaseType typeName
        put (n + 1, type1 :=: type2 :>: newType : system)
        pure newType
    makeSystem' (L var expr') = do
        exprType' <- makeSystem' expr'
        pure $ (BaseType $ "t" ++ var) :>: exprType'

    renamedExpression = evalState (renameAbstractions expr) (0, Map.empty)
    (exprType, (_, rawSystem)) = runState (makeSystem' renamedExpression) (0, [])

inferType :: Expression -> Maybe (Type, [(Var, Type)])
inferType expr = do
    let (system, exprType) = makeSystem expr
    resolvedSystem <- resolveSystem system
    let freeVars = Set.toList $ getFreeVars expr
    let context = map (\var -> (var, applySystem resolvedSystem $ BaseType $ "t" ++ var)) freeVars
    pure (applySystem resolvedSystem exprType, context)
