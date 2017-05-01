module HM.TypeInference where

import           Control.Monad.State
import qualified Data.Set             as Set

import           HM.Expression
import           Simple.Expression    (Type (..), TypeName, Var)
import           Simple.TypeInference

type Unificator = Type -> Type

type Context = [(Var, Rank1Type)]

unify :: Type -> Type -> Maybe Unificator
unify t1 t2 = applySystem <$> resolveSystem [t1 :=: t2]

getFreeTypes :: Rank1Type -> Set.Set TypeName
getFreeTypes (ForAll typeName t) = Set.delete typeName $ getFreeTypes t
getFreeTypes (Monotype t)        = getBaseTypes t

rank1Unificator :: Unificator -> Rank1Type -> Rank1Type
rank1Unificator s (ForAll typeName t) = ForAll typeName $ rank1Unificator s t
rank1Unificator s (Monotype t)        = Monotype $ s t

applyUnificator :: Unificator -> Context -> Context
applyUnificator s context = map (fmap $ rank1Unificator s) context

typeClosure :: Type -> Context -> Rank1Type
typeClosure t context =
    let monoT = Monotype t
        tFreeTypes = getFreeTypes $ Monotype t
        contextFT = mconcat $ map (getFreeTypes . snd) context
        alphas = filter (not . flip Set.member contextFT) $ Set.toList tFreeTypes in
    foldr ForAll monoT alphas

removeVar :: Var -> Context -> Context
removeVar var context = filter ((/= var) . fst) context

getNewType :: Monad m => StateT Int m Type
getNewType = do
    nextTypeN <- get
    modify (+ 1)
    pure $ BaseType $ "t" ++ show nextTypeN

algorithmW :: Context -> HMExpression -> StateT Int Maybe (Unificator, Type)
algorithmW context (Var var) =
    let maybeVarType = lookup var context in
    case maybeVarType of
        Nothing -> lift Nothing
        Just varType -> do
            let (alphas, t') = getMonotype varType
            newTypes <- replicateM (length alphas) getNewType
            let substitutions = zip alphas newTypes
            let t = foldr typeSubstitute t' substitutions
            pure (id, t)
  where
    getMonotype (ForAll v t) = ([v], ()) *> getMonotype t
    getMonotype (Monotype t) = ([], t)
algorithmW context (expr1 :$: expr2) = do
    (s1, t1) <- algorithmW context expr1
    (s2, t2) <- algorithmW (applyUnificator s1 context) expr2
    beta <- getNewType
    v <- lift $ unify (s2 t1) (t2 :>: beta)
    let s = v . s1 . s2
    pure (s, s beta)
algorithmW context (Lambda var expr) = do
    beta <- getNewType
    let contextX = removeVar var context
    (s1, t1) <- algorithmW ((var, Monotype beta) : contextX) expr
    pure (s1, s1 beta :>: t1)
algorithmW context (Let var expr1 expr2) = do
    (s1, t1) <- algorithmW context expr1
    let s1ContextX = applyUnificator s1 $ removeVar var context
    let varType = typeClosure t1 s1ContextX
    let nextContext = (var, varType) : s1ContextX
    (s2, t2) <- algorithmW nextContext expr2
    pure (s2 . s1, t2)

hmInferType :: HMExpression -> Maybe Type
hmInferType expr = snd . fst <$> (runStateT (algorithmW [] expr) 0)
