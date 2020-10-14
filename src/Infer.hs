{-# LANGUAGE FlexibleContexts, TupleSections #-}
module Infer where

import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import Types
import Pretty
import Scope

type TypeEnv = Map VarIdent PolyType

type Subs = Map TVarIdent MonoType

data InferState = InferState TVarIdent Subs
type Infer = State InferState

-- | Create a fresh type variable
fresh :: Infer MonoType
fresh = do
  (InferState n env) <- get
  put $ InferState (inc n) env
  return $ TVar n
  where
    inc (TVarIdent n) = TVarIdent $ n + 1


-- | Add some additional context to an error message
contextualize :: (Pretty c, MonadError String m) => c -> m a -> m a
contextualize context comp = catchError comp (\err -> throwError $ err ++ "\n  in " ++ pretty context)


-- | Instantiate a polytype by creating fresh monotypes for all its arguments and subing them in
instantiate :: PolyType -> Infer MonoType
instantiate (PolyType idents term) = do
  freshTVars <- mapM (const fresh) idents
  let subs = Map.fromList $ zip idents freshTVars
  return $ substitute subs term

-- | Replace all instances of a type variable in a monotype with a different type
substitute :: Subs -> MonoType -> MonoType
substitute subs term@(TVar ident) = fromMaybe term $ Map.lookup ident subs
substitute subs (TLam argTypes retType) = TLam (map (substitute subs) argTypes) (substitute subs retType)
substitute subs (TApp name targs) = TApp name $ map (substitute subs) targs


-- | Make a polytype that quantifies all free type variables in a monotype that are not in the environment
generalize :: TypeEnv -> MonoType -> PolyType
generalize env monoType = PolyType (Set.toList (freeTVars monoType `Set.difference` freeInEnv)) monoType
  where
    freeInEnv = Set.unions $ map freePolyTVars (Map.elems env)


-- | Results in an error if the types cannot be unified or a substitution if they can be
unify :: MonadError String m => MonoType -> MonoType -> m Subs
unify (TVar ident) term = mkSub ident term
unify term (TVar ident) = mkSub ident term
unify (TLam argsA retA) (TLam argsB retB) = do
  subs <- unify retA retB
  unifyAll subs argsA argsB
unify (TApp nameA argsA) (TApp nameB argsB) | nameA == nameB = unifyAll Map.empty argsA argsB
unify a b = throwError $ "Type Error: Cannot unify " ++ show a ++ " with " ++ show b

unifyAll :: MonadError String m => Subs -> [MonoType] -> [MonoType] -> m Subs
unifyAll subs [] [] = return subs
unifyAll subs (a:as) (b:bs) = do
  subs' <- unify (subMonoType subs a) (subMonoType subs b)
  unifyAll (Map.union subs subs') as bs
unifyAll _ _ _ = throwError "Type Error: Arity mismatch"

-- | Build a substitution for the typevar ident in the given monotype
mkSub :: MonadError String m => TVarIdent -> MonoType -> m Subs
mkSub ident monotype
  | monotype == TVar ident = return Map.empty
  | occurs = throwError $ "Cannot create infinite type: " ++ pretty (TVar ident) ++ " = " ++ pretty monotype
  | otherwise = return $ Map.singleton ident monotype
  where
    occurs :: Bool
    occurs = Set.member ident $ freeTVars monotype


subMonoType :: Subs -> MonoType -> MonoType
subMonoType subs (TVar n) =
  case Map.lookup n subs of
    Nothing -> TVar n
    (Just t) -> subMonoType (Map.delete n subs) t -- delete from the sub so we can't hang
subMonoType subs (TLam args ret) = TLam (map (subMonoType subs) args) (subMonoType subs ret)
subMonoType subs (TApp name args) = TApp name $ map (subMonoType subs) args

subPolyType :: Subs -> PolyType -> PolyType
subPolyType subs (PolyType idents monoType) = PolyType idents $ subMonoType freeSubs monoType
  where
    freeSubs = foldl' (flip Map.delete) subs idents


-- | Infer the type of an expression
infer :: Expr -> Either String PolyType
infer expr = do
  (subs, monoType) <- runInference $ inferRec Map.empty expr
  return $ generalize Map.empty $ subMonoType subs monoType
  where
    runInference :: ExceptT String Infer a -> Either String a
    runInference comp = evalState (runExceptT comp) $ InferState (TVarIdent 0) Map.empty

inferRec :: TypeEnv -> Expr -> ExceptT String Infer (Subs, MonoType)
inferRec env expr = case expr of
  -- lookup s in the env and specialize if it so we can unify its quantified variables with real types
  Var ident -> case Map.lookup ident env of
    Nothing -> throwError $ "Undefined Variable: " ++ show ident
    (Just polyType) -> lift $ (Map.empty,) <$> instantiate polyType

  -- typecheck the body with the argNames bound to new typevars in the environment
  Lam argNames body -> do
    argTypes <- lift $ mapM (const fresh) argNames
    let bindings = Map.fromList $ zip argNames (map (PolyType []) argTypes)
    let env' = Map.union bindings env
    (subs, retType) <- contextualize expr $ inferRec env' body
    let lamType = TLam (map (subMonoType subs) argTypes) (subMonoType subs retType)
    return (subs, lamType)

  -- infer the value type, generalize it, and then infer the body with the new type in the environment
  Let name value body -> do
    (valueSubs, valueType) <- contextualize expr $ inferRec env value
    let subEnv = Map.map (subPolyType valueSubs) env
        env' = Map.insert name (generalize subEnv valueType) subEnv
    (bodySubs, bodyType) <- inferRec env' body
    return (Map.union valueSubs bodySubs, bodyType)

  Letrec name value body -> do
    freshValueType <- lift fresh
    let valueEnv = Map.insert name (PolyType [] freshValueType) env
    (s1, valueType) <- contextualize expr $ inferRec valueEnv value
    s2 <- unify valueType (subMonoType s1 freshValueType)
    let env' = Map.map (subPolyType (Map.union s1 s2)) env
        bodyEnv = Map.insert name (generalize env' (subMonoType s2 valueType)) env'
    (s3, bodyType) <- contextualize expr $ inferRec bodyEnv body
    return (Map.unions [s1, s2, s3], bodyType)

  -- Check all args and func, then unify
  App f args -> do
    (baseSubs, fType) <- contextualize expr $ inferRec env f
    (argSubs, argTypes) <- recurseArgs baseSubs (Map.map (subPolyType baseSubs) env) args
    retType <- lift fresh
    appSubs <- unify (subMonoType argSubs fType) (TLam (map (subMonoType argSubs) argTypes) retType)
    return (Map.union argSubs appSubs, subMonoType appSubs retType)

  where
    recurseArgs :: Subs -> TypeEnv -> [Expr] -> ExceptT String Infer (Subs, [MonoType])
    recurseArgs subs _ [] = return (subs, [])
    recurseArgs subs baseEnv (arg:args) = do
      (subs', argType) <- contextualize expr $ inferRec (Map.map (subPolyType subs) baseEnv) arg
      let ourSubs = Map.union subs subs'
      (otherSubs, argTypes) <- recurseArgs ourSubs (Map.map (subPolyType ourSubs) baseEnv) args
      return (Map.union ourSubs otherSubs, argType : argTypes)
