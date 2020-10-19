{-# LANGUAGE FlexibleContexts #-}
module Infer where

import Control.Monad.Except
import Control.Monad.State
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace (trace)

import Types
import Pretty
import Scope

type TypeEnv = Map VarIdent PolyType

type Subs = Map TVarIdent MonoType

data InferState = InferState TVarIdent (Map TVarIdent MonoType)
type Infer = State InferState

type Fresh = State TVarIdent

-- | Create a fresh type variable
fresh :: Infer MonoType
fresh = do
  (InferState (TVarIdent n) s) <- get
  put $ InferState (TVarIdent (n + 1)) s
  return $ TVar $ TVarIdent n


-- | Core types
numType :: MonoType
numType = TApp "Num" []


-- | Add some additional context to an error message
contextualize :: (Pretty c, MonadError String m) => c -> m a -> m a
contextualize context comp = catchError comp (\err -> throwError $ err ++ "\n  in " ++ pretty context)


-- | Instantiate a polytype by creating fresh monotypes for all its arguments and subbing them in
instantiate :: PolyType -> Infer MonoType
instantiate (PolyType idents term) = do
  freshTVars <- mapM (const fresh) idents
  let subs = Map.fromList $ zip idents freshTVars
  return $ subMonoType subs term
  where
    subMonoType :: Subs -> MonoType -> MonoType
    subMonoType subs (TVar n) =
      case Map.lookup n subs of
        Nothing -> TVar n
        (Just t) -> subMonoType (Map.delete n subs) t -- delete from the sub so we can't hang
    subMonoType subs (TLam args ret) = TLam (map (subMonoType subs) args) (subMonoType subs ret)
    subMonoType subs (TApp name args) = TApp name $ map (subMonoType subs) args


-- | Make a polytype that quantifies all free type variables in a monotype that are not in the environment
generalize :: TypeEnv -> MonoType -> Infer PolyType
generalize env monoType = do
  resolved <- resolve monoType
  let tvars = Set.toList $ freeTVars resolved `Set.difference` freeInEnv
  return $ PolyType tvars resolved
  where
    freeInEnv = Set.unions $ map freePolyTVars (Map.elems env)


-- | Resolves all bound instances of type variables in a type
resolve :: MonoType -> Infer MonoType
resolve t = do
  (InferState _ subs) <- get
  return $ go subs t
  where
    go :: Subs -> MonoType -> MonoType
    go subs monoType = case monoType of
      TVar ident -> case Map.lookup ident subs of
        Nothing -> monoType
        (Just other) -> go (Map.delete ident subs) other -- Don't loop forever
      TLam args res -> TLam (map (go subs) args) (go subs res)
      TApp name args -> TApp name (map (go subs) args)


-- | Unify two types, possibly binding a type variable to another type
unify :: MonoType -> MonoType -> ExceptT String Infer ()
unify a b = do
  a' <- lift $ resolve a
  b' <- lift $ resolve b
  go a' b'
  where
    assign :: TVarIdent -> MonoType -> ExceptT String Infer ()
    assign ident monoType
      | monoType == TVar ident = return ()
      | occurs = throwError $ "Cannot create infinite type: " ++ pretty (TVar ident) ++ " = " ++ pretty monoType
      | otherwise = insert
      where
        occurs = Set.member ident $ freeTVars monoType
        insert = do
          (InferState next subs) <- get
          let subs' = Map.insert ident monoType subs
          put $ InferState next subs'

    go :: MonoType -> MonoType -> ExceptT String Infer ()
    go (TVar ident) monoType = assign ident monoType
    go monoType (TVar ident) = assign ident monoType
    go (TLam argsA retA) (TLam argsB retB) = unifyAll (retA:argsA) (retB:argsB)
    go (TApp nameA argsA) (TApp nameB argsB) | nameA == nameB = unifyAll argsA argsB
    go badA badB = throwError $ "Type Error: Cannot unify " ++ pretty badA ++ " with " ++ pretty badB

-- | Unify two lists of types. If the lists are not the same size throw a type error
unifyAll :: [MonoType] -> [MonoType] -> ExceptT String Infer ()
unifyAll [] [] = return ()
unifyAll (a:as) (b:bs) = unify a b >> unifyAll as bs
unifyAll _ _ = throwError "Type Error: Arity mismatch"


-- | Infer the type of an expression
infer :: Expr -> Either String PolyType
infer expr = runInference $ inferRec Map.empty expr >>= (lift . generalize Map.empty)
  where
    runInference :: ExceptT String Infer a -> Either String a
    runInference comp = evalState (runExceptT comp) $ InferState (TVarIdent 0) Map.empty


-- | Recursively infer the type of an expression with all the state needed to make that happen
inferRec :: TypeEnv -> Expr -> ExceptT String Infer MonoType
inferRec env expr = do
  (InferState _ s1) <- get
  trace ("\n>>> " ++ pretty expr ++ prettyState s1) $ return ()
  ret <- contextualize expr $ case expr of
    -- lookup s in the env and specialize if it so we can unify its quantified variables with real types
    Var ident -> case Map.lookup ident env of
      Nothing -> throwError $ "Undefined Variable: " ++ show ident
      (Just polyType) -> lift $ instantiate polyType

    NumExp _ -> return numType

    -- typecheck the body with the argNames bound to new type vars in the environment
    Lam argNames body -> do
      argTypes <- lift $ mapM (const fresh) argNames
      let bindings = Map.fromList $ zip argNames (map (PolyType []) argTypes)
      let env' = Map.union bindings env
      retType <- inferRec env' body
      return $ TLam argTypes retType

    -- infer the value type, generalize it, and then infer the body with the new type in the environment
    Let name value body -> do
      valueType <- inferRec env value
      polyType <- lift $ generalize env valueType
      let env' = Map.insert name polyType env
      inferRec env' body

    Letrec name value body -> do
      freshValueType <- lift fresh
      let valueEnv = Map.insert name (PolyType [] freshValueType) env
      valueType <- inferRec valueEnv value
      unify valueType freshValueType
      polyType <- lift $ generalize env valueType
      let bodyEnv = Map.insert name polyType env
      inferRec bodyEnv body

    -- Check all args and func, then unify
    App f args -> do
      fType <- inferRec env f
      argTypes <- mapM (inferRec env) args
      retType <- lift fresh
      unify fType (TLam argTypes retType)
      return retType

    If0 cond true false -> do
      condType <- inferRec env cond
      trueType <- inferRec env true
      falseType <- inferRec env false
      unify condType numType
      unify trueType falseType
      return trueType

  (InferState _ s2) <- get
  trace ("\n<<< " ++ pretty expr ++ " : " ++ pretty ret ++ prettyState s2) $ return ret
  where
    prettyState s = "\n  env - " ++ prettyEnv ++ "\n  var - " ++ prettySub s
    prettyEnv = intercalate "\n        " $ map prettyEnvEntry $ Map.toList env
    prettyEnvEntry (VarIdent name, t) = name ++ ": " ++ pretty t
    prettySub = intercalate "\n        " . map prettySubEntry . Map.toList
    prettySubEntry (tv, t) = pretty (TVar tv) ++ ": " ++ pretty t
