module Scope
  ( freeVars
  , freeTVars
  , freePolyTVars
  ) where

import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as Set

import Types


deleteAll :: Ord a => Set a -> [a] -> Set a
deleteAll = foldl' $ flip Set.delete


freeVars :: Expr -> Set VarIdent
freeVars (Var name) = Set.singleton name
freeVars (Lam args body) = deleteAll (freeVars body) args
freeVars (Let name value body) = freeVars value `Set.union` Set.delete name (freeVars body)
freeVars (Letrec name value body) = Set.delete name $ freeVars value `Set.union` freeVars body
freeVars (App func args) = freeVars func `Set.union` Set.unions (map freeVars args)


freeTVars :: MonoType -> Set TVarIdent
freeTVars (TVar ident) = Set.singleton ident
freeTVars (TLam argTypes retType) = freeTVars retType `Set.union` Set.unions (map freeTVars argTypes)
freeTVars (TApp _ types) = Set.unions $ map freeTVars types


freePolyTVars :: PolyType -> Set TVarIdent
freePolyTVars (PolyType idents monotype) = deleteAll (freeTVars monotype) idents
