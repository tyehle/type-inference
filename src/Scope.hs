module Scope
  ( freeVars
  , freeTVars
  , freePolyTVars
  ) where

import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as Set

import Types


deleteAll :: Ord a => [a] -> Set a -> Set a
deleteAll elems set = foldl' remove set elems
  where
    remove set elem = Set.delete elem set


freeVars :: Expr -> Set VarIdent
freeVars (Var name) = Set.singleton name
freeVars (Lam args body) = deleteAll args (freeVars body)
freeVars (Let name value body) = freeVars value `Set.union` Set.delete name (freeVars body)
freeVars (App func args) = freeVars func `Set.union` Set.unions (map freeVars args)


freeTVars :: MonoType -> Set TVarIdent
freeTVars (TVar id) = Set.singleton id
freeTVars (TLam argTypes retType) = freeTVars retType `Set.union` Set.unions (map freeTVars argTypes)
freeTVars (TApp _ types) = Set.unions $ map freeTVars types


freePolyTVars :: PolyType -> Set TVarIdent
freePolyTVars (PolyType ids monotype) = deleteAll ids (freeTVars monotype)
