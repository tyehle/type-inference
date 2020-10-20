module Types where

newtype VarIdent = VarIdent String deriving (Show, Eq, Ord)

data Expr
  = Var VarIdent
  | NumExp Integer
  | Lam [VarIdent] Expr
  | Let VarIdent Expr Expr
  | Letrec VarIdent Expr Expr
  | App Expr [Expr]
  | If0 Expr Expr Expr
  | BinOp BinOp Expr Expr
  deriving (Show, Eq, Ord)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  deriving (Show, Eq, Ord)

newtype TVarIdent = TVarIdent Int deriving (Show, Eq, Ord)

data MonoType
  = TVar TVarIdent
  | TLam [MonoType] MonoType
  | TApp String [MonoType]
  deriving (Show, Eq, Ord)


data PolyType
  = PolyType [TVarIdent] MonoType
  deriving (Show, Eq, Ord)
