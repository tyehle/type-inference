module Pretty where

import Data.List (intercalate)
import Data.Char (ord, chr)

import Types

subscript :: Int -> String
subscript = map conv . show
  where
    conv c = chr $ ord c + 8272


class Pretty a where
  pretty :: a -> String

instance Pretty Expr where
  pretty (Var (VarIdent name)) = name
  pretty (NumExp n) = show n
  pretty (Lam args body) = "(λ (" ++ unwords argNames ++ ") " ++ pretty body ++ ")"
    where
      argNames = map (\(VarIdent name) -> name) args
  pretty (Let (VarIdent name) value body) = "(let [" ++ name ++ " " ++ pretty value ++ "] " ++ pretty body ++ ")"
  pretty (Letrec (VarIdent name) value body) = "(letrec [" ++ name ++ " " ++ pretty value ++ "] " ++ pretty body ++ ")"
  pretty (App fn args) = "(" ++ pretty fn ++ " " ++ unwords (map pretty args) ++ ")"
  pretty (If0 c t f) = "(if0 " ++ pretty c ++ " " ++ pretty t ++ " " ++ pretty f ++ ")"

instance Pretty MonoType where
  pretty (TVar (TVarIdent n)) = "τ" ++ subscript n
  pretty (TLam argTypes retType) = "(" ++ intercalate ", " (map pretty argTypes) ++ ") → " ++ pretty retType
  pretty (TApp name types) = unwords $ name : map pretty types

instance Pretty PolyType where
  pretty (PolyType idents monoType) = "∀ " ++ unwords (map prettyIdent idents) ++ ". " ++ pretty monoType
    where
      prettyIdent (TVarIdent ident) = "τ" ++ subscript ident
