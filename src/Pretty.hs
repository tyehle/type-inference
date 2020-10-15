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
  pretty (Lam args body) = "(λ (" ++ intercalate " " argNames ++ ") " ++ pretty body ++ ")"
    where
      argNames = map (\(VarIdent name) -> name) args
  pretty (Let (VarIdent name) value body) = "(let [" ++ name ++ " " ++ pretty value ++ "] " ++ pretty body ++ ")"
  pretty (Letrec (VarIdent name) value body) = "(letrec [" ++ name ++ " " ++ pretty value ++ "] " ++ pretty body ++ ")"
  pretty (App fn args) = "(" ++ pretty fn ++ " " ++ intercalate " " (map pretty args) ++ ")"

instance Pretty MonoType where
  pretty (TVar (TVarIdent n)) = "τ" ++ subscript n
  pretty (TLam argTypes retTypes) = intercalate " → " (map pretty $ argTypes ++ [retTypes])
  pretty (TApp name types) = intercalate " " $ name : map pretty types

instance Pretty PolyType where
  pretty (PolyType idents monoType) = "∀ " ++ intercalate " " (map prettyIdent idents) ++ ". " ++ pretty monoType
    where
      prettyIdent (TVarIdent ident) = "τ" ++ subscript ident
