module Parse (parse) where

import Data.Bifunctor (first)
import Data.List (isPrefixOf)
import qualified Text.Megaparsec as M (errorBundlePretty, parse)

import ParseLisp
import Types


parse :: String -> String -> Either String Expr
parse filename = first M.errorBundlePretty . M.parse parser filename
  where
    parser = wholeFile >>= translate


translate :: MonadFail m => Lisp -> m Expr
translate sym@(Symbol _) =
  Var <$> parseIdentifier sym
translate (String s) = fail $ "Unexpected string" ++ show s
translate (Number n) = return $ NumExp n
translate (Float f) = fail $ "Unexpected float" ++ show f
translate (List []) = fail "Unexpected nil"
translate (List [Symbol "lambda", List args, body]) =
  Lam <$> mapM parseIdentifier args <*> translate body
translate (List [Symbol "let", List [name, value], body]) =
  Let <$> parseIdentifier name <*> translate value <*> translate body
translate (List [Symbol "letrec", List [name, value], body]) =
  Letrec <$> parseIdentifier name <*> translate value <*> translate body
translate (List (fn : args)) =
  App <$> translate fn <*> mapM translate args


keywords :: [String]
keywords = ["let", "letrec", "lambda"]

parseIdentifier :: MonadFail m => Lisp -> m VarIdent
parseIdentifier (Symbol name)
  | name `elem` keywords || isPrefixOf "__" name = fail $ "Invalid identifier: " ++ name
  | otherwise = return $ VarIdent name
parseIdentifier bad = fail $ "Invalid identifier: " ++ show bad