module Parse where

import Data.Bifunctor (first)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.Megaparsec as M (errorBundlePretty, parse)

import ParseLisp
import Types


parse :: String -> String -> Either String Expr
parse filename = first M.errorBundlePretty . M.parse parser filename
  where
    parser = wholeFile >>= translate


translate :: MonadFail m => Lisp -> m Expr
translate expr = case expr of
  -- atoms
  Symbol _ -> Var <$> parseIdentifier expr
  String s -> fail $ "Unexpected string" ++ show s
  Number n -> return $ NumExp n
  Float f -> fail $ "Unexpected float" ++ show f
  List [] -> fail "Unexpected nil"
  -- keywords
  List (Symbol name : exprs) -> parseKeyword name exprs
  -- assume application if the list does not start with a keyword
  List (fn : args) -> App <$> translate fn <*> mapM translate args


parseKeyword :: MonadFail m => String -> [Lisp] -> m Expr
parseKeyword name args = case Map.lookup name keywordParsingTable of
  Nothing -> App <$> translate (Symbol name) <*> mapM translate args
  Just parseFunction -> parseFunction args


keywordParsingTable :: MonadFail m => Map String ([Lisp] -> m Expr)
keywordParsingTable = Map.fromList
  [ ("lambda", \exprs -> case exprs of
      [List args, body] -> Lam <$> mapM parseIdentifier args <*> translate body
      _ -> syntaxFailure "lambda"
    )
  , ("let", \exprs -> case exprs of
      [List [name, value], body] -> Let <$> parseIdentifier name <*> translate value <*> translate body
      _ -> syntaxFailure "let"
    )
  , ("letrec", \exprs -> case exprs of
      [List [name, value], body] -> Letrec <$> parseIdentifier name <*> translate value <*> translate body
      _ -> syntaxFailure "letrec"
    )
  , ("if0", \exprs -> case exprs of
      [c, t, f] -> If0 <$> translate c <*> translate t <*> translate f
      _ -> syntaxFailure "if0"
    )
  ]
  where
    syntaxFailure :: MonadFail m => String -> m a
    syntaxFailure name = fail $ "Invalid syntax in " ++ name ++ " expression"


parseIdentifier :: MonadFail m => Lisp -> m VarIdent
parseIdentifier expr = case expr of
  (Symbol name) | isKeyword name || isInvalid name -> fail $ "Invalid identifier: " ++ name
  (Symbol name) -> return $ VarIdent name
  bad -> fail $ "Invalid identifier: " ++ show bad
  where
    isKeyword name = Map.member name (keywordParsingTable :: Map String ([Lisp] -> Maybe Expr))
    isInvalid name = isPrefixOf "__" name
