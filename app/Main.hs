module Main where

import Infer
import Pretty
import Types

main :: IO ()
main = printType letrecInf
  where
    printType = putStrLn . pretty . either error id . infer
    -- expr = App (Lam [VarIdent "y"] (Var (VarIdent "y"))) [Lam [VarIdent "x"] (Var (VarIdent "x"))]
    letrecInf =
      Letrec
        (VarIdent "f") (Lam [VarIdent "x"] (App (Var (VarIdent "f")) [Var (VarIdent "x")]))
        (App (Var (VarIdent "f")) [Lam [VarIdent "y"] (Var (VarIdent "y"))])