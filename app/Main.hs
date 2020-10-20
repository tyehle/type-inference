module Main where

import Control.Monad ((>=>))

import Infer
import Parse
import Pretty

printType :: String -> IO ()
printType = putStrLn . either id pretty . (parse "input" >=> infer)

main :: IO ()
main = printType prog
  where
    -- prog = "((lambda (y) y) (lambda (x) x))"
    prog = "(letrec [f (lambda (x) (f x))] (f f))"
    -- prog = "(lambda (x) x)"
