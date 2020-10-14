module Main where

import Control.Monad ((>=>))

import Infer
import Parse
import Pretty


main :: IO ()
main = printType prog
  where
    printType = putStrLn . pretty . either error id . (parse "input" >=> infer)
    -- expr = "((lambda (y) y) (lambda (x) x))"
    prog = "(letrec [f (lambda (x) (f x))] (f (lambda (y) y)))"
