module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  (expr:_) <- getArgs
  cont <- readFile expr
  putStrLn (readExpr cont)
