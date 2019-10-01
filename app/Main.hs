module Main where

import System.Environment
import Parser

main :: IO ()
main =
  do args <- getArgs
     putStrLn $ head args
