module Main where

import Data.List
import System.Environment


-- An inefficient test function.
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
    args <- getArgs
    case length args of
      1 -> do
          let x = read $ head args
          print $ fib x
      _ -> do
          putStrLn "You need to specify an integer."

