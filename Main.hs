module Main where

import KnightsTour
import BDD
import Data.Map

main :: IO ()
main = do
  n <- readLn :: IO Int
  let res = knight n
  putStrLn $ show res
