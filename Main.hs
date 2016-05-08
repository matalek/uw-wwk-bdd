module Main where

import KnightsTour
import BDD
import Data.Map

-- Reads size of the board (n) from standard input and prints out the result
-- of knight function
main :: IO ()
main = do
  n <- readLn :: IO Int
  let res = knight n
  putStrLn $ show res
