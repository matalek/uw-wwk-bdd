module Main where

import KnightsTour
import BDD
import Data.Map

main :: IO ()
main = do
  --let ((m, _), _) = build (transitions (4, 1) (4, 2)) 32
  --putStrLn $ show $ Data.Map.size m
  let res = knight 3
  putStrLn $ show res
