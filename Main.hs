module Main where

import KnightsTour

main :: IO ()
main = do
  res <- knight 3
  putStrLn $ show res
