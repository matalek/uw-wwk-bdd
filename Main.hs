module Main where

import KnightsTour

main :: IO ()
main = do
  let res = knight 3
  putStrLn $ show res
