{-# LANGUAGE OverloadedStrings #-}
module KnightsTour where
import BDD

solve :: Int -> Bool
solve _ = False

type Position = (Int, Int)
type Board = (Int, Int) -- (size, start variable)

variable  :: Board -> Position -> Int
variable (n, s) (i, j) = s + n * i + j

variables :: Board -> [Int]
variables b@(n, s) =
  [variable b (i, j) | i <- [0..n - 1], j <- [0..n - 1]]


move ::  Board ->  Position -> Board -> Position -> BExp
move b1 a@(i, j) b2 b@(k, l) =
   let
     old = And (Var $ variable b1 a) (Neg $ Var $ variable b1 b)
     new = And (Var $ variable b2 b) (Neg $ Var $ variable b2 a)
     fields1 = [x | x <- variables b1, x /= (variable b1 a), x /= (variable b1 b)]
     fields2 = [x | x <- variables b2, x /= (variable b2 a), x /= (variable b2 b)]
     rest = foldl (\exp (v1, v2) -> And exp (Eq (Var v1) (Var v2))) (Val True) $ zip fields1 fields2
   in And (And old new) rest
    
transitions :: Board -> Board -> BExp
transitions b1@(n, _) b2 =
  let
    positions = [((i, j), (k, l)) |
                 i <- [0..n-1], j <- [0..n-1], k <- [0..n-1], l <- [0..n-1],
                 ((abs (i-k) == 1 && abs (j-l) == 2) || (abs (i-k) == 2 && abs (j-l) == 1))]
  in
    foldl (\exp (a, b) -> Or exp $ move b1 a b2 b) (Val False) positions

reachable :: Int -> BExp -> BDDNode -> IO BDDNode
reachable n start trans =
  reachableAux n (build start (2*n*n)) (build start (2*n*n)) trans

reachableAux :: Int -> BDDNode -> BDDNode -> BDDNode -> IO BDDNode
reachableAux n start last trans =
  let
    and =  apply trans last (&&)
    m = foldl (\acc v -> apply (restrict acc v True) (restrict acc v False) (||)) and [1..n*n]
    newM = rename m [n*n+1..2*n*n] [1..n*n]
    cur = apply start newM (||)
  in do
    putStrLn $ show $ anySat and
    putStrLn ""
    if cur /= last then reachableAux n start cur trans
    else return cur

knight :: Int -> IO Int
knight n = do
    let s = (Var $ variable (n, 1) (0,0))
    let rest = [x | x <- variables (n,1) , x /= 1]
    let start = foldl (\exp v -> And exp (Neg (Var v))) s rest
    let trans = build (transitions (n, 1) (n, n*n + 1)) (2*n*n)
    fin <- reachable n start trans
    return $ div (satCount fin) (2^(n*n))
