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
    
