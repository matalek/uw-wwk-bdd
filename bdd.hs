module BDD where
import Data.Map as Map

type Value = Bool
type Variable = Int
type Node = Maybe Int
data BExp = Var Variable | Val Value | Neg BExp | And BExp BExp | Or BExp BExp deriving (Show, Eq)

type Triple = (Int, Node, Node)

type BDD = (Map Int Triple, Map Triple Int)

mk :: BDD -> Int -> Node -> Node -> (BDD, Node)
mk bdd@(t, h) i low high
  | low == high = (bdd, low)
  | member (i, low, high) h = (bdd, Just $ h ! (i, low, high))
  | otherwise =
    let 
      u = (fst $ findMax t) + 1
      newT = insert u (i, low, high) t
      newH = insert (i, low, high) u h
    in
      ((newT, newH), Just u)


assign :: BExp -> Variable -> Value -> BExp
assign (Val v) _ _ = Val v
assign (Var s) x v
  | s == x = Val v
  | otherwise = Var s
assign (Neg e) x v = Neg $ assign e x v
assign (And e1 e2) x v = And (assign e1 x v) (assign e2 x v)
assign (Or e1 e2) x v = Or (assign e1 x v) (assign e2 x v) 

-- Calculates the value of the expression, where are variables have assigned values
calculate :: BExp -> Value
calculate (Val v) = v
calculate (Neg e) = not $ calculate e
calculate (And e1 e2) =
  let
    v1 = calculate e1
    v2 = calculate e2
  in case (v1, v2) of
    (True, True) -> True
    _ -> False
calculate (Or e1 e2) =
  let
    v1 = calculate e1
    v2 = calculate e2
  in case (v1, v2) of
    (False, False) -> False
    _ -> True

-- Assuming, that no values are assigned
maxVar :: BExp -> Int
maxVar (Var v) = v
maxVar (Neg e) = maxVar e
maxVar (Or e1 e2) = max (maxVar e1) $ maxVar e2
maxVar (And e1 e2) = max (maxVar e1) $ maxVar e2

build :: BExp -> BDD
build b = let
  n = maxVar b + 1
  v = (n, Nothing, Nothing)
  in
    fst $ buildAux b (fromList [(0, v), (1, v)], Map.empty) 1

buildAux :: BExp -> BDD -> Variable -> (BDD, Node)
buildAux b bdd@(t, h) i
  | i > maxVar b =
    if calculate b == False then (bdd, Just 0)
    else (bdd, Just 1)
  | otherwise =
    let
      (bdd1, v0) = buildAux (assign b i False) bdd (i + 1)
      (bdd2, v1) = buildAux (assign b i True) bdd1 (i + 1)
    in mk bdd2 i v0 v1

test = Or (Var 1) (Var 3)
