module BDD where
import Data.Map as Map

type Value = Bool
type Variable = Int
type Node = Int
type Edge = Maybe Int
data BExp = Var Variable | Val Value | Neg BExp | And BExp BExp | Or BExp BExp deriving (Show, Eq)

type Triple = (Int, Edge, Edge)

type BDD = (Map Int Triple, Map Triple Int)

mk :: BDD -> Int -> Node -> Node -> (BDD, Node)
mk bdd@(t, h) i low high
  | low == high = (bdd, low)
  | member (i, Just low, Just high) h = (bdd, h ! (i, Just low, Just high))
  | otherwise =
    let 
      u = (fst $ findMax t) + 1
      newT = insert u (i, Just low, Just high) t
      newH = insert (i, Just low, Just high) u h
    in
      ((newT, newH), u)


assign :: BExp -> Variable -> Value -> BExp
assign (Val v) _ _ = Val v
assign (Var s) x v
  | s == x = Val v
  | otherwise = Var s
assign (Neg e) x v = Neg $ assign e x v
assign (And e1 e2) x v = And (assign e1 x v) (assign e2 x v)
assign (Or e1 e2) x v = Or (assign e1 x v) (assign e2 x v) 

-- Calculates the value of the expression, where all variables have assigned values
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
    if calculate b == False then (bdd, 0)
    else (bdd, 1)
  | otherwise =
    let
      (bdd1, v0) = buildAux (assign b i False) bdd (i + 1)
      (bdd2, v1) = buildAux (assign b i True) bdd1 (i + 1)
    in mk bdd2 i v0 v1

type Arr2D = Map (Node, Node) Node
type Op = Bool -> Bool -> Bool

apply :: BDD -> BDD -> Op -> Node -> Node -> (BDD, Node)
apply b1 b2 op u1 u2 =
  let (b, u, _) = app b1 b2 op u1 u2 (Map.empty, Map.empty) (Map.empty)
  in (b, u)

app :: BDD -> BDD -> Op -> Node -> Node -> BDD ->  Arr2D -> (BDD, Node, Arr2D)
app bdd1@(t1, h1) bdd2@(t2, h2) op u1 u2 res g =
  let
    (res', u', g') =
      if member (u1, u2) g then (res, g ! (u1, u2), g)
      else if u1 < 2 && u2 < 2 then (res, evalOp u1 u2 op, g)
           else if v1 == v2 then mkApp v1 low1 low2 high1 high2
                else if v1 < v2 then mkApp v1 low1 u2 high1 u2
                     else mkApp v2 u1 low2 u1 high2
  in
    (res', u', insert (u1, u2) u' g')
  where
    (v1, Just high1, Just low1) = t1 ! u1
    (v2, Just high2, Just low2) = t2 ! u2
    mkApp w a1 a2 b1 b2 =
      let
        (res1, w1, g1) = app bdd1 bdd2 op a1 a2 res g
        (res2, w2, g2) = app bdd1 bdd2 op b1 b2 res1 g1
        (res3, w3) = mk res2 w w1 w2
      in
        (res3, w3 , g2)
        
evalOp :: Int -> Int -> Op -> Int
evalOp u1 u2 op =
  if op b1 b2 then 1 else 0
  where
    b1 = u1 == 1
    b2 = u2 == 1

test = Or (Var 1) (Var 3)
