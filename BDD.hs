module BDD(BDDNode, BExp(..), mk, build, apply, restrict, satCount, anySat, rename) where
import Data.HashMap.Lazy as Map
import Data.Set as Set

type Value = Bool
type Variable = Int
type Node = Int
type Edge = Maybe Int
data BExp = Var Variable
          | Val Value
          | Neg BExp
          | And BExp BExp
          | Or BExp BExp
          | Imp BExp BExp
          | Eq BExp BExp
          deriving (Show, Eq)

type Triple = (Int, Edge, Edge)

type BDD = (HashMap Int Triple, HashMap Triple Int)
type BDDNode = (BDD, Node)

mk :: BDD -> Int -> Node -> Node -> BDDNode
mk bdd@(t, h) i low high
  | low == high = (bdd, low)
  | Map.member (i, Just low, Just high) h = (bdd, h ! (i, Just low, Just high))
  | otherwise =
    let 
      u = Map.size t + 1
      newT = Map.insert u (i, Just low, Just high) t
      newH = Map.insert (i, Just low, Just high) u h
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
assign (Imp e1 e2) x v = Imp (assign e1 x v) (assign e2 x v)
assign (Eq e1 e2) x v = Eq (assign e1 x v) (assign e2 x v)

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
calculate (Imp e1 e2) =
  calculate $ Or (Neg e1) e2
calculate (Eq e1 e2) =
  calculate $ And (Imp e1 e2) (Imp e2 e1)
calculate e = error $ show e

-- Assuming, that no values are assigned
maxVar :: BExp -> Int
maxVar (Var v) = v
maxVar (Val v) = 0
maxVar (Neg e) = maxVar e
maxVar (Or e1 e2) = max (maxVar e1) $ maxVar e2
maxVar (And e1 e2) = max (maxVar e1) $ maxVar e2
maxVar (Imp e1 e2) = max (maxVar e1) $ maxVar e2
maxVar (Eq e1 e2) = max (maxVar e1) $ maxVar e2


initBDD :: Variable  -> BDD
initBDD n =
  (Map.fromList [(0, v), (1, v)], Map.empty)
  where
    v = (n, Nothing, Nothing)

build :: BExp -> Int -> BDDNode
build b n = let
  v = (n + 1, Nothing, Nothing)
  in
    buildAux b n (Map.fromList [(0, v), (1, v)], Map.empty) 1

buildAux :: BExp -> Int -> BDD -> Variable -> BDDNode
buildAux b n bdd@(t, h) i
  | i > n =
    if calculate b == False then (bdd, 0)
    else (bdd, 1)
  | otherwise =
    let
      (bdd1, v0) = buildAux (assign b i False) n bdd (i + 1)
      (bdd2, v1) = buildAux (assign b i True) n bdd1 (i + 1)
    in mk bdd2 i v0 v1

type Arr2D = HashMap (Node, Node) Node
type Op = Bool -> Bool -> Bool

countVariables :: BDD -> Int
countVariables (t, _) = maximum [v | (_, (v, _, _)) <- Map.toList t]  

apply :: BDDNode -> BDDNode -> Op -> BDDNode
apply (b1, u1) (b2, u2) op =
  (b, u)
  where
    n1 = countVariables b1
    n2 = countVariables b2
    n = max n1 n2
    start = initBDD n
    (b, u, _) = app b1 b2 op u1 u2 start (Map.empty)
    

app :: BDD -> BDD -> Op -> Node -> Node -> BDD ->  Arr2D -> (BDD, Node, Arr2D)
app bdd1@(t1, h1) bdd2@(t2, h2) op u1 u2 res g =
  let
    (res', u', g') =
      if Map.member (u1, u2) g then (res, g ! (u1, u2), g)
      else if u1 < 2 && u2 < 2 then (res, evalOp u1 u2 op, g)
           else if v1 == v2 then mkApp v1 low1 low2 high1 high2
                else if v1 < v2 then mkApp v1 low1 u2 high1 u2
                     else mkApp v2 u1 low2 u1 high2
  in
    (res', u', Map.insert (u1, u2) u' g')
  where
    (v1, _, _) = t1 ! u1
    (v2, _, _) = t2 ! u2
    (_, Just low1, Just high1) = t1 ! u1
    (_, Just low2, Just high2) = t2 ! u2
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

restrict :: BDDNode -> Variable -> Value -> BDDNode
restrict bddNode j b =
  restrictAux bddNode
  where
  -- restrictAux :: BDDNode -> BDDNode
  restrictAux bn@(bdd, u) =
    let
      resLow = restrictAux (bdd, countLow bn)
      resHigh = restrictAux (bdd, countHigh bn)
      resLowHigh = restrictAux (fst resLow, countHigh bn)
      newBDD = fst resLowHigh
      v = countVar bn
    in
      if v > j then bn
      else if v < j then mk (fst resLowHigh) v (snd resLow) (snd resLowHigh)
           else if b == False then resLow
                else resHigh


countVar :: BDDNode -> Variable
countVar ((t, _), u) = v
  where
    (v, _, _) = t ! u

countLow :: BDDNode -> Node
countLow ((t, _), u) = v
  where
    (_, Just v, _) = t ! u

countHigh :: BDDNode -> Node
countHigh ((t, _), u) = v
  where
    (_, _, Just v) = t ! u


satCount :: BDDNode -> Int
satCount (bdd@(t, _), node) =
  2^(vNode - 1) * (count node) 
  where
    (vNode, _, _) = t ! node
    count u =
      if u == 0 then 0
      else if u == 1 then 1
           else 2^(countVar (bdd, countLow (bdd, u)) - countVar (bdd, u) - 1)
                * count (countLow (bdd, u))
                +  2^(countVar (bdd, countHigh (bdd, u)) - countVar (bdd, u) - 1)
                * count (countHigh (bdd, u))

anySat :: BDDNode -> [(Variable, Value)]
anySat (bdd, u) =
  if u == 0 then error "No truth-assignment exists"
  else if u == 1 then []
       else if countLow (bdd, u) == 0 then
              ((countVar (bdd, u), True) : anySat (bdd, countHigh (bdd, u)))
            else ((countVar (bdd, u), False) : anySat (bdd, countLow (bdd, u)))


createH :: HashMap Int Triple -> HashMap Triple Int
createH t = Map.fromList $ Prelude.map (\(a,b) -> (b,a)) $ Map.toList t

rename :: BDDNode -> [Variable] -> [Variable] -> BDDNode
rename ((t, h), u) old new =
  let
    rep = Map.fromList $ zip old new
    f (v, low, high)
          | Map.member v rep = Just (rep ! v, low, high)
          | otherwise = Just (v, low, high)
    newT = mapMaybe f t
    newH = createH newT
  in ((newT, newH), u)
  

countNodes :: BDDNode -> Int
countNodes (bdd, u) =
  Set.size $ countNodesAux bdd u Set.empty

countNodesAux :: BDD -> Int -> Set.Set Int -> Set.Set Int
countNodesAux bdd@(t, _) u vis =
  if Set.member u vis then vis
  else
    let
      (_, low, high) = t ! u
      vis' = Set.insert u vis
      vis'' = case low of 
        (Just l) -> countNodesAux bdd l vis'
        _ -> vis'
      vis''' = case high of 
        (Just l) -> countNodesAux bdd l vis''
        _ -> vis''
    in
      vis'''
      
    

-- test for build
test0 = And (Eq (Var 1) (Var 2)) (Eq (Var 3) (Var 4))

-- test for apply
test11 :: BDD
test11 = (Map.fromList [(0, (6, Nothing, Nothing)), (1, (6, Nothing, Nothing)), (2, (5, Just 1, Just 0)), (3, (4, Just 2, Just 0)), (4, (4, Just 0, Just 2)), (5, (3, Just 3, Just 4)), (6, (2, Just 5, Just 0)), (7, (2, Just 0, Just 5)), (8, (1, Just 6, Just 7))],
          Map.fromList  [((6, Nothing, Nothing), 0), ((6, Nothing, Nothing), 1), ((5, Just 1, Just 0), 2), ((4, Just 2, Just 0), 3), ((4, Just 0, Just 2), 4), ((3, Just 3, Just 4), 5), ((2, Just 5, Just 0), 6), ((2, Just 0, Just 5), 7), ((1, Just 6, Just 7), 8)]) 

test12 :: BDD
test12 = (Map.fromList [(0, (6, Nothing, Nothing)), (1, (6, Nothing, Nothing)), (2, (5, Just 1, Just 0)), (3, (3, Just 2, Just 0)), (4, (3, Just 0, Just 2)), (5, (1, Just 3, Just 4))], Map.fromList [((6, Nothing, Nothing), 0), ((6, Nothing, Nothing), 1), ((5, Just 1, Just 0), 2), ((3, Just 2, Just 1), 3), ((3, Just 0, Just 2), 4), ((1, Just 3, Just 4), 5)])

-- test for build and apply
--test21 = build $ Eq (Var 1) (Var 2)
--test22 = build $ Eq (Var 3) (Var 4)
-- you would have to keep additionally the number of variables in each bdd


-- test for restrict
test3 = Or (Eq (Var 1) (Var 2)) (Var 3)
