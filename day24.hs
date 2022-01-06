import Utils

import Data.Maybe (fromJust)
import Data.Char (isDigit)
import Data.Either (isLeft, fromLeft, isRight, fromRight)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Ops = (String, String, Either Int String)

data OpTree = Inp String
            | Add OpTree OpTree
            | Mul OpTree OpTree
            | Div OpTree OpTree
            | Mod OpTree OpTree
            | Eql OpTree OpTree
            | Const Int deriving Eq

instance Show OpTree where
    show (Inp a) = a
    show (Const i) = show i
    show (Add a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
    show (Mul a b) = "(" ++ show a ++ "*" ++ show b ++ ")"
    show (Div a b) = "(" ++ show a ++ "/" ++ show b ++ ")"
    show (Mod a b) = "(" ++ show a ++ "%" ++ show b ++ ")"
    show (Eql a b) = "(" ++ show a ++ "=" ++ show b ++ ")"


main :: IO ()
main = do
    input <- map parseOps . lines <$> readFile "24.txt"
    let operations = sepOps 0 input
    let trees = stitchSyntaxTree' operations
    let tree = fromJust $ Map.lookup "z" trees
    let (decomTree, mi, ma) = graphDecom tree
    let ind = ['w':show i | i <- [0..13]]
    let sub = zip ind [9,9,9,9,2,9,9,2,7,9,6,1]
    --print sub
    let (decomSub, s1, s2) = graphDecom $ substituteInp (Map.fromList sub) decomTree
    let (treeSub, t1, t2) = graphDecom $ substituteInp (Map.fromList sub) tree
    print $ findOrd decomTree [9,8..1]
    --print (t1, t2)
    print $ findOrd decomTree [1..9]

findOrd :: OpTree -> [Int] -> Maybe [Int]
findOrd tree order = findMax' 0 order tree
    where
        findMax' _ [] _ = Nothing
        findMax' 14 _ _ = Just []
        findMax' n (x:xs) tree
            | low > 0 = findMax' n xs tree
            | otherwise = case findMax' (n+1) order tree' of
                Nothing -> findMax' n xs tree
                Just ns -> Just (x:ns)
            where
                w = 'w':show n
                (tree',low,_) = graphDecom $ substituteInp (Map.singleton w x) tree

substituteInp :: Map String Int -> OpTree -> OpTree
substituteInp mp (Inp v) = case Map.lookup v mp of
    Nothing -> Inp v
    Just i -> Const i
substituteInp _ (Const c) = Const c
substituteInp mp (Add a b) = let a' = substituteInp mp a; b' = substituteInp mp b in condenseAdd (Add a' b')
substituteInp mp (Mul a b) = let a' = substituteInp mp a; b' = substituteInp mp b in condenseMul (Mul a' b')
substituteInp mp (Div a b) = let a' = substituteInp mp a; b' = substituteInp mp b in condenseDiv (Div a' b')
substituteInp mp (Mod a b) = let a' = substituteInp mp a; b' = substituteInp mp b in condenseMod (Mod a' b')
substituteInp mp (Eql a b) = let a' = substituteInp mp a; b' = substituteInp mp b in condenseEql (Eql a' b')

findInvariants :: OpTree -> [String]
findInvariants t = Set.toList $ Set.difference fullSet posSet
    where
        fullSet = Set.fromList ['w':show i | i <- [0..13]]
        posSet = findInv' Set.empty t
        findInv' s (Inp v) = Set.insert v s
        findInv' s (Const c) = s
        findInv' s v = case getOperators v of
            Nothing -> s
            Just (a, b) -> findInv' (findInv' s a) b

getOperators :: OpTree -> Maybe (OpTree, OpTree)
getOperators (Add a b) = Just (a, b)
getOperators (Mul a b) = Just (a, b)
getOperators (Div a b) = Just (a, b)
getOperators (Mod a b) = Just (a, b)
getOperators (Eql a b) = Just (a, b)
getOperators a = Nothing

graphDecom :: OpTree -> (OpTree, Int, Int) -- (graph, min, max)
graphDecom (Inp v) = (Inp v, 1, 9)
graphDecom (Const c) = (Const c, c, c)
graphDecom (Add a b) = let (a', m1, m2) = reduce $ graphDecom a; (b', m1', m2') = reduce $ graphDecom b
                       in (condenseAdd $ Add a' b', m1 + m1', m2 + m2')
graphDecom (Mul a b) = let (a', m1, m2) = reduce $ graphDecom a; (b', m1', m2') = reduce $ graphDecom b; mi = m1 * m1'; ma = m2 * m2'
                       in reduce (condenseMul $ Mul a' b', min mi ma, max mi ma)
graphDecom (Div a b) = let (a', m1, m2) = reduce $ graphDecom a; (b', m1', m2') = reduce $ graphDecom b; mi = m1 `div` m1'; ma = m2 `div` m2'
                       in reduce (condenseDiv $ Div a' b', min mi ma, max mi ma)
graphDecom (Mod a b) = let (a', m1, m2) = reduce $ graphDecom a; (b', m1', m2') = reduce $ graphDecom b;
                           exelBound = m2 > m2' && (m1 /= m2)
                           mi | m1 < 0 = error "mod neg a" | exelBound = 0 | otherwise = m1 `mod` m1'
                           ma | m2 < 0 = error "mod neg b" | exelBound = m2' - 1 | otherwise = m2 `mod` m2'
                       in reduce (condenseMod $ Mod a' b', min mi ma, max mi ma)
graphDecom (Eql a b) = let (a', m1, m2) = reduce $ graphDecom a; (b', m1', m2') = reduce $ graphDecom b;
                           withinA = (m1' <= m2 && m2 <= m2') || (m1' <= m1 && m1 <= m2')
                           withinB = (m1 <= m2' && m2' <= m2) || (m1 <= m1' && m1' <= m2)
                           (mi, ma) | m1 == m2 && m1' == m2' && m1 == m1' = (1, 1) | withinA || withinB = (0, 1) | otherwise = (0, 0)
                       in reduce (condenseEql $ Eql a' b', mi, ma)

reduceMul :: OpTree -> OpTree
reduceMul = reduceMul' 1
    where
        reduceMul' n (Inp v) = Mul (Inp v) (Const n)
        reduceMul' n (Const a) = Const (a * n)
        reduceMul' n (Add a b) = Add (reduceMul' n a) (reduceMul' n b)
        reduceMul' n (Mul a (Const b)) = reduceMul' (n * b) a
        reduceMul' n (Mul (Const a) b) = reduceMul' (n * a) b
        reduceMul' n (Mul a b) = Mul (reduceMul' n a) (reduceMul' n b)
        reduceMul' n (Div a b) = Div (reduceMul' n a) (reduceMul' n b)
        reduceMul' n (Mod a b) = Mod (reduceMul' n a) (reduceMul' n b)
        reduceMul' n (Eql a b) = Eql (reduceMul' n a) (reduceMul' n b)

{-
>>> a = Eql (Eql (Inp "w") (Const 1)) (Const 0)
>>> graphDecom a
(((w=1)=0),0,1)
-}

eql :: Eq a => a -> a -> Int
eql a b = fromEnum (a == b)

reduce :: (OpTree, Int, Int) -> (OpTree, Int, Int)
reduce (a, mi, ma) = if mi == ma then (Const mi, mi, ma) else (a, mi, ma)

stitchSyntaxTree' :: [[Ops]] -> Map String OpTree
stitchSyntaxTree' xs = trees
    where trees = foldl (\acc xt ->
            let (_, v, _) = head xt
                tr = stitchOnce acc xt
            in Map.alter (const Nothing) v tr) (Map.fromList [("x", Const 0), ("y", Const 0), ("z", Const 0)]) xs

stitchOnce :: Map String OpTree -> [Ops] -> Map String OpTree
stitchOnce acc xt = let (_,v,_) = head xt
                        xt' = map (\(o, v', t) -> if v' == "w" then (o, v, t) else
                            if fromRight "." t == "w" then (o,v',Right v) else (o,v',t)) xt
                        tr = foldl stitchTree' acc xt'
                    in tr

stitchTree' :: Map String OpTree -> Ops -> Map String OpTree
stitchTree' mp ("inp", v, _) = Map.insert v (Inp v) mp
stitchTree' mp (o, v, t)
    | o == "add" = putBack $ condenseAdd $ Add subTreeL subTreeR
    | o == "mul" = putBack $ condenseMul $ Mul subTreeL subTreeR
    | o == "div" = putBack $ condenseDiv $ Div subTreeL subTreeR
    | o == "mod" = putBack $ condenseMod $ Mod subTreeL subTreeR
    | otherwise  = putBack $ condenseEql $ Eql subTreeL subTreeR
    where
        (Just subTreeL) = Map.lookup v mp
        subTreeR = case t of
            Left i -> Const i
            Right c -> let (Just str) = Map.lookup c mp in str
        putBack tree = Map.update (const (Just tree)) v mp


condenseAdd (Add (Const a) (Const b)) = Const (a + b)
condenseAdd (Add a         (Const 0)) = a
condenseAdd (Add (Const 0)         a) = a
condenseAdd a = a

condenseMul (Mul (Const a) (Const b)) = Const (a * b)
condenseMul (Mul _         (Const 0)) = Const 0
condenseMul (Mul (Const 0)         _) = Const 0
condenseMul (Mul a         (Const 1)) = a
condenseMul (Mul (Const 1)         a) = a
condenseMul a = a

condenseDiv (Div (Const a) (Const b)) = Const (a `div` b)
condenseDiv (Div a         (Const 1)) = a
condenseDiv a = a

condenseMod (Mod (Const a) (Const b)) = Const (a `mod` b)
condenseMod (Mod a         (Const 1)) = a
condenseMod a = a

condenseEql (Eql (Const a) (Const b)) = Const (a `eql` b)
condenseEql (Eql (Inp   a) (Inp   b))
    | a == b = Const 1
    | otherwise = Eql (Inp   a) (Inp   b)
condenseEql a = a

getVar (Left i) = Const i
getVar (Right c) = Inp c
getOp (o, v, t)
    | o == "add" = Add (Inp v) (getVar t)
    | o == "mul" = Mul (Inp v) (getVar t)
    | o == "div" = Div (Inp v) (getVar t)
    | o == "mod" = Mod (Inp v) (getVar t)
    | otherwise = Eql (Inp v) (getVar t)

sepOps :: Int -> [Ops] -> [[Ops]]
sepOps n [] = []
sepOps n ((o, v, t):xs) = ((o, v ++ show n, t):h):sepOps (n+1) e
    where
        h = takeWhile (\(op, _, _) -> op /= "inp") xs
        e = dropWhile (\(op, _, _) -> op /= "inp") xs

parseOps :: String -> Ops
parseOps xs
    | length spVal == 2 = ("inp", var, Left (-1))
    | otherwise = (op, var, t)
    where
        spVal = split " " xs
        op = head spVal
        var = spVal !! 1
        t = let temp = last spVal in if isDigit (last temp)
            then Left (read temp) else Right temp
