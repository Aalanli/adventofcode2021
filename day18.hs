import Utils

import Data.Char (isDigit, digitToInt)


data Pair = Pair {depth::Int, left::Pair, right::Pair} | Root {value::Int, depth::Int}
data Carry = BothC Int Int | LeftC Pair Int | RightC Pair Int | NoC Pair deriving Show
data Change a = NoChange | Changed a

instance Show Pair where
    show (Root n i) = show n
    show Pair {depth=d, left=l, right=r} = "[" ++ show l ++ "," ++ show r ++ "]"

main :: IO ()
main = do
    input <- lines <$> readFile "18.txt"
    let pairs = map parsePair input
    let finalSum = addAll pairs
    print $ magnitude finalSum
    print $ largestPair pairs


largestPair :: [Pair] -> Int
largestPair [] = 0
largestPair (x:xs) = max (largestPair' x xs) $ largestPair xs
    where
        largestPair' p = foldr (\x acc -> max (max acc (magnitude (addReduce p x))) (magnitude (addReduce x p))) 0

magnitude :: Pair -> Int
magnitude (Root n _) = n
magnitude (Pair _ l r) = 3 * magnitude l + 2 * magnitude r

addReduce :: Pair -> Pair -> Pair
addReduce p1 p2 = reducePair $ addPair p1 p2

addAll :: [Pair] -> Pair
addAll = foldl1 addReduce

reducePair :: Pair -> Pair
reducePair p = case reduceExplode p of
    (NoC p') -> case reduceSplit p' of
        NoChange -> p'
        Changed p'' -> reducePair p''
    other -> p

reduceSplit :: Pair -> Change Pair
reduceSplit (Root n d)
    | n > 9 = let l = n `div` 2
                  r = n - l
              in Changed $ Pair d (Root l (d+1)) (Root r (d+1))
    | otherwise = NoChange
reduceSplit (Pair d l r) = case reduceSplit l of
    Changed l' -> Changed $ Pair d l' r
    NoChange -> case reduceSplit r of
        Changed r' -> Changed $ Pair d l r'
        NoChange -> NoChange

reduceExplode :: Pair -> Carry
reduceExplode (Root n d) = NoC (Root n d)
reduceExplode (Pair 4 (Root v1 d1) (Root v2 d2)) = BothC v1 v2
reduceExplode (Pair d l r) = case reduceExplode l of
    (BothC n1 n2) -> LeftC (Pair d (Root 0 (d+1)) (addLeft n2 r)) n1
    (LeftC pl n) -> if d > 0 then LeftC (Pair d pl r) n else reduceExplode (Pair d pl r)
    (RightC pl n) -> reduceExplode $ Pair d pl (addLeft n r)
    (NoC pl) -> case reduceExplode r of
        (BothC n1' n2') -> RightC (Pair d (addRight n1' pl) (Root 0 (d+1))) n2'
        (LeftC pr n) -> reduceExplode (Pair d (addRight n pl) pr)
        (RightC pr n) -> if d > 0 then RightC (Pair d pl pr) n else reduceExplode (Pair d pl pr)
        (NoC pr) -> NoC (Pair d pl pr)

addPair :: Pair -> Pair -> Pair
addPair l r = Pair 0 (increaseDepth l) (increaseDepth r)

increaseDepth :: Pair -> Pair
increaseDepth (Root v d) = Root v (d+1)
increaseDepth (Pair d l r) = Pair (d+1) (increaseDepth l) (increaseDepth r)

addLeft :: Int -> Pair -> Pair
addLeft n (Root v d) = Root (v + n) d
addLeft n (Pair d l r) = Pair d (addLeft n l) r

addRight :: Int -> Pair -> Pair
addRight n (Root v d) = Root (v + n) d
addRight n (Pair d l r) = Pair d l (addRight n r)

parsePair :: String -> Pair
parsePair xs = let (Just (a, b)) = parsePair' 0 xs in a
    where
        parsePair' _ [] = Nothing
        parsePair' d (x:xs)
            | x == '[' = let (Just (left', lt)) = parsePair' (d+1) xs
                             (Just (right', rt)) = parsePair' (d+1) lt
                         in Just (Pair d left' right', rt)
            | isDigit x = Just (Root (read (takeWhile isDigit (x:xs))) d, dropWhile isDigit xs)
            | otherwise = parsePair' d xs
