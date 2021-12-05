{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Map (Map)
import qualified Data.Map as Map
import Utils

type Point = (Int, Int)
type Counter = Map Point Int

{-
>>> convertCoord ((1, 3), (4, 6))
[(1,3),(2,4),(3,5),(4,6)]
-}

main :: IO ()
main = do
    ln <- fmap lines (readFile "5.txt")
    let toTuple [x, y] = (x, y)
    let pairs = map (toTuple . map (toTuple . map read . split ",") . split " -> ") ln :: [(Point, Point)]
    let coords = flatten . map convertCoord $ filter (\((a, b), (c, d)) -> a == c || b == d) pairs
    let instances = foldl mapInstances Map.empty coords
    let count = foldr (\x acc -> acc + if x > 1 then 1 else 0) 0 instances
    captionPrint "A: " count
    let coords' = flatten . map convertCoord $ pairs
    let instances' = foldl mapInstances Map.empty coords'
    let count' = foldr (\x acc -> acc + if x > 1 then 1 else 0) 0 instances'
    captionPrint "B: " count'


mapInstances :: Counter -> Point -> Counter
mapInstances c p = case Map.lookup p c of
    Just i -> Map.adjust (+1) p c
    Nothing -> Map.insert p 1 c

convertCoord :: (Point, Point) -> [Point]
convertCoord ((a, b), (c, d))
    | b == d = zip (arange a c) (replicate (abs (c-a) + 1) b)
    | a == c = zip (replicate (abs (d-b) + 1) a) (arange b d)
    | otherwise = zip (arange a c) (arange b d)

arange a b
    | a > b = [a,a-1..b]
    | otherwise = [a..b]

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = (if x `elem` xs then id else (x:)) $ uniq xs
