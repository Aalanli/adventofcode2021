{-# LANGUAGE TupleSections #-}
import Utils

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.Set (Set)
import qualified Data.Set as Set

type Coord = (Int, Int)

main :: IO ()
main = do
    input <- map (map read . unsqueeze) . lines <$> readFile "15.txt" :: IO [[Int]]
    let (arr, (x, y)) = format input
    let cost = aSearch (x, y) arr
    print $ IntMap.lookup (x*y-1) cost
    let (arr', (x', y')) = format $ tile input
    let cost' = aSearch (x', y') arr'
    print $ IntMap.lookup (x'*y'-1) cost'

format :: [[a]] -> (IntMap a, (Int, Int))
format xs = let (x, y) = (length (head xs), length xs)
            in (IntMap.fromList $ zip [0..x*y-1] $ flatten xs, (x, y))

tile :: [[Int]] -> [[Int]]
tile = tileY . tileX
    where
        tileX = map (\x -> concat [map (wrapSum n) x | n <- [0..4]])
        tileY xs = concatMap (\ x -> map (map (wrapSum x)) xs) [0..4]
        wrapSum x n = if x + n > 9 then x + n - 9 else x + n

aSearch :: (Ord a, Num a) => (IntMap.Key, IntMap.Key) -> IntMap a -> IntMap a
aSearch (x, y) mp = aSearch' [(0, 0)] (IntMap.fromList [(0, 0)])
    where
        aSearch' posSet minFromS
            | null posSet = minFromS'
            | otherwise = aSearch' posSet' minFromS'
            where
                nextConnected = removeDuplicates $ concatMap (\(pos, prev) -> map (, pos) (filter (/=prev) (getCoord (x, y) pos))) posSet
                (minFromS', posSet') = foldr (\(pos, prev) (m, p) -> let (Just curMinFromS) = IntMap.lookup prev m
                                                                         (Just curCost) = IntMap.lookup pos mp
                                                                         nCost = curCost + curMinFromS in
                        case IntMap.lookup pos m of
                        Nothing -> (IntMap.insert pos nCost m, (pos, prev):p)
                        Just c -> if nCost > c then (m, p)
                                  else (IntMap.update (\_ -> Just nCost) pos m, (pos, prev):p)) (minFromS, []) nextConnected


extractList :: Int -> IntMap a -> [[a]]
extractList x mp = let list = map snd (IntMap.toList mp) in unFlatten x list

getCoord (x, y) a = ax ++ ay
    where
        stx = a `div` x * x
        ax = filter (\h -> stx <= h && h < stx + x) [a+1, a-1]
        ay = filter (\h -> 0 <= h && h < x*y) [a+x, a-x]


