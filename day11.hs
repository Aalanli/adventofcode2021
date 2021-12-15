import Utils

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

type Grid = Map Int Int

main :: IO ()
main = do
    input <- map (map read . unsqueeze) . lines <$> readFile "11.txt" :: IO [[Int]]
    let (x, y) = (length (head input), length input)
    let grid = Map.fromList $ zip [0..x*y-1] (flatten input)
    let n = simulateN (x, y) 100 grid
    captionPrint "A: " n
    let fstSync = firstSync 1 (x, y) grid
    captionPrint "B: " fstSync


simulateN :: (Int, Int) -> Int -> Grid -> Int
simulateN p 0 ms = 0
simulateN p n ms = let (ms', s) = simulateStep p ms in s + simulateN p (n-1) ms'

firstSync :: Int -> (Int, Int) -> Grid -> Int
firstSync n p g
    | isAllZero g' = n
    | otherwise = firstSync (n+1) p g'
    where (g', _) = simulateStep p g
          isAllZero = Map.foldr (\ a acc -> a == 0 && acc) True

simulateStep :: (Int, Int) -> Grid -> (Grid, Int)
simulateStep (x, y) ms = (afterFlashes, Set.size flashes)
    where
        (curStep, flashes) = simulateStep' updatedStep Set.empty
        afterFlashes = zeroMap (Set.toList flashes) curStep
        updatedStep = Map.map (+1) ms
        zeroMap xs ms = foldl (flip (Map.update (\_ -> Just 0))) ms xs
        updateMap xs ms = foldl (flip (Map.adjust (+ 1))) ms xs
        updateSet xs sx = foldl (flip Set.insert) sx xs
        simulateStep' ms bs
            | null newBs = (ms', bs)
            | otherwise = simulateStep' ms' $ updateSet newBs bs
            where
                keyAccess k a result = if not (Set.member k bs) && a > 9 then k:result else result
                newBs = Map.foldrWithKey keyAccess [] ms
                updateCoords = flatten $ map (getCoord (x, y)) newBs
                ms' = updateMap updateCoords ms

showGrid :: Grid -> IO ()
showGrid ms = printByCol $ unFlatten 5 (getGrid ms)

getGrid :: Map b1 b2 -> [b2]
getGrid ms = map snd (Map.toList ms)

getCoord :: Integral a => (a, a) -> a -> [a]
getCoord (x, y) a = map (\(a, b) -> x * b + a) $ filter (\(a, b) -> 0 <= a && a < x && 0 <= b && b < y) ps
    where
        (ax, ay) = let yVal = a `div` x in (a - yVal * x, yVal)
        ps = [(ax + i, ay + j) | i <- [-1..1], j <- [-1..1], i /= 0 || j /= 0]
