import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Utils
import Data.Bifunctor (second)


main :: IO ()
main = do
    input <- map (\x -> let (a, b) = cutAt ((=='|') . head) x in (split " " a, split " " (drop 2 b))) . lines <$> readFile "8.txt"
    let specialCount xs = let l = length xs in if l == 2 || l == 4 || l == 3 || l == 7 then 1 else 0
    let special = foldr (\(_, b) acc -> acc + foldr ((+) . specialCount) 0 b) 0 input
    captionPrint "A: " special
    let sum = foldr (\(a, b) acc -> solve a b + acc) 0 input
    captionPrint "B: " sum


solve :: Ord a => [[a]] -> [[a]] -> Int
solve xs ys = read $ concatMap (show . decode) ys
    where
        mappings = solve' $ aggregate xs
        decode x = fst . head $ filter ((==Set.fromList x) . snd) mappings

solve' :: Ord a => [(Int, [Set a])] -> [(Int, Set a)]
solve' xs = solve'' ones [] other
    where
        filterOne = filter ((==1) . length . snd)
        filterNotOne = filter ((/=1) . length . snd)
        ones = map (second head) $ filterOne xs
        other = filterNotOne xs
        intersectionSize a b = Set.size $ Set.intersection a b
        filterIntersections coordA setA coordB setsB = let
            nCoordIntersections = intersectionSize coordA coordB
            in filter ((==nCoordIntersections) . intersectionSize setA) setsB
        solve'' a b [] = a ++ b
        solve'' [] b _ = b
        solve'' ((n,x):xs) b hs = solve'' (newOnes ++ xs) ((n,x):b) others
            where
                newOnes = map (second head) $ filterOne filteredHs
                others = filterNotOne filteredHs
                filteredHs = let
                    (Just coordA) = Map.lookup n positionsMap
                    in map (\(nB, setsB) -> let
                        (Just coordB) = Map.lookup nB positionsMap
                        filteredB = filterIntersections coordA x coordB setsB
                        in (nB, filteredB)) hs

positions :: [(Int, [Int])]
positions = [
    (1, [3, 6]),
    (2, [1, 3, 4, 5, 7]),
    (3, [1, 3, 4, 6, 7]),
    (4, [2, 3, 4, 6]),
    (5, [1, 2, 4, 6, 7]),
    (6, [1, 2, 4, 5, 6, 7]),
    (7, [1, 3, 6]),
    (8, [1, 2, 3, 4, 5, 6, 7]),
    (9, [1, 2, 3, 4, 6, 7]),
    (0, [1, 2, 3, 5, 6, 7])]

positionsMap :: Map Int (Set Int)
positionsMap = Map.fromList $ map (second Set.fromList) positions

insertPossible :: Ord a => [a] -> Map Int [Set a] -> Map Int [Set a]
insertPossible xs mms
    | len == 2 = insertEvery [1]
    | len == 3 = insertEvery [7]
    | len == 4 = insertEvery [4]
    | len == 5 = insertEvery [2, 3, 5]
    | len == 6 = insertEvery [6, 9, 0]
    | otherwise = insertEvery [8]
    where
        len = length xs
        sxs = Set.fromList xs
        insertEvery = foldr (Map.adjust (sxs:)) mms

aggregate :: Ord a => [[a]] -> [(Int, [Set a])]
aggregate xs = Map.toList ms
    where ms = foldr insertPossible start xs
          start = Map.fromList [(n, []) | n <- [0..9]]

