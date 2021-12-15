import Utils

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Char (isLower)
import Data.Maybe (fromMaybe)

type Connections = [(String, String)]
type Paths = Map String [String]
type Visited = Set String


main :: IO ()
main = do
    input <- map ((\[x, y] -> (x, y)) . split "-") . lines <$> readFile "12.txt"
    let lowerNodes = Set.toList $ Set.fromList $ filter (\x -> isLower (head x) && x /= "end" && x /= "start") $ concatMap (\(x, y) -> [x, y]) input
    let lowerNodePerms = map (Map.fromList . zip lowerNodes) $ interweave 2 (replicate (length lowerNodes - 1) 1)
    let places = makeMap input
    let paths = calculatePaths places
    captionPrint "A: " paths
    let paths' = foldr (\x acc -> joinSet acc (calculatePaths2 places x)) Set.empty lowerNodePerms
    captionPrint "B: " $ Set.size paths'


makeMap :: Connections -> Paths
makeMap xs = foldr (\(k, a) acc -> if a == "start" then acc else if Map.member k acc then Map.adjust (a:) k acc else Map.insert k [a] acc) Map.empty (xs ++ xs')
    where xs' = map (\(x, y) -> (y, x)) xs

calculatePaths :: Paths -> Int
calculatePaths ms = calculatePaths' "start" ms Set.empty
    where
        calculatePaths' node ps vis
            | node == "end" = 1
            | null possiblePaths = 0
            | otherwise = calculateLower possiblePaths
            where
                (Just paths) = Map.lookup node ps
                possiblePaths = filter (`Set.notMember` vis) paths
                calculateLower = foldr (\x acc -> acc + calculatePaths' x ps (if isLower (head x) then Set.insert x vis else vis)) 0

type Counter = Map String Int

calculatePaths2 :: Paths -> Counter -> [[String]]
calculatePaths2 = calculatePaths' "start"
    where
        calculatePaths' :: String -> Paths -> Counter -> [[String]]
        calculatePaths' node ps vis
            | node == "end" = [["end"]]
            | null possiblePaths = []
            | curNode == 0 = []
            | otherwise = map (node:) $ calculateLower possiblePaths
            where
                curNode = Data.Maybe.fromMaybe 1 (Map.lookup node vis)
                (Just paths) = Map.lookup node ps
                possiblePaths = filter (\x -> let a = Data.Maybe.fromMaybe 1 (Map.lookup node vis) in a > 0) paths
                calculateLower = foldr (\x acc -> case calculatePaths' x ps (if isLower (head node) then Map.adjust (\x -> x-1) node vis else vis) of
                                                  [] -> acc
                                                  xs -> xs ++ acc) []