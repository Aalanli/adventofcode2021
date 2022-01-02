import Utils

import Data.Map (Map)
import qualified Data.Map as Map

type CountMap = Map String Int
type CharMap = Map String Char

main :: IO ()
main = do
    [a, b] <- split [""] . lines <$> readFile "14.txt"
    let charMap = Map.fromList $ map ((\[x, y] -> (x, head y)) . split " -> ") b
    let initial = head a
    let up10 = foldr (\x acc -> updateOnce acc charMap) initial [1..10]
    let counts = foldr (adjustDefault 1 (+1)) Map.empty up10
    let vals = map snd $ Map.toList counts
    print (maximum vals - minimum vals)
    let countMap = foldr (adjustDefault 1 (+1)) Map.empty $ getPairs initial
    let up40 = foldr (\x acc -> efficientUpdate charMap acc) countMap [1..40]
    let charCounts' = getCharCounts up40
    let charCounts'' = let start = head initial
                           end = last initial
                        in Map.map (`div` 2) $ foldr (Map.adjust (+1)) charCounts' [start, end]
    let vals' = map snd $ Map.toList charCounts''
    print (maximum vals' - minimum vals')
    

getPairs :: [a] -> [[a]]
getPairs [] = []
getPairs [a] = [[a]]
getPairs [a, b] = [[a, b]]
getPairs (a:b:xs) = [a, b]:getPairs (b:xs)

efficientUpdate :: CharMap -> CountMap -> CountMap
efficientUpdate h s = efficientUpdate' h (Map.toList s) Map.empty
    where 
        efficientUpdate' _ [] s' = s'
        efficientUpdate' h ((v, n):xs) s' = case Map.lookup v h of
            Nothing -> efficientUpdate' h xs $ adjustDefault n (+n) v s'
            Just c -> efficientUpdate' h xs $ foldr (adjustDefault n (+n)) s' [c:tail v, [head v, c]]

getCharCounts :: CountMap -> Map Char Int
getCharCounts cm = foldr (\([f,s], n) acc -> adjustDefault n (+n) f (adjustDefault n (+n) s acc)) Map.empty (Map.toList cm)

updateOnce :: String -> CharMap -> String
updateOnce [] c = []
updateOnce [a] c = [a]
updateOnce (a:b:xs) c = case Map.lookup [a, b] c of
    Nothing -> a:updateOnce (b:xs) c
    Just h -> a:h:updateOnce (b:xs) c

adjustDefault :: Ord k => a -> (a -> a) -> k -> Map k a -> Map k a
adjustDefault def f k mp
    | Map.member k mp = Map.adjust f k mp
    | otherwise = Map.insert k def mp

