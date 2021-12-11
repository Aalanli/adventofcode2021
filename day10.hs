{-# LANGUAGE LambdaCase #-}
import Utils

import Data.Map (Map)
import qualified Data.Map as Map

data LineAttr a b = Valid | InValid a | Incomplete b deriving (Show)


main :: IO ()
main = do
    input <- lines <$> readFile "10.txt"
    let attrs = map (`checkLine` []) input
    let score = foldr (\l acc -> case l of 
                        InValid x -> x + acc
                        a -> acc) 0 attrs
    captionPrint "A: " score
    let incompletes = filter (\case
                              Incomplete a -> True 
                              b -> False) attrs
    let autoScores = map (calculateAutoComplete . (\(Incomplete x) -> x )) incompletes
    let m = mid autoScores
    captionPrint "B: " m


checkLine :: String -> String -> LineAttr Int String
checkLine [] [] = Valid
checkLine [] h = Incomplete h
checkLine (l:ls) queue
    | l `elem` opening = checkLine ls (let (Just c) = Map.lookup l respective in c:queue)
    | l == head queue = checkLine ls (tail queue)
    | otherwise = let (Just score) = Map.lookup l scores in InValid score

calculateAutoComplete :: String -> Int
calculateAutoComplete = foldl (\acc x -> let (Just s) = Map.lookup x autocompleteScores in acc * 5 + s) 0

opening :: String
opening = ['(', '[', '{', '<']
closing :: String
closing = [')', ']', '}', '>']

scores :: Map Char Int
scores = Map.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

autocompleteScores :: Map Char Int
autocompleteScores = Map.fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)]

respective :: Map Char Char
respective = Map.fromList (zip opening closing)
