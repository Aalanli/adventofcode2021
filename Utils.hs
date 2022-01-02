module Utils where
import Data.List (sort)

import Data.Set (Set)
import qualified Data.Set as Set

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = Set.toList . Set.fromList

removeIth :: Int -> [a] -> [a]
removeIth i xs = take i xs ++ drop (1 + i) xs

replace :: (a -> a) -> [a] -> [a]
replace _ [] = []
replace f (x:xs) = f x:replace f xs

find :: (Eq a) => a -> [a] -> Maybe Int
find = find' 0
    where
        find' _ _ [] = Nothing
        find' n v (x:xs)
            | v == x = Just n
            | otherwise = find' (n + 1) v xs

takeMultiple :: Int -> Int -> [a] -> [a]
takeMultiple s m = takeMultiple' 0 s
    where
        takeMultiple' _ _ [] = []
        takeMultiple' n p (x:xs)
            | n == p = x:takeMultiple' (n + 1) (p + m) xs
            | otherwise = takeMultiple' (n + 1) p xs

slice :: Int -> Int -> [a] -> [a]
slice st sp xs = take (sp - st) (drop st xs)

flatten :: [[a]] -> [a]
flatten [] = []
flatten [a] = a
flatten (x:xs) = x ++ flatten xs

unFlatten :: Int -> [a] -> [[a]]
unFlatten _ [] = []
unFlatten n xs = take n xs:unFlatten n (drop n xs)

split :: (Eq a) => [a] -> [a] -> [[a]]
split v [] = []
split v xs
    | not (null sHead) = sHead:split v sTail
    | not (null sTail) = split v sTail
    | otherwise = [xs]
    where
        len = length v
        (sHead, sTail') = cutAt (\x -> take len x == v) xs
        sTail = drop len sTail'

cutAt :: (Eq a) => ([a] -> Bool) -> [a] -> ([a], [a])
cutAt f xs = (f' [], fx)
    where
        (f', fx) = cutAt' id xs
        cutAt' _ [] = (id, [])
        cutAt' g as
            | f as = (g, as)
            | otherwise = cutAt' (g . (head as:)) (tail as)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose xs
    | null (head xs) = []
    | otherwise = [head x | x <- xs]:transpose [tail x | x <- xs]

unsqueeze :: [a] -> [[a]]
unsqueeze = map (: [])

listRead :: String -> IO [Int]
listRead file = do
    contents  <- readFile file
    return $ map read (lines contents) :: IO [Int]

captionPrint :: (Show a) => String -> a -> IO ()
captionPrint t v = putStr $ t ++ show v ++ "\n"

convertPair :: [String] -> [(String, Int)]
convertPair [] = []
convertPair (x:xs) = (head strPair, read (last strPair)):convertPair xs
    where strPair = words x

count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count f (x:xs)
    | f x = 1 + count f xs
    | otherwise = count f xs

mid :: Ord a => [a] -> a
mid xs = xs' !! ls
    where 
        xs' = sort xs
        ls = length xs `div` 2

printByCol :: Show a => [a] -> IO ()
printByCol [] = return ()
printByCol (x:xs) = do
    print x
    printByCol xs


interweave :: a -> [a] -> [[a]]
interweave = interweave' id
    where 
        interweave' f a [] = [f [a]]
        interweave' f a (x:xs) = f (a:x:xs) : interweave' (f . (x:)) a xs 

joinSet :: Ord a => Set a -> [a] -> Set a
joinSet = foldr Set.insert