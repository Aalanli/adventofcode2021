import Utils
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort, product, sortOn)
import qualified Data.Ord

main = do
    input <- map (map read . unsqueeze) . lines <$> readFile "9.txt" :: IO [[Int]]
    let boolMap = lowest2D input
    let nums = fetch input boolMap
    let sums = sum $ map (+1) nums
    captionPrint "A: " sums
    let coords = extractCoord boolMap
    let basinSizes = map (length . growBasin input) coords
    let mul = product $ take 3 (sortOn Data.Ord.Down basinSizes)
    captionPrint "B: " mul


growBasin :: (Eq a, Num a) => [[a]] -> (Int, Int) -> Set (Int, Int)
growBasin xs ps = growBasin' (initialX ++ initialY) $ Set.fromList [ps]
    where
        ys = transpose xs
        growX (x, y) = [(x1, y) | x1 <- growByAxis x (xs !! y)]
        growY (x, y) = [(x, y1) | y1 <- growByAxis y (ys !! x)]
        initialX = growX ps
        initialY = growY ps
        growBasin' [] grown = grown
        growBasin' (g:grow) grown = growBasin' (yGrowth ++ xGrowth ++ grow) $ Set.insert g grown
            where xGrowth = filterBySet (growY g) grown
                  yGrowth = filterBySet (growX g) grown

growByAxis :: (Eq a, Num a) => Int -> [a] -> [Int]
growByAxis p gx = leftSide ++ rightSide
    where
        leftSide = map (p-) $ grow1D (reverse (take p gx)) 1
        rightSide = grow1D (drop (p+1) gx) (p+1)
        grow1D [] _ = []
        grow1D (x:xs) n
            | x /= 9 = n : grow1D xs (n+1)
            | otherwise = []

filterBySet :: Ord a => [a] -> Set a -> [a]
filterBySet [] _ = []
filterBySet (x:xs) s
    | Set.member x s = filterBySet xs s
    | otherwise = x:filterBySet xs s

extractCoord :: [[Bool]] -> [(Int, Int)]
extractCoord xxs = extractCoord' xxs 0
    where
        extractCoord' [] _ = []
        extractCoord' (xs:xxs) y = case extractCoord1D xs 0 of
            [] -> extractCoord' xxs (y+1)
            c  -> [(a, y) | a <- c] ++ extractCoord' xxs (y+1)

extractCoord1D :: Num a => [Bool] -> a -> [a]
extractCoord1D [] _ = []
extractCoord1D (x:xs) n
    | x = n:extractCoord1D xs (n+1)
    | otherwise = extractCoord1D xs (n+1)

fetch :: [[b]] -> [[Bool]] -> [b]
fetch xs ys = map fst $ filter snd (zip (flatten xs) (flatten ys))

lowest2D :: Ord a => [[a]] -> [[Bool]]
lowest2D xs = [elemAnd x y | (x, y) <- zip a b]
    where a = map lowest1D xs
          b = transpose $ map lowest1D (transpose xs)

lowest1D :: Ord a => [a] -> [Bool]
lowest1D [] = []
lowest1D [a] = []
lowest1D (x:y:xs) = (x < y) : lowest1D' (x:y:xs)
    where
        lowest1D' [] = []
        lowest1D' [a] = []
        lowest1D' [a, b] = []
        lowest1D' [a,b,c] = [b < a && b < c, c < b]
        lowest1D' (a:b:c:xs) = (b < a && b < c):lowest1D' (b:c:xs)

elemAnd :: [Bool] -> [Bool] -> [Bool]
elemAnd xs ys = [x && y | (x, y) <- zip xs ys]


