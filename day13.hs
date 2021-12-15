import Utils

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed ( UArray, elems, bounds)

data Fold = Y Int | X Int deriving (Show, Eq)
type Point = (Int, Int)
type Points = Set Point

main = do
    a <- lines <$> readFile "13.txt"
    let (points, folds) = let [p, f] = split [""] a
                              p' = Set.fromList $ map ((\ [x, y] -> (read x, read y)) . split ",") p :: Points
                              f' = map readFolds f
                          in (p', f')
    let fstFold = foldOnce (head folds) points
    captionPrint "A: " $ Set.size fstFold
    let allFolds = foldl (flip foldOnce) points folds
    let matrix = writePoints $ Set.toList allFolds
    let ((_, _), (x, y)) = bounds matrix
    let matrix' = unFlatten (y+1) $ elems matrix
    printByCol matrix'


writePoints :: [Point] -> UArray (Int, Int) Char
writePoints ps = runSTUArray $ do
    matrix <- newArray ((0, 0), (my, mx)) '.'
    forM_ ps $ \(x, y) -> writeArray matrix (y, x) '#'
    return matrix
    where (mx, my) = (maximum (map fst ps), maximum (map snd ps))


foldOnce :: Fold -> Points -> Points
foldOnce (Y n) = Set.map (\(a, b) -> if b > n then (a, 2 * n - b) else (a, b))
foldOnce (X n) = Set.map (\(a, b) -> if a > n then (2 * n - a, b) else (a, b))


readFolds :: String -> Fold
readFolds xs = let w = last $ words xs
                   n = read $ drop 2 w
                in if head w == 'x' then X n else Y n