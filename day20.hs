import Utils
import Data.List (foldl')

import qualified Control.Monad as M
import qualified Control.Monad.ST as ST
import qualified Data.Array.ST as STA
import Data.Array.Unboxed (UArray, (!))
import qualified Data.Array.Unboxed as Array

import Data.Map (Map)
import qualified Data.Map as Map


main :: IO ()
main = do
    (rep, img) <- (\x -> (head x, drop 2 x)) . lines <$> readFile "20.txt"
    let imgArr = let (x, y) = (length (head img), length img) in Array.listArray ((0,0),(x-1,y-1)) $ concat img :: UArray (Int, Int) Char
    let repArr = let n = length rep in Array.listArray (0,n-1) rep :: UArray Int Char
    let conv2 = enhanceN 2 repArr imgArr
    let elems = foldr (\x acc -> if x == '#' then 1 + acc else acc) 0 $ Array.elems conv2
    print elems
    let conv50 = enhanceN 50 repArr imgArr
    let elems' = foldr (\x acc -> if x == '#' then 1 + acc else acc) 0 $ Array.elems conv50
    print elems'


enhanceN :: Int -> UArray Int Char -> UArray (Int, Int) Char -> UArray (Int, Int) Char
enhanceN n rep img = enhanceN' n curChar rep img
    where
        curChar = if rep ! 0 == '.' then '.' else '#'
        outChar = if rep ! 0 == '.' then const '.' else (\x -> if x == '#' then '.' else '#')
        enhanceN' 0 _ _ img = img
        enhanceN' n c rep img = enhanceN' (n-1) c' rep (conv3 c' rep img)
            where c' = outChar c

extractList :: UArray (Int, Int) Char -> [String]
extractList mp = let list = Array.elems mp in unFlatten (x+1) list
    where ((0,0),(x,y)) = Array.bounds mp

conv3 :: Char -> UArray Int Char -> UArray (Int, Int) Char -> UArray (Int, Int) Char
conv3 b rep img = STA.runSTUArray $ do
    img' <- STA.newArray ((0,0),(x+2,y+2)) '.'
    M.forM_ [-1..x+1] $ \i ->
        M.forM_ [-1..y+1] $ \j ->
            let
                coords = getCoord (i,j)
                loc = map (\a -> if isValidCoord (x,y) a then img ! a else b) coords
                dec = rep ! toDec loc
            in STA.writeArray img' (i+1,j+1) dec
    return img'
    where
        (_, (x,y)) = Array.bounds img

isValidCoord (x,y) (a,b) = a >= 0 && a <= x && b >= 0 && b <= y

getCoord :: (Num a, Enum a) => (a, a) -> [(a, a)]
getCoord (x,y) = [(x+i,y+j) | i <- [-1..1], j <- [-1..1]]

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

digitToInt :: Num p => Char -> p
digitToInt a = if a == '.' then 0 else 1
