{-import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

import Data.Set (Set)
import qualified Data.Set as Set

primesUpto :: Int -> [Int]
primesUpto n = [p | (p, True) <- assocs $ sieve n]

sieve :: Int -> UArray Int Bool
sieve n = runSTUArray $ do
    sieve <- newArray (2, n) True
    forM_ [2..n] $ \p -> do
        isPrime <- readArray sieve p
        when isPrime $ do
            forM_ [p*2, p*3 .. n] $ \k -> do
                writeArray sieve k False
    return sieve

-}
{-
>>> :t f = 
parse error (possibly incorrect indentation or mismatched brackets)

maxMONAD :: [(F, FuncType)] -> Maybe [Int]
maxMONAD fs = maxMONAD' fs [9,8..1] (0, 0, 0, 0)
    where
        maxMONAD' [] _ _ = Nothing
        maxMONAD' _ [] _ = Nothing
        maxMONAD' [(f, b)] (v:vals) inp = 
            let inp' = putVal 'w' v inp
                out = f inp'
                z = getVal 'z' out
            in if z == 0 then Just [v] else maxMONAD' [(f, b)] vals inp
        maxMONAD' ((f, b):fs) (v:vals) inp = let inp' = putVal 'w' v inp in 
            case maxMONAD' fs [9,8..1] inp' of
                Nothing -> maxMONAD' ((f, b):fs) vals inp
                Just xs -> Just (v:xs)


parseTree :: [[Ops]] -> [(F, FuncType)]
parseTree xs = parseTree' xs (False, True, True, True)
    where
        parseTree' [] _ = []
        parseTree' (x:xs) b = let (f', b') = foldl parseFunc (id, b) (tail x) in (f', b'): parseTree' xs b'

parseFunc :: (F, FuncType) -> Ops -> (F, FuncType)
parseFunc (f, b) (op, tar, var)
    | op == "mul" && fromLeft 1 var == 0 = (applyFunc (tar, 'w') (const (const 0)) f, putVal tar True b)
    | otherwise = case var of
        Left i -> (applyFunc (tar, 'w') (applyConst i oper) f, b)
        Right i -> (applyFunc (tar, i) oper f, if not (getVal i b) then putVal tar False b else b)
    where
        oper = getOp op

applyConst :: Int -> (Int -> Int -> Int) -> (Int -> Int -> Int)
applyConst v f a _ = a `f` v

applyFunc :: (Char, Char) -> (Int -> Int -> Int) -> F -> Quad -> Quad
applyFunc (i, o) oper f a = let a' = f a
                                iVal = getVal i a'
                                oVal = getVal o a'
                                out = iVal `oper` oVal
                            in putVal i out a'

putVal :: Char -> a -> (a, a, a, a) -> (a, a, a, a)
putVal c v (w, x, y, z)
    | c == 'w' = (v, x, y, z)
    | c == 'x' = (w, v, y, z)
    | c == 'y' = (w, x, v, z)
    | otherwise = (w, x, y, v)

getVal :: Char -> (a, a, a, a) -> a
getVal c (w, x, y, z)
    | c == 'w' = w
    | c == 'x' = x
    | c == 'y' = y
    | otherwise = z

getOp :: String -> (Int -> Int -> Int)
getOp s = case s of
    "add" -> (+)
    "mul" -> (*)
    "div" -> div
    "mod" -> mod
    other -> \a b -> if a == b then 1 else 0
-}
