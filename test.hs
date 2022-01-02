import Control.Monad
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


{-
>>> a = Set.fromList [(4, 'a'), (6, 'b')]
>>> Set.lookupGT 3 a
Non type-variable argument in the constraint: Num (a, Char)
(Use FlexibleContexts to permit this)

>>> (3, '1') > (2, '3')
True
-}
