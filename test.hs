import Utils
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
{-
>>> (-1) 2
Non type-variable argument in the constraint: Num (t1 -> t2)
(Use FlexibleContexts to permit this)
-}

getCoord :: Integral a => (a, a) -> a -> [a]
getCoord (x, y) a = map (\(a, b) -> x * b + a) $ filter (\(a, b) -> 0 <= a && a < x && 0 <= b && b < y) ps
    where
        (ax, ay) = let yVal = a `div` x in (a - yVal * x, yVal)
        ps = [(ax + i, ay + j) | i <- [-1..1], j <- [-1..1], i /= 0 || j /= 0]

{-
>>> getCoord (5, 5) 8
[2,7,12,3,13,4,9,14]

-}

