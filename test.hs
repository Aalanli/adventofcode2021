import Utils
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
{-
>>> growByAxis 1 [2,1,9,9,9,4,3,2,1,0]
[0]


>>> drop 2 [1, 2, 3]
[3]
-}
growByAxis p gx = leftSide ++ rightSide
    where 
        leftSide = map (p-) $ grow1D (reverse (take p gx)) 1
        rightSide = grow1D (drop (p+1) gx) (p+1)
        grow1D [] _ = []
        grow1D (x:xs) n
            | x /= 9 = n : grow1D xs (n+1)
            | otherwise = []
