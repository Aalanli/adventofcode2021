import Utils
import Data.Map (Map)
import qualified Data.Map as Map
{-
>>> take 3 [9..9]
[9]

-}

a = Map.empty :: Map (Int, Int) Int
b = Map.insert (1, 0) 1 a
c = Map.adjust (+1) (1, 0) b
