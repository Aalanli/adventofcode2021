import Utils
import Data.Map (Map)
import qualified Data.Map as Map
{-
>>> (map read . split " ") <$> Just "1 2 3 4" :: Maybe [Int]
Just [1,2,3,4]

-}
