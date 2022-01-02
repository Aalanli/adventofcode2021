
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (transpose)

import Data.Maybe (isNothing, fromJust)

type Hallway = Set (Int, Char)
type Room = (String, Int)
type Rooms = Map Int String


rooms :: [String]
rooms = ["CDDB", "DCBA", "DBAB", "AACC"]

main :: IO ()
main = do
    --input <- lines <$> readFile "23.txt"
    --let hall = makeHall $ drop 1 $ take 12 $ input !! 1
    --let rooms = let temp = transpose $ drop 2 input in makeRoom [filter (/= '.') $ take roomDepth $ temp !! i | i <- roomPos]
    let minScore = solve Set.empty rooms'
    print minScore

roomDepth = 4


solve :: Hallway -> Rooms -> Maybe Int
solve = solve'
    where
        solve' h rss
            | Map.null rs = Just 0
            | otherwise = case moveHall h rs of
                Just (h', rs', c') -> getNewCost h' rs' c'
                Nothing -> case presentMoves of
                    [] -> Nothing
                    ms -> lowerScore presentMoves
            where
                -- removes rooms that are complete
                rs = Map.differenceWith (\al ar -> if al == ar then Nothing else Just al) rss completeRooms

                presentMoves = proposeMoves h rs
                lowerScore = foldr (\x acc -> let (h', rs', c') = moveRoom x h rs
                                              in case solve' h' rs' of
                                                 Nothing -> acc
                                                 Just v -> case acc of
                                                     Nothing -> Just (v + c')
                                                     Just v' -> Just (min v' (v + c'))) Nothing
                getNewCost h' rs' c' = do
                    v <- solve' h' rs'
                    return (c' + v)


proposeMoves :: Set (Int, Char) -> Map Int String -> [(Int, Int, Char)]
proposeMoves h rm = concatMap (\(k, a, l, u) -> [(k, i, a) | i <- filter (`notElem` roomPos) [l+1..u-1]]) roomBounds
    where
        availableRooms = getAvailableRooms rm
        roomBounds = map getLowerUpper availableRooms
        getLowerUpper (k, a) = let l = Set.lookupLT (k, a) h
                                   u = Set.lookupGT (k, a) h
                                   lower = maybe 0 fst l
                                   upper = maybe 12 fst u
                               in (k, a, lower, upper)
        locked (_, a, l, u) = let (Just dest) = Map.lookup a targetPos
                               in dest < l || dest > u

getAvailableRooms :: Map Int String -> [(Int, Char)]
getAvailableRooms = Map.foldrWithKey (\k a acc -> if possibleRoom k a then (k, head a):acc else acc) []

possibleRoom :: Int -> String -> Bool
possibleRoom i s
    | l == roomDepth = s /= s'
    | l > 0 = s /= take l s'
    | otherwise = False
    where (Just s') = Map.lookup i completeRooms
          l = length s

moveRoom :: (Int, Int, Char) -> Hallway -> Rooms -> (Hallway, Rooms, Int)
moveRoom (b, e, a) h rs = (h', rs', c)
    where
        (Just xs) = Map.lookup b rs
        rs' = Map.update (\(x:xs) -> Just xs) b rs
        h' = Set.insert (e, a) h
        c = let (Just cost) = Map.lookup a costMove in (abs (e-b) + roomDepth + 1 - length xs) * cost

moveHall :: Hallway -> Rooms -> Maybe (Hallway, Rooms, Int)
moveHall h rs
    | null openRoom = Nothing
    | otherwise = Just (hallway', rooms', costs)
    where
        notObstructed b e c
            | e > b = case Set.lookupGT (b, c) h of
                        Nothing -> True
                        Just (i, v) -> i > e
            | otherwise = case Set.lookupLT (b, c) h of
                        Nothing -> True
                        Just (i, v) -> i < e
        availableRoom b e c room
            | l == roomDepth = False
            | l > 0 = all (==c) room
            | otherwise = True
            where l = length room
        (openRoom, old) = Set.foldr (\(b, c) (n, o) -> let (Just e) = Map.lookup c targetPos
                                                           (Just rooms) = Map.lookup e rs
                                                       in if notObstructed b e c && availableRoom b e c rooms
                                                          then let (Just cost) = Map.lookup c costMove
                                                                   tCosts = (abs(b-e) + roomDepth - length rooms) * cost
                                                               in ((e, c, tCosts, cost):n, o)
                                                          else (n, (b, c):o)) ([], []) h
        costs = fst $ foldr (\(_, c, tCost, origCost) (acc, vis) -> (tCost + acc - count (== c) vis * origCost, c:vis)) (0, []) openRoom
        hallway' = Set.fromAscList old
        rooms' = foldr (\(e, c, _, _) acc -> Map.adjust (c:) e acc) rs openRoom

count :: (Foldable t1, Num a) => (t2 -> Bool) -> t1 t2 -> a
count f = foldr (\x acc -> if f x then 1 + acc else acc) 0

lookupHall :: Ord k => k -> Map k a -> Bool
lookupHall pos h = case Map.lookup pos h of -- hallway is filled if key exists, not filled if it doesn't
                   Nothing -> True
                   Just a -> False

-- Viewing Convenience --
makeHall :: String -> Hallway
makeHall xs = foldr (\(i, x) acc -> if x == '.' then acc else Set.insert (i, x) acc) Set.empty $ zip [1..] xs

makeRoom :: [String] -> Rooms
makeRoom xs = Map.fromList $ zip roomPos xs

viewHall :: Hallway -> String
viewHall hs = unfold 1 $ Set.toList hs
    where
        unfold n [] = replicate (12 - n) '.'
        unfold n ((i, c):xs)
            | n == i = c : unfold n xs
            | otherwise = '.' : unfold n ((i, c):xs)


-- Constants --
rooms' :: Rooms
rooms' = makeRoom rooms

completeRooms :: Map Int String
completeRooms = Map.fromList $ zip roomPos [replicate roomDepth a | a <- ['A', 'B', 'C', 'D']]

roomPos :: [Int]
roomPos = [3,5..9]

targetPos :: Map Char Int
targetPos = Map.fromList $ zip "ABCD" roomPos

costMove :: Map Char Int
costMove = Map.fromList [('A', 1), ('B', 10), ('C', 100), ('D', 1000)]
