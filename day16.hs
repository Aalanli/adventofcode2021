import Utils

import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map


data Packet = Operator {version :: Int, typeId :: Int, lengthId :: Int, valSubPacket :: Int, subPackets :: [Packet]}
            | Literal {version :: Int, value :: Int} deriving (Show)


main :: IO ()
main = do
    input <- readFile "16.txt"
    let bin = toBin input
    let (Just packet) = decodePackets bin
    let sum = sumVersion packet
    print sum
    let reduced = reduceVal packet
    print reduced


reduceVal :: Packet -> Int
reduceVal Literal {value=v} = v
reduceVal Operator {typeId=v, subPackets=ps}
    | v == 0 = foldr (\x acc -> reduceVal x + acc) 0 ps
    | v == 1 = foldr (\x acc -> reduceVal x * acc) 1 ps
    | v == 2 = foldr (min . reduceVal) (reduceVal (head ps)) (tail ps)
    | v == 3 = foldr (max . reduceVal) 0 ps
    | v == 5 = if reduceVal (head ps) < reduceVal (last ps) then 1 else 0
    | v == 6 = if reduceVal (head ps) > reduceVal (last ps) then 1 else 0
    | v == 7 = if reduceVal (head ps) == reduceVal (last ps) then 1 else 0
    | otherwise = -1

sumVersion :: Packet -> Int
sumVersion Operator {version=v, subPackets=xs} = v + foldr (\x acc -> acc + sumVersion x) 0 xs
sumVersion Literal {version=v} = v

checkValid :: Packet -> Bool
checkValid Literal {} = True
checkValid Operator {lengthId=l, valSubPacket=v, subPackets=ps}
    | l == 1 = length ps == v && all ((==True) . checkValid) ps
    | otherwise = True

decodePackets :: String -> Maybe Packet
decodePackets xs = fst <$> decodePackets' xs
    where
        decodePackets' xs
            | length xs < 11 = Nothing
            | id == 4 = let (body, edge) = getLiteral xs' in Just (Literal ver (toDec body), edge)
            | otherwise = Just (Operator ver id lenId val subpackets, edge)
            where
                (ver, id, xs') = getIdOp xs
                lenId = digitToInt $ head xs'
                (subpackets, edge, val) = if lenId == 1
                    then let val = toDec (slice 1 12 xs')
                             (sp, nEdge) = accumDoN val [] $ drop 12 xs'
                         in (sp, nEdge, val)
                    else let val = toDec (slice 1 16 xs')
                             (sp, _) = accumDoWhile [] (slice 16 (16+val) xs')
                         in (sp, drop (16+val) xs', val)

        accumDoWhile subP edge = case decodePackets' edge of
            Nothing -> (subP, edge)
            Just (packet', edge') -> accumDoWhile (packet':subP) edge'
        
        accumDoN n subP edge
            | n == 0 = (subP, edge)
            | otherwise = let Just (packet', edge') = decodePackets' edge in accumDoN (n-1) (packet':subP) edge'

getLiteral :: String -> (String, String)
getLiteral = getLiteral' []
    where
        getLiteral' ts xs
            | length xs < 5 = (ts, xs)
            | head xs == '1' = getLiteral' (ts ++ slice 1 5 xs) (drop 5 xs)
            | otherwise = (ts ++ slice 1 5 xs, drop 5 xs)

getIdOp :: String -> (Int, Int, String)
getIdOp xs = let id = toDec (take 3 xs)
                 op = toDec (slice 3 6 xs)
             in (id, op, drop 6 xs)

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

toBin :: String -> String
toBin = concatMap (\x -> let (Just v) = Map.lookup x hex in v)

hex :: Map Char String
hex = Map.fromList [
    ('0', "0000"),
    ('1', "0001"),
    ('2', "0010"),
    ('3', "0011"),
    ('4', "0100"),
    ('5', "0101"),
    ('6', "0110"),
    ('7', "0111"),
    ('8', "1000"),
    ('9', "1001"),
    ('A', "1010"),
    ('B', "1011"),
    ('C', "1100"),
    ('D', "1101"),
    ('E', "1110"),
    ('F', "1111")]
