import Utils

import Data.List (permutations, sort)
import Data.Set (Set)
import qualified Data.Set as Set

type Point = (Int, Int, Int)


main :: IO ()
main = do
    input <- map (Set.fromList . map ((\[a, b, c] -> (a, b, c)) . map read . split ",") . tail) . split [""] . lines <$> readFile "19.txt" :: IO [Set Point]
    let Just (points, ps) = resolvePositions input
    print $ Set.size points
    let manhattan = maximum [l1 a b | a <- ps, b <- ps]
    print manhattan


l1 :: Num a => (a, a, a) -> (a, a, a) -> a
l1 (x1,y1,z1) (x2,y2,z2) = abs (x1-x2) + abs (y1-y2) + abs (z1-z2)

resolvePositions :: [Set Point] -> Maybe (Set Point, [Point])
resolvePositions [] = Nothing
resolvePositions (x:xs) = case resolvePos x xs [] of
    (Nothing, _) -> resolvePositions xs
    (Just (ps, pos), left) -> resolvePositions' (ps, [pos]) left
    where
        resolvePositions' a [] = Just a
        resolvePositions' (ps, pos) xs = case resolvePos ps xs [] of
            (Nothing, _) -> Just (ps, pos)
            (Just (ps', pos'), left) -> resolvePositions' (ps', pos':pos) left
        resolvePos a [] left = (Nothing, left)
        resolvePos a (x:xs) left = case solveRotations a x of
            Nothing -> resolvePos a xs (x:left)
            Just a -> (Just a, left ++ xs)

solveRotations :: Set Point -> Set Point -> Maybe (Set Point, Point)
solveRotations p1 p2 = solveRotations' allRotations
    where
        solveRotations' [] = Nothing
        solveRotations' (f:fs) = case linearIntersection p1 (Set.map f p2) of
            Nothing -> solveRotations' fs
            Just a -> Just a

linearIntersection :: Set Point -> Set Point -> Maybe (Set Point, Point) -- probe offset
linearIntersection p1 p2 = linearIntersection' (Set.toList p2)
    where
        xs = Set.toList p1
        linearIntersection' [] = Nothing
        linearIntersection' (y:ys)
            | null dm = linearIntersection' ys
            | otherwise = let (p2', diff) = head dm in Just (Set.union p2' p1, diff)
            where
                df = map (`diffPoint` y) xs
                dm = dropWhile (\(dm', _) -> Set.size (Set.intersection p1 dm') < 12) $ map (\diff -> (Set.map (sumPoint diff) p2, diff)) df

pairWiseDistance :: [Point] -> [Int]
pairWiseDistance [] = []
pairWiseDistance [p] = []
pairWiseDistance (p1:p2:ps) = let (x,y,z) = diffPoint p1 p2 in x^2+y^2+z^2:pairWiseDistance (p2:ps)

sumPoint :: (Num a, Num b, Num c) => (a, b, c) -> (a, b, c) -> (a, b, c)
sumPoint (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)
diffPoint :: (Num a, Num b, Num c) => (a, b, c) -> (a, b, c) -> (a, b, c)
diffPoint (x1,y1,z1) (x2,y2,z2) = (x1-x2,y1-y2,z1-z2)

allRotations :: [Point -> Point]
allRotations = concatMap (\x -> map (.x) basicRotations) otherRotations

otherRotations :: [Point -> Point] -- up direction
otherRotations = [
    \(x, y, z) -> (x, y, z),
    \(x, y, z) -> (-z, y, x),
    \(x, y, z) -> (-x, y, -z),
    \(x, y, z) -> (z, y, -x),
    \(x, y, z) -> (z, x, y),
    \(x, y, z) -> (z, -x, -y)
    ]

basicRotations :: [Point -> Point] -- rotate around facing direction
basicRotations = [
    \(x, y, z) -> (x, y, z),
    \(x, y, z) -> (y, -x, z),
    \(x, y, z) -> (-x, -y, z),
    \(x, y, z) -> (-y, x, z)
    ]
