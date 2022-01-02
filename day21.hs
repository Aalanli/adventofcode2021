{-# LANGUAGE BangPatterns #-}
import Data.List (foldl')
{-
>>> playUpTo 1000 4 10
855624

>>> dirac2
[((3,3),1),((3,4),3),((3,5),6),((3,6),7),((3,7),6),((3,8),3),((3,9),1),((4,3),3),((4,4),9),((4,5),18),((4,6),21),((4,7),18),((4,8),9),((4,9),3),((5,3),6),((5,4),18),((5,5),36),((5,6),42),((5,7),36),((5,8),18),((5,9),6),((6,3),7),((6,4),21),((6,5),42),((6,6),49),((6,7),42),((6,8),21),((6,9),7),((7,3),6),((7,4),18),((7,5),36),((7,6),42),((7,7),36),((7,8),18),((7,9),6),((8,3),3),((8,4),9),((8,5),18),((8,6),21),((8,7),18),((8,8),9),((8,9),3),((9,3),1),((9,4),3),((9,5),6),((9,6),7),((9,7),6),((9,8),3),((9,9),1)]

>>> newScore 1 10
1
-}


main = do
    let (a,b) = simulate 4 8 0 0
    print (a,b)

type N = Int

simulate :: N -> N -> N -> N -> (N, N)  -- wrong logic and stackoverflow, recursion 
simulate !p1 !p2 !s1 !s2
    | s1 >= 20 = (1, 0)
    | s2 >= 20 = (0, 1)
    | otherwise = foldl' (\(u1, u2) ((d1, d2), n) -> 
        let p1' = newScore p1 d1
            p2' = newScore p2 d2
            (c1, c2) = simulate p1' p2' (p1' + s1) (p2' + s2)
        in (n * c1 + u1, n * c2 + u2)) (0,0) dirac2

dirac1 :: [(N, N)]
dirac1 = [(3,1), (4,3), (5,6), (6,7), (7,6), (8,3), (9,1)]
dirac2 :: [((N, N), N)]
dirac2 = [((a,b),i*j) | (a,i) <- dirac1, (b,j) <- dirac1]

playUpTo n p1 p2 = playUpTo' n p1 p2 0 0 1 0
    where
        playUpTo' n p1 p2 s1 s2 d r
            | s1' >= n = s2 * (r + 3)
            | s2' >= n = s1' * (r + 6)
            | otherwise = playUpTo' n p1' p2' s1' s2' d' (r+6)
            where
                (p1', p2', d') = playOnce p1 p2 d
                s1' = p1' + s1
                s2' = p2' + s2

playOnce :: Integral b => b -> b -> b -> (b, b, b)
playOnce p1 p2 d = (p1', p2', d'')
    where 
        (sm1, d') = dieRoll d
        p1' = newScore p1 sm1
        (sm2, d'') = dieRoll d'
        p2' = newScore p2 sm2

newScore :: Integral a => a -> a -> a
newScore curPos sumScore = (curPos + sumScore - 1) `mod` 10 + 1

dieRoll :: Integral b => b -> (b, b) -- (sum of rolls, new pos)
dieRoll n
    | n < 98 = (3 * n + 3, n + 3)
    | n == 98 = (297, 1)
    | n == 99 = (200, 2)
    | otherwise = (103, 3)
