import Utils
type Mask = [Bool]
type Mask2D = [Mask]
type Board = [[Int]]

main :: IO ()
main = do
    ln <- fmap lines (readFile "4.txt")
    let nums = map read $ split "," (head ln) :: [Int]
    let boards = map (map (map read . split " ")) $ split [""] (tail ln) :: [[[Int]]]
    let (brdInd, msk, n) = winner nums boards
    let computeMatch xs ys = map fst $ filter (\(a, b) -> not b) (zip xs ys)
    let unmarked = computeMatch (flatten (boards !! brdInd)) (flatten (msk !! brdInd))
    captionPrint "A: " $ sum unmarked * n
    let (loss, msk', n') = loser nums boards
    let unmarked' = computeMatch (flatten loss) (flatten msk')
    captionPrint "B: " $ sum unmarked' * n'

loser :: [Int] -> [Board] -> (Board, Mask2D, Int)
loser xs brd
    | length brd == 1 = (head brd, head msk, n)
    | otherwise = loser xs brd'
    where
        (ind, msk, n) = winner xs brd
        brd' = removeIth ind brd


winner :: [Int] -> [Board] -> (Int, [Mask2D], Int)
winner xs bs = let
    bDim = length (head (head bs))
    nBoards = length bs
    mask = replicate nBoards (replicate bDim (replicate bDim False))
    in winner' mask xs bs
        where
            winner' ms [] bs = (-1, [], -1)
            winner' ms (x:xs) bs = case winInd of
                Nothing -> winner' ms' xs bs
                Just a  -> (a, ms', x)
                where
                    ms' = zipWith (makeMask2D x) bs ms
                    winInd = find True $ zipWith (\ b m -> isWin m) bs ms'

isWin :: Mask2D -> Bool
isWin mask = check mask || check colOrder
    where
        check = any (all (==True))
        colOrder = transpose mask

makeMask :: Int -> [Int] -> Mask -> Mask
makeMask _ [] _ = []
makeMask _ _ [] = []
makeMask v (x:xs) (m:ms)
    | v == x = True:makeMask v xs ms
    | otherwise = m:makeMask v xs ms

makeMask2D :: Int -> Board -> Mask2D -> Mask2D
makeMask2D _ [] _ = []
makeMask2D _ _ [] = []
makeMask2D v (x:xs) (m:ms) = makeMask v x m : makeMask2D v xs ms
