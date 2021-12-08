import Utils

main :: IO ()
main = do
    val <- map read . split "," <$> readFile "7.txt" :: IO [Int]
    let c = minimum [l1Reduce x val | x <- [minimum val..maximum val]]
    captionPrint "A: " c
    let c' = minimum [l2Reduce x val | x <- [minimum val..maximum val]]
    captionPrint "B:" c'

l1Reduce :: (Foldable t, Num a) => a -> t a -> a
l1Reduce c = foldr (\x acc -> acc + abs (x - c)) 0

l2Reduce :: (Foldable t, Integral a) => a -> t a -> a
l2Reduce c = foldr (\x acc -> let dist = abs (x-c) in acc + (dist^2 + dist) `div` 2) 0