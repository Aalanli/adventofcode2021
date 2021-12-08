{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Utils

main = do
    nums <- map read . split "," <$> readFile "6.txt" :: IO [Int]
    let one = simulateN 80 simulateOne nums
    captionPrint "A: " $ length one
    let counts = [count (==i) nums | i <- [0..8]]
    let finalCount = simulateN 256 efficientSimulate counts
    captionPrint "B: " $ sum finalCount

simulateN :: Int -> (a -> a) -> a -> a
simulateN 0 f xs = xs
simulateN n f xs = simulateN (n-1) f $ f xs

simulateOne :: [Int] -> [Int]
simulateOne [] = []
simulateOne (x:xs)
    | x == 0 = 8:6:simulateOne xs
    | otherwise = (x-1):simulateOne xs

efficientSimulate :: [Int] -> [Int]
efficientSimulate [a,b,c,d,e,f,g,h,i] = [b,c,d,e,f,g,h+a,i,a]