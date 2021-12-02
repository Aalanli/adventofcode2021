import Utils

main = do
    contents  <- readFile "1.txt"
    let intContents = map read $ lines contents :: [Int]
    let num = count' intContents
    captionPrint "Part A: " num
    let numb = countWindow intContents
    captionPrint "Part B: " numb

count' :: [Int] -> Int 
count' [] = 0
count' [_] = 0
count' (x:y:xs)
    | y > x = 1 + count' (y:xs)
    | otherwise = 0 + count' (y:xs)

countWindow :: [Int] -> Int 
countWindow [] = 0
countWindow [a] = 0
countWindow [a, b] = 0
countWindow [a, b, c] = 0
countWindow (a:b:c:d:xs)
    | d > a = 1 + countWindow (b:c:d:xs)
    | otherwise = 0 + countWindow (b:c:d:xs)
