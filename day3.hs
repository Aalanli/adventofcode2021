import Utils

{-
>>> closestMatch "10110" ["10110", "10111", "10101", "01111"]
"10110"
-}

main = do
    raw <- readFile "3.txt"
    let contents = lines raw
    let transposed = transpose contents
    let counting x = (count '0' x, count '1' x)
    let counts = map counting transposed
    let binary = map (\(a, b) -> if b > a then '1' else '0') counts
    let binary' = map (\x -> if x == '0' then '1' else '0') binary
    let a = convertBin binary
    let b = convertBin binary'
    captionPrint "A: " (a * b)
    let oxyBin = recurseFilter (\xs -> let (a, b) = counting xs in if b >= a then '1' else '0') contents
    let co2Bin = recurseFilter (\xs -> let (a, b) = counting xs in if b >= a then '0' else '1') contents
    print (convertBin oxyBin * convertBin co2Bin) 


recurseFilter :: (String -> Char) -> [String] -> String
recurseFilter _ [] = []
recurseFilter f xs
    | length filtered > 1 = a:recurseFilter f (map tail filtered)
    | otherwise = head filtered
    where
        a = f (map head xs) 
        filtered = filter ((==a) . head) xs


transpose :: [[a]] -> [[a]]
transpose [] = []
transpose xs
    | null (head xs) = []
    | otherwise = [head x | x <- xs]:transpose [tail x | x <- xs]

convertBin :: String -> Int 
convertBin h = convertBin' (length h - 1) h
    where
        convertBin' _ [] = 0
        convertBin' n (x:xs)
            | n < 0 = 0
            | x == '1' = 2 ^ n + convertBin' (n-1) xs
            | otherwise = convertBin' (n-1) xs

count :: (Eq a) => a -> [a] -> Int
count a = length . filter (==a)
