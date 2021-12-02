import Utils

main = do
    contents <- readFile "2.txt"
    let cont' = convertPair $ lines contents
    let (f, d) = foldl depthFunc (0, 0) cont'
    captionPrint "A: " (f * d)
    let (f', d', a) = foldl depthFunc' (0, 0, 0) cont'
    captionPrint "B: " (f' * d')


depthFunc :: Num a => (a, a) -> (String, a) -> (a, a)
depthFunc (a, b) (n, x)
    | n == "forward" = (a + x, b)
    | n == "up" = (a, b - x)
    | n == "down" = (a, b + x)
    | otherwise = (a, b)

depthFunc' :: Num a => (a, a, a) -> (String, a) -> (a, a, a)
depthFunc' (a, b, c) (n, x)
    | n == "forward" = (a + x, b + c * x, c)
    | n == "up" = (a, b, c - x)
    | n == "down" = (a, b, c + x)
    | otherwise = (a, b, c)


