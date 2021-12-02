module Utils where
import System.IO

listRead :: String -> IO [Int]
listRead file = do
    contents  <- readFile file
    return $ map read (lines contents) :: IO [Int]


captionPrint :: (Show a) => String -> a -> IO ()
captionPrint t v = putStr $ t ++ show v ++ "\n"

convertPair :: [String] -> [(String, Int)]
convertPair [] = []
convertPair (x:xs) = (head strPair, read (last strPair)):convertPair xs
    where strPair = words x