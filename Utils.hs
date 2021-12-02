module Utils where
import System.IO

listRead :: String -> IO [Int]
listRead file = do
    contents  <- readFile file
    return $ map read (lines contents) :: IO [Int]


captionPrint :: (Show a) => String -> a -> IO ()
captionPrint t v = putStr $ "part A: " ++ show v ++ "\n"