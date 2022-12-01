import System.IO
import Data.List

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delim l = filter (/= [delim]) $ groupBy (\x y -> (x == delim) == (y == delim)) l

getList :: String -> [[Int]]
getList s = map (map read) $ splitOn "" $ lines s

getAnswer :: [[Int]] -> Int
getAnswer l = sum $ take 3 $ reverse $ sort $ map sum l

solve :: String -> String
solve s = show $ getAnswer $ getList s

main :: IO ()
main = do 
    input <- readFile "input"
    putStrLn $ solve input
