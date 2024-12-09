module Main where

import Data.List
import Data.Char
import Control.Arrow ((***))
import Data.Text (splitOn , pack , unpack)
import Debug.Trace
import Data.Maybe
import Control.Concurrent

readl :: String -> (Int , [Int])
readl s = (read (takeWhile (/=':') s) , map read $ words $ tail $ dropWhile (/=':') s)

conc :: Int -> Int -> Int
conc x y = read (show x ++ show y)

options :: Int -> [Int] -> [Int]
options n []     = [n]
options n (x:xs) = options (n + x) xs ++ options (n * x) xs ++ options (conc n x) xs
  

reaches :: (Int , [Int]) -> Bool
reaches (n , ns) = any (==n) $ options (head ns) (tail ns) 

main :: IO ()
main = do
 ls <- (map readl . lines) <$> readFile "input.txt"
 print $ sum $ map fst $ filter reaches $ ls
  
