module Main where

import Data.List

readData :: IO [[Int]] 
readData = do
  d <- readFile "input.txt"
     >>= return . map (map read) . map words . lines
  return d 

main :: IO ()
main = do
  return ()
