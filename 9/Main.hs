module Main where

import Data.List
import Data.Char
import Control.Arrow ((***))
import Data.Text (splitOn , pack , unpack)
import Debug.Trace
import Data.Maybe
import Control.Concurrent

data Segment = File Int Int
             | Free Int
             deriving Show 

type Disk = [Segment]

readDisk :: [Int] -> Disk
readDisk = go True 0 
  where
    go :: Bool -> Int -> [Int] -> Disk
    go _     _  []     = []
    go True  id (x:xs) = File id x : go False (id+1) xs
    go False id (x:xs) = Free x : go True id xs 

compact :: Disk -> Disk
compact []                     = []
compact (file@(File _ _):disk) = file:compact disk
compact (Free n:disk)          =
  let (segs, disk') = takeSegs n (reverse disk) in
  segs <> compact (reverse disk') 
  where 
    takeSegs :: Int -> Disk -> (Disk , Disk)
    takeSegs n []            = ([] , []) 
    takeSegs n (Free _:disk) = takeSegs n disk
    takeSegs n (seg@(File id m):disk)
      | n == m = ([seg] , disk) 
      | n > m  = let (segs, disk') = takeSegs (n-m) disk in (seg:segs,disk')
      | n < m  = ([File id n],File id (m-n):disk)

compact' :: Disk -> Disk
compact' disk = reverse $ go (reverse disk)
  where
    go :: Disk -> Disk
    go [] = []
    go (file@(File id n):disk)
      = maybe (file:go disk)
         (\disk' -> Free n:go (reverse disk'))
          $ tryMove file (reverse disk) 
    go (Free n:disk) = Free n:go disk

    tryMove :: Segment -> Disk -> Maybe Disk
    tryMove _                []            = Nothing
    tryMove file@(File id n) (Free m:disk)
      | n == m = pure $ file:disk 
      | n > m  = (:) (Free m) <$> tryMove file disk
      | n < m  = pure $ file:Free (m-n):disk
    tryMove file (file'@(File _ _):disk) = (:)file' <$> tryMove file disk 
  

decode :: Disk -> [Int]
decode [] = []
decode (Free n:disk)    = replicate n 0 <> decode disk 
decode (File id n:disk) = replicate n id <> decode disk  

checksum = sum . map (uncurry (*)) . zip [0..]

main :: IO ()
main = do
  disk <- readDisk . map digitToInt . init <$> readFile "input.txt"
  print $ checksum $ decode $ compact disk
  print $ checksum $ decode $ compact' disk

