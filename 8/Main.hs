module Main where

import Data.List
import Data.Char
import Control.Arrow ((***))
import Data.Text (splitOn , pack , unpack)
import Debug.Trace
import Data.Maybe
import Control.Concurrent
import Data.Map as M (lookup , insert, fromList , Map , elems)

data Cell = Empty | Antenna Char
instance Show Cell where
  show Empty = "."
  show (Antenna c) = [c] 

type Grid = [[Cell]]
type Location = (Int , Int)

antennas :: Grid -> M.Map Char [Location]
antennas xs = go 0 xs $ M.fromList []
  where
    go :: Int -> Grid -> M.Map Char [Location] -> M.Map Char [Location]
    go y []     mp = mp 
    go y (r:rs) mp = go (y+1) rs $ goRow r (0,y) mp

    goRow :: [Cell] -> Location -> M.Map Char [Location] -> M.Map Char [Location]
    goRow []     (x,y) mp = mp
    goRow (c:cs) (x,y) mp =
      case c of
        Empty     -> goRow cs (x+1,y) mp
        Antenna a -> goRow cs (x+1,y) $ M.insert a (maybe [(x,y)] ((:)(x,y)) $ M.lookup a mp) mp 

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (x:xs) = fmap (x,) xs <> pairs xs

antinodes :: Grid -> (Location, Location) -> [Location]
antinodes grid (l1@(x,y),l2@(x',y')) =
  let xdiff = x'-x ; ydiff = y'-y in
  takeWhile (inBounds grid) (iterate (\(x,y) ->(x-xdiff,y-ydiff)) l1)
  <> takeWhile (inBounds grid) (iterate (\(x,y) -> (x+xdiff ,y+ydiff)) l2)

inBounds :: Grid -> Location -> Bool
inBounds grid (x,y) = x >= 0 && y >= 0 && x < length (grid!!0) && y < length grid 
  
main :: IO ()
main = do
 grid <- (fmap (fmap (\c -> if c == '.' then Empty else Antenna c)) . lines) <$> readFile "input.txt"
 print $ length $ nub $ filter (inBounds grid) $ concat $ concat $ fmap (fmap (antinodes grid) . pairs) $ elems $ antennas grid 
 
