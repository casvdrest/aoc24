module Main where

import Data.List
import Data.Char
import Control.Arrow ((***))
import Data.Text (splitOn , pack , unpack)
import Debug.Trace
import Data.Maybe
import Control.Concurrent
import Control.Monad.Reader

type Location = (Int , Int) 

data Env = Env { grid :: [[Int]] }

inBounds :: Int -> Int -> Location -> Bool
inBounds dimx dimy (x,y) = x >= 0 && y >= 0 && x < dimx && y < dimy
 
neighbors :: Location -> Reader Env [Location]
neighbors (x , y) = do
  Env grid <- ask
  return $ filter (inBounds (length $ grid!!0) $ length grid) $
    [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

val :: Location -> Reader Env Int
val (x,y) = do
  Env grid <- ask
  return $ grid!!y!!x

next :: Location -> Reader Env [Location]
next l = do 
  xs <- neighbors l >>= traverse (\l' -> val l' >>= return . (,)l') 
  v  <- val l
  return $ map fst $ filter ((==)(v+1) . snd) xs

findTrailheads :: Reader Env [Location]
findTrailheads = do
  Env grid <- ask
  vs <- traverse (\l -> val l >>= return . (,)l)
          [(x,y)|x<-[0..length (grid!!0)-1],y<-[0..length grid-1]]
  return $ map fst $ filter ((==)0 . snd) vs

trails :: Location -> Reader Env [[Location]]
trails l = do
  v <- val l
  case v of
    9 -> return [[l]]
    _ -> do 
      r <- next l >>= traverse trails 
      return $ map ((:)l) $ concat r

run = findTrailheads >>= traverse trails 
  
main :: IO ()
main = do
  grid <- map (map digitToInt) . lines <$> readFile "input.txt"
  let ts = runReader run $ Env grid 
  print $ sum $ map (length . nub . map last) $ ts

