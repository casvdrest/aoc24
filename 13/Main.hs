module Main where 

import Data.List (partition)
import Debug.Trace
import Data.Maybe (catMaybes)
type Point = (Int , Int)

split :: (a -> Bool) -> [a] -> ([a], [a])
split f s = (left,right)
        where
        (left,right')=break f s
        right = if null right' then [] else tail right'

splitList :: Eq a => a -> [a] -> [[a]]
splitList _   [] = []
splitList sep list = h:splitList sep t
        where (h,t)=split (==sep) list

data Problem = Problem Point Point Point deriving Show

readProblem :: [String] -> Problem 
readProblem [a,b,prize] = Problem (readc 2 a) (readc 2 b) (readc 1 prize)
  where 
    readc :: Int -> String -> Point 
    readc n s = let [x,y] = map (drop 2) $ drop n $ words s in 
      if n > 1 then (read (init x) , read y) else (read (init x)+10000000000000 , read y+10000000000000)

solve :: Problem -> Maybe Int 
solve (Problem (dxa,dya) (dxb, dyb) (px,py)) = 
  let a1 = (px*dyb - py*dxb) in 
  let a2 = (dxa*dyb - dya*dxb) in 
  let b1 = (dxa*py - dya*px) in 
  let b2 = (dxa*dyb - dya*dxb) in 
  if a1 `mod` a2 == 0 && b1 `mod` b2 == 0 then 
    Just ((a1 `div` a2)*3 + (b1 `div` b2))
  else 
    Nothing 

main :: IO () 
main = do 
  inp <- map readProblem . splitList [] . lines <$> readFile "input"
  print $ sum $ catMaybes $ map solve $ inp