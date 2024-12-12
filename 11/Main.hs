{-# LANGUAGE BangPatterns #-} 

module Main where

import Debug.Trace
import Data.Map.Strict qualified as M 

type Memo = M.Map (Int , Int) Int 

main :: IO ()
main = do
  ss <- map read . words <$> readFile "input.txt"
  let iterations = 75 
  print $ snd $ stones M.empty (ss , iterations)  
  where
    rules :: Int -> [Int]
    rules !n 
      | n == 0 = [1]
      | even (lengthInt n) =
          let l = lengthInt n in
          let p = (10^(lengthInt n `div`2)) in 
          [n `div` p , n `mod` p]
      | otherwise = [n*2024]

    run :: Int -> [Int] -> [Int]
    run 0 !xs = xs
    run !n !xs = run (n-1) (concat $! map (rules $!) xs)


    -- For a given stone and number of iterations, returns the number of stones after iterating
    stone :: Memo -> (Int , Int) -> (Memo , Int)
    stone !mem !i@(s , n) 
      | n > 0 = case mem M.!? i of 
                  Just n  -> (mem , n)
                  Nothing -> stones mem (rules s,n-1) 
      | n == 0  = (mem , 1)

    stones :: Memo -> ([Int] , Int) -> (Memo , Int)
    stones mem (ss , it) = foldr (\s (mem , n) -> let (mem' , x) = stone mem (s,it) in (M.insert (s , it) x mem' , n+x)) (mem , 0) ss 
  
    lengthInt = length . show

