module Main where

import Data.List (partition)
import Debug.Trace
import Control.Monad

import Data.Maybe (catMaybes)
import Control.Monad.Trans.Writer (Writer, tell, runWriter)
import GHC.RTS.Flags (getCCFlags)
type Point = (Int , Int)

data Robot = Robot Point Point deriving Show

readPoint :: String -> Point
readPoint s = (read (takeWhile (/=',') s) , read (tail $ dropWhile (/=',') s))

readRobot :: String -> Robot
readRobot s =
  let [ps,vs] = words s in
  Robot (readPoint $ drop 2 ps) (readPoint $ drop 2 vs )

simulate :: Int -> (Int , Int) -> Robot -> Robot
simulate ticks (dimx , dimy) (Robot (x,y) v@(vx,vy)) =
  let x' = x + ticks * vx ; y' = y + ticks * vy in
  let x'' = if x' >= 0 then x' `mod` dimx else (abs x' * dimx + x') `mod` dimx in
  let y'' = if y' >= 0 then y' `mod` dimy else (abs y' * dimy + y') `mod` dimy in
  Robot (x'',y'') v

sf :: Point -> [Robot] -> [Int]
sf (dimx,dimy) = foldr countSf [0,0,0,0]
  where
    countSf :: Robot -> [Int] -> [Int]
    countSf (Robot (x,y) _) q@[q1,q2,q3,q4]
      | x < dimx `div` 2 && y < dimy `div` 2 = [q1+1,q2,q3,q4]
      | x < dimx `div` 2 && y > dimy `div` 2 = [q1,q2,q3+1,q4]
      | x > dimx `div` 2 && y < dimy `div` 2 = [q1,q2+1,q3,q4]
      | x > dimx `div` 2 && y > dimy `div` 2 = [q1,q2,q3,q4+1]
      | otherwise = q

printMap :: [Robot] -> Point -> Writer String ()
printMap rs (dimx,dimy) =
  mapM_
    (\c@(cy,cx) ->
      tell (let len = length $ filter (\(Robot c' _) -> c' == (cx,cy)) rs in if len == 0 then "." else show len)
        >> when (cx == dimx - 1) (tell "\n"))
    [(y,x)|y<-[0..dimy-1],x<-[0..dimx-1]]

run1 :: [Robot] -> [Robot]
run1 = map (simulate 100 (101,103))

point (Robot l _) = l

hasNoDups :: (Eq a) => [a] -> Bool
hasNoDups xs = f [] xs
  where
    f _           []     = True
    f seen_so_far (x:xs) = not (x `elem` seen_so_far) && f (x:seen_so_far) xs

run2 :: Int -> [Robot] -> IO [Robot]
run2 n rs = do
  let rs' = map (simulate 1 (101,103)) rs
  let (() , s) = runWriter (printMap rs' (101,103))
  when (hasNoDups $ map point rs') $ putStr s >> print (n+1) >> getChar >> return ()
  run2 (n+1) rs'

  where
    peq :: Point -> Robot -> Bool
    peq l (Robot l' _) = l' == l

    xeq :: Int -> Robot -> Bool
    xeq x (Robot (x',_) _) = x == x'

    yeq :: Int -> Robot -> Bool
    yeq y (Robot (_,y') _) = y == y'


main :: IO ()
main = do
  inp <- map readRobot . lines <$> readFile "input"
  let rs1 = map (simulate 100 (101,103)) inp
  run2 0 inp
  print $ product $ sf (101,103) $ rs1