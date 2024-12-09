module Main where

import Data.List
import Data.Char
import Control.Arrow ((***))
import Data.Text (splitOn , pack , unpack)
import Debug.Trace

data Rule = Int :| Int deriving (Show, Eq)

satisfy :: [Rule] -> [Int] -> Bool
satisfy rs xs = all (\(x :| y) -> maybe True id ((<) <$> elemIndex x xs <*> elemIndex y xs)) rs 

readRule :: String -> Rule
readRule s = let (x,_:y) = break (=='|') s in read x :| read y 

readUpdate :: String -> [Int] 
readUpdate = map (read . unpack) . splitOn (pack ",")  . pack 

compareWith :: [Rule] -> Int -> Int -> Ordering
compareWith rules x y =
  if (x :| y) `elem` rules then LT else
  if (y :| x) `elem` rules then GT else EQ

main :: IO ()
main = do
  (rules , _:updates) <-
    (map readRule *** map readUpdate) . break (==[]) . lines <$> readFile "input.txt"
  let correct = filter (satisfy rules) updates
  let incorrect = filter (not . satisfy rules) updates 
  print $ sum $ map (\xs -> xs !! (length xs `div` 2)) $ correct
  print $ sum $ map (\xs -> xs !! (length xs `div` 2)) $ map (sortBy (compareWith rules)) $ incorrect
  
  
