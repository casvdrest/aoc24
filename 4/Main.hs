module Main where

import Data.List
import Data.Char (isDigit)

xmas = "XMAS" 

diagonals :: [[a]] -> [[a]]
diagonals = tail . go [] where
  go b es_ = [h | h:_ <- b] : case es_ of
    []   -> transpose ts
    e:es -> go (e:ts) es
    where ts = [t | _:t <- b]

count :: String -> String -> Int
count s []        = 0
count s s'@(x:xs) = let x = count s xs in if s `isPrefixOf` s' then x+1 else x

boxes :: [[a]] -> [[[a]]]
boxes (xs:ys:zs:xxs) = go xs ys zs ++ boxes (ys:zs:xxs)
  where go :: [a] -> [a] -> [a] -> [[[a]]]
        go (x1:xs'@(x2:x3:xs)) (y1:ys'@(y2:y3:ys)) (z1:zs'@(z2:z3:zs))
          = [[x1,x2,x3],[y1,y2,y3],[z1,z2,z3]]:go xs' ys' zs'
        go _ _ _                = []
boxes _              = []

checkBox :: [[Char]] -> Bool
checkBox [[a,_,b],[_,c,_],[d,_,e]] =
  let xs = [a,c,e] ; ys = [b,c,d] in
  (xs == "SAM" || xs == "MAS") && (ys == "SAM" || ys == "MAS") 

main :: IO ()
main = do
  d <- lines <$> readFile "input.txt"
  let xs = d <> transpose d <> diagonals d <> diagonals (map reverse d)
  print $ sum $ map (count xmas) $ xs <> map reverse xs
  print $ length $ filter checkBox $ boxes d 
  
  
