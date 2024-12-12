module Main where 

import Data.Map qualified as M 
import Data.List (partition , nub , sort)

data Cell = Cell Char Bool Bool Bool Bool
type Point = (Int , Int)
data Region = Region Char [Point] deriving Show
type Grid = M.Map Point Char 

neighbors :: Point -> [Point]
neighbors (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

inBounds :: Int -> Int -> Point -> Bool 
inBounds rows cols (x,y) = x >= 0 && y >= 0 && x < cols && y < rows

contains :: [Point] -> Char -> Region -> Bool 
contains ns c (Region c' ls) = c == c' && any (`elem`ls) ns 

extendRegion :: Point -> Region -> Region 
extendRegion l (Region c xs) = Region c (l:xs)

concatRegions :: Char -> [Region] -> Region
concatRegions c [] = Region c [] 
concatRegions c (Region _ ls:rs) = 
  case concatRegions c rs of 
    Region _ rs' -> Region c (nub $ ls <> rs') 

mergeRegions :: [Region] -> [Region]
mergeRegions [] = [] 
mergeRegions (r@(Region c ls):rs) = 
  let (rs1,rs2) = partition (contains ls c) rs in  
  case rs1 of 
    [] -> r:mergeRegions rs 
    _  -> mergeRegions (concatRegions c (r:rs1):rs2)

regions :: Int -> Int -> Grid -> [Region]
regions rows cols grid = mergeRegions $ 
  M.foldrWithKey' addPoint [] grid 
  where 
    addPoint :: Point -> Char -> [Region] -> [Region]
    addPoint l c rs = 
      let ns = neighbors l in 
      let (xs , ys) = partition (contains ns c) rs in
      case xs of 
        [] -> Region c [l] : ys   
        _  -> map (extendRegion l) xs <> ys       

area :: Region -> Int 
area (Region _ r) = length r

perimeter :: Region -> Int 
perimeter(Region _ r) = sum $ 
  map 
    (\l -> 4 - (length $ filter (\l' -> (l' `elem` r)) (neighbors l))
    ) r  


data Direction = N | E | S | W  deriving (Show , Eq)
-- For each row: get the set of "exposed points" in both upwards and downwards direction 
-- then determine which of the exposed sides are connected
-- then sum up+down
-- repeat for columns
-- sum results 
sides :: Region -> Int
sides (Region _ ls) = 
  let xs = sort $ nub $ map fst ls in 
  let ys = sort $ nub $ map snd ls in 
  sum (map (\x -> findSides E $ filter ((==)x . fst) ls) xs) +
  sum (map (\x -> findSides W $ filter ((==)x . fst) ls) xs) +
  sum (map (\y -> findSides N $ filter ((==)y . snd) ls) ys) +
  sum (map (\y -> findSides S $ filter ((==)y . snd) ls) ys)

  where findSides d = countSides d . exposed d ls

exposed :: Direction -> [Point] -> [Point] -> [Point] 
exposed N rs = filter (\(x,y) -> (x,y-1) `notElem` rs)
exposed E rs = filter (\(x,y) -> (x+1,y) `notElem` rs)
exposed S rs = filter (\(x,y) -> (x,y+1) `notElem` rs)
exposed W rs = filter (\(x,y) -> (x-1,y) `notElem` rs)

cnt :: [Int] -> Int 
cnt []       = 0
cnt [x]      = 1 
cnt (x:y:xs) | y-x == 1  = cnt (y:xs) 
             | otherwise = 1 + cnt (y:xs) 

countSides :: Direction -> [Point] -> Int 
countSides d 
  | d `elem` [N,S] = cnt . sort . map fst 
  | d `elem` [W,E] = cnt . sort . map snd 

convert :: Int -> Int -> [[Char]] -> M.Map Point Char 
convert _ _ [] = M.empty 
convert x y ([c]:rs) = M.insert (x,y) c $ convert 0 (y+1) rs
convert x y ((c:cs):rs) = M.insert (x,y) c $ convert (x+1) y (cs:rs)

main :: IO () 
main = do 
  inp <- lines <$> readFile "input"
  let rows = length inp ; cols = length $ inp!!0 
  print -- $ sum 
        $ sum $ map (\r -> sides r * area r)
        $ regions rows cols
        $ convert 0 0 inp