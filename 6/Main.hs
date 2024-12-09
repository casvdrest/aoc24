module Main where

import Data.List
import Data.Char
import Control.Arrow ((***))
import Data.Text (splitOn , pack , unpack)
import Debug.Trace
import Data.Maybe
import Control.Concurrent


import Control.Monad.State

data Direction = N | E | S | W deriving (Show , Enum , Eq) 
data Cell = Obstacle | Empty deriving (Eq)
type Tile = (Cell, (Bool, [Direction]))
type Map = [[Tile]]
type History = [(Action, Direction , (Int , Int), Map)]
data St = St
  { unMap :: Map
  , unLoc :: Location
  , unDir :: Direction
  , history :: History 
}
type Location = (Int , Int) 


instance Show Cell where
  show Obstacle = "#"
  show Empty    = "."

data Action = Turn | Exit | Walk deriving (Show , Eq) 

nextD = \d -> if d == W then N else succ d
nextL = \d (x,y) ->
  case d of
    N -> (x,y-1)
    E -> (x+1,y)
    S -> (x,y+1)
    W -> (x-1,y)
inBounds = \(x,y) (dimx,dimy) -> x >= 0 && y >= 0 && x < dimx && y < dimy  
dims = \xs -> (length (xs !! 0) , length xs) 

getAction :: State St Action
getAction = do
  St mp l d h <- get
  let l'@(x,y) = nextL d l
  if not (inBounds l' (dims mp)) then return Exit else (
    if fst (mp !! y !! x) == Obstacle then return Turn else
    return Walk)

turn :: State St ()
turn = get >>= \s -> put (s {unDir = nextD $ unDir s} )  

writeHistory :: Action -> State St ()
writeHistory a = do
  st <- get
  put $ st { history = (a, unDir st, unLoc st, unMap st):history st }

visitRow :: Direction -> Int -> [Tile] -> ([Tile] , Bool)
visitRow d 0 ((c,(_,ds)):xs) = ((c,(True,d:ds)):xs , d `elem` ds)
visitRow d n (x:xs)        = let (xs' , isLoop) = visitRow d (n-1) xs in (x:xs' , isLoop)

visit :: Direction -> Location -> Map -> (Map, Bool)
visit d (x,0) (r:mp) = let (r',isLoop) = visitRow d x r in (r':mp,isLoop)
visit d (x,y) (r:mp) = let (mp',isLoop) = visit d (x,y-1) mp in (r:mp',isLoop)

act :: Action -> State St Bool 
act Turn = do
  writeHistory Turn
  st <- get
  put $ st { unDir = nextD $ unDir st }
  return False
act Walk = do
  --writeHistory Walk
  st <- get
  let (mp', isLoop) = visit (unDir st) (unLoc st) (unMap st) 
  put $ st { unLoc = nextL (unDir st) (unLoc st) , unMap = mp'  }
  return isLoop

run :: State St Bool 
run = getAction >>=
  \a -> if a == Exit then return False else (act a >>= \isLoop -> if isLoop then return True else run)

printGuard :: Direction -> Char
printGuard N = '^'
printGuard E = '>'
printGuard S = 'v'
printGuard W = '<'

printCell :: Bool -> Cell -> Char
printCell b c =
  if b then 'X' else
    case c of
      Obstacle -> '#'
      Empty    -> '.' 

printRow :: Direction -> Maybe Int -> [Tile] -> String
printRow d mx [] = ""
printRow d mx ((c,(v,_)):cs) =
  let ch = if mx == Just 0 then printGuard d else printCell v c in
  ch:printRow d ((\x -> x-1) <$> mx)  cs

printMap :: Direction -> Location -> Map -> IO ()
printMap d (x,y) []     = return ()
printMap d (x,y) (r:xs) | y == 0     = putStrLn (printRow d (Just x) r) >> printMap d (x,y-1) xs
                        | otherwise  = putStrLn (printRow d Nothing r) >> printMap d (x,y-1) xs

debug :: History -> IO ()
debug [] = putStrLn "=== DONE ==="
debug ((a,d,(x,y),mp):xs) = do
  putStrLn $ "ACTION: " <> show a
  printMap d (x,y) mp
  threadDelay 15000 >> debug xs 

readRow :: String -> ([Cell] , Maybe Int)
readRow ""     = ([],Nothing)
readRow (x:xs) | x == '^' = (Empty:fst (readRow xs) , Just 0)
               | otherwise = let (cs, mx) = readRow xs in
                             case x of
                               '#' -> (Obstacle:cs,(+1)<$>mx)
                               '.' -> (Empty:cs,(+1)<$>mx) 

readMap :: [String] -> (Map , Maybe Location)
readMap [] = ([] , Nothing)
readMap (x:xs) =
  let (mp , ml) = readMap xs in
  let (cs , mx') = readRow x in
  (zip cs (repeat (False , [])):mp , maybe ((,0) <$> mx') (pure . ((+1)<$>) ) ml ) 

tryMap :: St -> IO (Maybe Int)
tryMap st = do 
  let (lp, st') = runState run st 
  debug $ reverse $ history st'
  if not lp then
    return $ Just $ sum $ map (foldr (\(_,(v,_)) x -> if v then x+1 else x) 0) (fst $ visit N (unLoc st') $ unMap st')
  else return $ Nothing 

addObstacle :: St -> Location -> Maybe St
addObstacle st l@(x,y) =
  case fst $ unMap st !! y !! x of
    Obstacle -> Nothing
    Empty    -> pure $ st { unMap = go l (unMap st) }

  where 
    go :: Location -> Map -> Map
    go (x,0) (r:rs) = go' x r :rs
    go (x,y) (r:rs) = r:go (x,y-1) rs

    go' :: Int -> [Tile] -> [Tile]
    go' 0 ((_,i):ts) = (Obstacle,i):ts
    go' x (t:ts)     = t:go' (x-1) ts

  
main :: IO ()
main = do
  (mp , Just l) <- (readMap . lines) <$> readFile "input.txt"
  let initialState = St mp l N [(Walk, N , l , mp)]
  x <- tryMap initialState
  case x of
    Just n -> print n
    Nothing -> print "LOOP!"
  --let coords = filter ((/=)l) [(x,y)|x<-[0..(fst $ dims mp)-1],y<-[0..(snd $ dims mp)-1]]
  --print $ length $ filter isNothing $ map (tryMap) $ catMaybes $ (map (addObstacle initialState) coords)
  
  
