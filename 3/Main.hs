module Main where

import Data.List
import Data.Char (isDigit)

data Token = Mul     -- "mul("
           | PClose   -- ")"
           | Num Int -- numbers
           | Comma   -- ","
           | Any     -- anything else
           | Do
           | Dont
           deriving (Show)

data Op = MulOp Int Int
        | DoOp
        | DontOp 
        deriving (Show)

tryOptions :: [(String,Token)] -> String -> Maybe (Token , String)
tryOptions [] s = Nothing
tryOptions ((s' , t):opts) s =
  if s' `isPrefixOf` s then
    do rem <- stripPrefix s' s
       return (t , rem)
  else
    tryOptions opts s 

options :: [(String , Token)]
options = 
  [ ("mul("    , Mul)
  , ("do()"    , Do)
  , ("don't()" , Dont)
  , (")"       , PClose)
  , (","       , Comma)
  ] 
  
lexer :: String -> [Token] 
lexer []      = []
lexer s@(_:rem) = 
  case tryOptions options s of
    Just (t , s') -> t:lexer s'
    Nothing       ->
      case takeWhile isDigit s of
        [] -> Any:lexer rem
        i  -> Num (read i):lexer (dropWhile isDigit s)

parseMulOp :: [Token] -> Maybe (Op , [Token])
parseMulOp (Num x:Comma:Num y:PClose:xs) = Just (MulOp x y , xs)
parseMulOp _                             = Nothing 
  
parser :: [Token] -> [Op]
parser []          = [] 
parser ts@(Mul:xs) =
  case parseMulOp xs of
    Just (op , ts) -> op:parser ts
    Nothing        -> parser xs
parser (Do:ts)   = DoOp:parser ts
parser (Dont:ts) = DontOp:parser ts
parser ts@(_:xs) = parser xs

readData :: IO String
readData = readFile "input.txt"

run :: [Op] -> Bool -> Int
run []              _ = 0 
run (MulOp x y:ops) b = (if b then x*y else 0) + run ops b
run (DoOp:ops)      b = run ops True
run (DontOp:ops)    b = run ops False 

main :: IO ()
main = do
  xs <- readData
  print (run (parser $ lexer xs) True)
  
