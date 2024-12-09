module Main where

import Data.List
import Data.Char (isDigit)

data Token = | Mul     -- "mul("
             | PClose   -- ")"
             | Num Int -- numbers
             | Comma   -- ","
             | Any     -- anything else

data Op = Mul Int Int 

tryOptions :: [(String,Token)] -> String -> Maybe (Token , String)
tryOptions [] s = Nothing
tryOptions ((s' , t):opts) s =
  if s' `isPrefixOf` s then
    do rem <- stripPrefix s s'
       return (t , rem)
  else
    tryOptions opts s 

options :: [(String , Token)]
options = 
  [ ("mul(" , Mul)
  , (")"    , PClose)
  , (","    , Comma)
  ] 
  
lexer :: String -> [Token] 
lexer []      = []
lexer s@(h:t) = 
  case tryOptions options s of
    Just (t , s') -> t:lexer s'
    Nothing       ->
      case takeWhile isDigit s of
        [] -> Any:lexer t

readData :: IO String
readData = readFile "input.txt"

main :: IO ()
main = do
  xs <- readData
  undefined


