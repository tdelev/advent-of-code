module Day14 where

import           Data.List
import           Data.List.Split
import           Data.Map        as M hiding (drop, filter, take)
import           Debug.Trace

toPair :: [String] -> ([String], String)
toPair (x:y:_) = (splitOn ", " x, y)

toInt :: String -> Int
toInt = read

type Chemical = (Int, String)

asChemical :: String -> Chemical
asChemical s = 
  let (a:b:_) = splitOn " " s
   in (toInt a, b)

main :: IO ()
main = do
  input <- readFile "day14test.txt"
  let rows = lines input
  let parts = fmap (toPair . splitOn " => ") rows
  print $ fmap (asChemical . snd) parts
