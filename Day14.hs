module Day14 where

import           Data.List.Split
import           Data.Map        as M hiding (drop, filter, take)
import           Debug.Trace

toPair :: [String] -> ([String], String)
toPair (x:y:_) = (splitOn ", " x, y)
toPair _      = ([], "")

toInt :: String -> Int
toInt = read

type Chemical = (Int, String)

type ReactionMap = M.Map Chemical [Chemical]

toReactionMap :: ([String], String) -> ReactionMap
toReactionMap (xs, key) = M.insert (asChemical key) (fmap asChemical xs) M.empty

asChemical :: String -> Chemical
asChemical s =
  let (a:b:_) = splitOn " " s
   in (toInt a, b)

main :: IO ()
main = do
  input <- readFile "day14test.txt"
  let rows = lines input
  let pairs = fmap (toPair . splitOn " => ") rows
  let reactions = fmap toReactionMap pairs
  print $ reactions
