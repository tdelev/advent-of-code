module Day14 where

import           Data.List.Split
import           Data.Map        as M hiding (drop, filter, take, foldl)
import           Debug.Trace

toPair :: [String] -> ([String], String)
toPair (x:y:_) = (splitOn ", " x, y)
toPair _      = ([], "")

toInt :: String -> Int
toInt = read

type Chemical = (Int, String)

type ReactionMap = M.Map String [Chemical]

toReactionMap :: ([String], String) -> ReactionMap -> ReactionMap
toReactionMap (xs, key) m = M.insert (snd $ asChemical key) (fmap asChemical xs) m

asMap :: [([String], String)] -> ReactionMap
asMap xs = foldl (\m a -> toReactionMap a m) M.empty xs

asChemical :: String -> Chemical
asChemical s =
  let (a:b:_) = splitOn " " s
   in (toInt a, b)

main :: IO ()
main = do
  input <- readFile "day14test.txt"
  let rows = lines input
  let pairs = fmap (toPair . splitOn " => ") rows
  let reactions = asMap pairs
  print $ reactions
