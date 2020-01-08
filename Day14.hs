module Day14 where

import           Data.List.Split
import           Data.Map        as M hiding (drop, filter, foldl, take)
import           Debug.Trace

toPair :: [String] -> ([String], String)
toPair (x:y:_) = (splitOn ", " x, y)
toPair _       = ([], "")

toInt :: String -> Int
toInt = read

type Chemical = (Int, String)

type ReactionMap = M.Map String (Int, [Chemical])

type FuelMap = M.Map String Float

toReactionMap :: ([String], String) -> ReactionMap -> ReactionMap
toReactionMap (xs, key) m =
  let (count, chemical) = asChemical key
   in M.insert chemical (count, (fmap asChemical xs)) m

asMap :: [([String], String)] -> ReactionMap
asMap xs = foldl (\m a -> toReactionMap a m) M.empty xs

asChemical :: String -> Chemical
asChemical s =
  let (a:b:_) = splitOn " " s
   in (toInt a, b)

updateFuel :: FuelMap -> Int -> Chemical -> FuelMap
updateFuel acc total (count, chemical) =
  M.insertWith (+) chemical (count / total) acc

isORE :: [String] -> Bool
isORE xs = length xs == 1 && (head xs == "ORE")

getMul :: Int -> Int -> Int
getMul need available =
  if need < available
    then 1
    else let res = need `div` available
             m = need `mod` available
          in if m > 0
               then res + 1
               else res

buildFuelMap :: ReactionMap -> (Int, String) -> FuelMap -> FuelMap
buildFuelMap rmap (need, key) acc =
  if M.null rmap
    then acc
    else let (available, values) = rmap ! key
             multiplier = getMul need available
             resultAcc = foldl (\fm v -> updateFuel fm multiplier v) acc values
             resultMap = M.delete key rmap
             accList = (fmap (\x -> buildFuelMap resultMap x M.empty) values)
             finalAcc = foldl (M.unionWith (+)) resultAcc accList
          in if isORE (fmap snd values)
               then acc
               else finalAcc

findFuel' :: ReactionMap -> (Int, String) -> Int
findFuel' rmap (need, key) =
  if M.null rmap
    then 0
    else let (available, values) = rmap ! key
             multiplier = getMul need available
             resultMap = M.delete key rmap
             total = sum $ fmap (\x -> findFuel resultMap x) values
          in if isORE (fmap snd values)
               then multiplier * (fst $ head values)
               else total

findFuel rmap nk = trace ("find fuel : map = " ++ (show rmap) ++ " ; key = " ++ (show nk)) findFuel' rmap nk


findORE :: (String, Int) -> ReactionMap -> Int
findORE (key, total) rmap =
  let (count, value) = rmap ! key
   in if isORE (fmap snd value)
        then let oreV = fst $ head value
              in (getMul total count) * oreV
        else 0

main :: IO ()
main = do
  input <- readFile "day14test.txt"
  let rows = lines input
  let pairs = fmap (toPair . splitOn " => ") rows
  let reactions = asMap pairs
  let fuels = buildFuelMap reactions (1, "FUEL") M.empty
  --print $ findFuel reactions (1, "FUEL")
  print $ fuels
  print $ sum $ fmap (\x -> findORE x reactions) (M.toList fuels)
