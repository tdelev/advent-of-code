module Day6 where

import           Data.List.Split
import           Data.Map        as M hiding (filter)

type Pair = (String, String)

type PlanetMap = M.Map String [String]

parse :: [String] -> [Pair]
parse = fmap $ toPair . splitOn ")"

toPair :: [String] -> Pair
toPair (a:b:_) = (a, b)

toMap :: [Pair] -> PlanetMap
toMap pairs =
  Prelude.foldl (\b (key, val) -> M.insertWith (++) key [val] b) M.empty pairs

type Planet = (String, Int)

data Cosmos =
  Cosmos Planet [Cosmos]
  deriving (Show)

toCosmos :: String -> Int -> [String] -> Cosmos
toCosmos start n xs = Cosmos (start, n) (fmap (\a -> Cosmos (a, n + 1) []) xs)

count :: PlanetMap -> Int -> String -> Int
count map n key =
  if M.null map
    then n
    else if (member key map) == False
           then n
           else let elements = map ! key
                    updated = M.delete key map
                 in n + (sum $ fmap (count updated (n + 1)) elements)

mTraverse :: PlanetMap -> String -> Int -> [[Planet]] -> [[Planet]]
mTraverse map start level acc =
  if M.null map
    then []
    else if (member start map) == False
           then [concat $ acc ++ [[(start, level)]]]
           else let elements = map ! start
                    links =
                      fmap
                        (\x ->
                           mTraverse
                             (M.delete start map)
                             x
                             (level + 1)
                             [(concat $ acc ++ [[(start, level)]])])
                        elements
                 in concat links

find :: String -> [Planet] -> Bool
find key links = (length $ filter (\x -> (fst x) == key) links) > 0

findLink :: [[Planet]] -> String -> [Planet]
findLink links key = head $ filter (find key) links

findMatching :: [Planet] -> [Planet] -> [Planet]
findMatching (x:xs) (y:ys) =
  if x == y
    then (findMatching xs ys) ++ [x]
    else []

findElement :: [Planet] -> String -> Planet
findElement links key = head $ filter (\x -> (fst x) == key) links

distance :: [Planet] -> String -> Int
distance links key = snd $ findElement links key

main :: IO ()
main = do
  input <- readFile "day6.txt"
  let connections = lines input
  let parsed = parse connections
  let map = toMap parsed
  let links = mTraverse map "COM" 0 []
  let me = findLink links "785"
  let san = findLink links "D1K"
  let matching = findMatching me san
  let intersection = snd $ head matching
  let meDistance = distance me "785"
  let sanDistance = distance san "D1K"
  print $ (meDistance - intersection) + (sanDistance - intersection)
