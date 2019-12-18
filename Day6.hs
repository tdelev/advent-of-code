module Day6 where

import           Data.List.Split
import           Data.Map        as M

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

main :: IO ()
main = do
  input <- readFile "day6.txt"
  let connections = lines input
  let parsed = parse connections
  let map = toMap parsed
  print $ count map 0 "COM"
