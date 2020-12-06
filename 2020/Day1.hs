module Day1 where

toInt :: String -> Int
toInt s = read s

pairsProduct :: [Int] -> [Int] -> Int
pairsProduct [] _ = 1
pairsProduct (x:xs) [] = pairsProduct xs xs
pairsProduct (x:xs) (y:ys) =
  if x + y == 2020
    then x * y
    else pairsProduct (x : xs) ys

sums :: [Int] -> [(Int, Int, Int)]
sums xs = (\x y -> (x, y, x + y)) <$> xs <*> xs

sums3 :: [Int] -> [(Int, Int, Int, Int)]
sums3 xs = (\x (a, b, c) -> (x, a, b, c + x)) <$> xs <*> (sums xs)

solve :: [Int] -> Int
solve xs =
  let (x, y, sum) = head $ filter (\(_, _, s) -> s == 2020) $ sums xs
   in x * y

solve3 :: [Int] -> Int
solve3 xs = 
  let (x, y, z, sum) = head $ filter (\(_, _, _, s) -> s == 2020) $ sums3 xs
   in x * y * z

main :: IO ()
main = do
  content <- readFile "day1.txt"
  let fileLines = lines content
  let numbers = map toInt fileLines
  --print $ solve numbers
  print $ solve3 numbers
