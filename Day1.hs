module Day1 where

toInt :: String -> Int
toInt s = read s

fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

massFuel :: Int -> Int
massFuel mf
  | fmf <= 0  = 0
  | otherwise = fmf + massFuel fmf
  where fmf = fuel mf

fact n
  | n <= 0 = 1
  | otherwise = n * fact (n - 1)

main :: IO ()
main = do
  content <- readFile "day1.txt"
  let fileLines = lines content
  let numbers = map toInt fileLines
    --print $ sum $ map fuel numbers
  print $ sum $ map massFuel numbers
