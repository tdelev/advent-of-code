module Day16 where

import           Data.Char
import           Data.List
import           Debug.Trace

pattern = [0, 1, 0, -1]

onesDigit :: Int -> Int
onesDigit n = (abs n) `mod` 10

fftPhase :: [Int] -> Int -> Int
fftPhase xs phase = onesDigit $ sum $ zipWith (*) xs (fftPattern phase)

fftPhase' :: [Int] -> [Int] -> Int
fftPhase' [] _          = 0
fftPhase' (x:xs) (y:ys) = (x * y) + fftPhase' xs ys

fft :: [Int] -> [Int]
fft xs =
  let size = length xs
   in fmap (\p -> onesDigit $ fftPhase' xs (fftPattern p)) [1 .. size]

fft' :: [Int] -> [Int]
fft' xs = snd $ fftMagicB xs --reverse $ fftMagic (reverse xs) 0

fftMagic :: [Int] -> Int -> [Int]
fftMagic (x:[]) n = ((n + x) `mod` 10) : []
fftMagic (x:xs) n = ((n + x) `mod` 10) : (fftMagic xs (n + x))

fftMagicB :: [Int] -> (Int, [Int])
fftMagicB (x:[]) = (x, [x])
fftMagicB (x:xs) =
  let (total, array) = fftMagicB xs
      total' = x + total
   in (total', (total' `mod` 10) : array)

fftN :: [Int] -> Int -> [Int]
fftN xs n = foldl' (\list _ -> fft' list) xs [1 .. n]
  --if n == 0
    --then xs
    --else fftN (fft' xs) (n - 1)

patternN :: Int -> [Int]
patternN n = concat $ fmap (replicate n) pattern

fftPattern :: Int -> [Int]
fftPattern phase = drop 1 $ cycle $ patternN phase

asString :: [Int] -> String
asString xs = intercalate "" $ fmap show xs

toInt :: String -> Int
toInt = read

main :: IO ()
main = do
  input <- readFile "day16.txt"
  let list = fmap digitToInt $ head $ lines input
  let list2 = concat $ replicate 10000 list
  let offset = toInt $ asString $ take 7 list2
  let size = length list2
  let final = drop offset list2
  print $ asString $ take 8 $ fftN final 100
