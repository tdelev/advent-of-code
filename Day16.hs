module Day16 where

import           Data.Char
import           Data.List
import           Debug.Trace

pattern = [0, 1, 0, -1]

onesDigit :: Int -> Int
onesDigit n = (abs n) `mod` 10

fftPhase :: [Int] -> Int -> Int
fftPhase xs phase = onesDigit $ sum $ zipWith (*) xs (fftPattern phase)

fft :: [Int] -> [Int]
fft xs =
  let size = length xs
   in fmap (\p -> fftPhase xs p) [1 .. size]

fftN :: [Int] -> Int -> [Int]
fftN xs n =
  if n == 0
    then xs
    else fftN (fft xs) (n - 1)

patternN :: Int -> [Int]
patternN n = concat $ fmap (replicate n) pattern

fftPattern :: Int -> [Int]
fftPattern phase = drop 1 $ cycle $ patternN phase

asString :: [Int] -> String
asString xs = intercalate "" $ fmap show xs

main :: IO ()
main = do
  input <- readFile "day16.txt"
  let list = fmap digitToInt $ head $ lines input
  print $ asString $ take 8 $ fftN list 100
