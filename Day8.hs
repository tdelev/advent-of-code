module Day8 where

import           Data.Char
import           Data.List
import           Data.Ord

type Layer = [Int]

type Pixel = [Int]

layerSize = 25 * 6

toLayers :: [Int] -> [Layer]
toLayers [] = []
toLayers xs = (take layerSize xs) : (toLayers (drop layerSize xs))

toPixels :: [Layer] -> [Pixel]
toPixels ([]:_) = []
toPixels xs     = (map head xs) : toPixels (map tail xs)

countDigit :: Int -> Layer -> Int
countDigit digit layer = length $ filter (== digit) layer

countZero = countDigit 0

countOne = countDigit 1

countTwo = countDigit 2

minZeros :: [Layer] -> Layer
minZeros layer = minimumBy (comparing countZero) layer

topPixel :: Pixel -> Int
topPixel (x:xs) =
  if x < 2
    then x
    else topPixel xs

--forPrint :: [Int] -> String


main :: IO ()
main = do
  input <- readFile "day8.txt"
  let line = head $ lines input
  let digits = fmap digitToInt line
  let layers = toLayers digits
    --let minLayer = minZeros layers
    --let ones = countOne minLayer
    --let twos = countTwo minLayer
  let pixels = toPixels layers
  let final = map topPixel pixels
  print $ map intToDigit final
    --print $ pixels
