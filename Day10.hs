module Day10 where

import           Data.List
import           Data.Map    as Map hiding (filter, foldl)
import           Data.Matrix as M
import           Data.Ord

type Pos = (Int, Int)

type Space = M.Matrix Char

slope :: Pos -> Pos -> Float
slope (x1, y1) (x2, y2) = (fromIntegral (x1 - x2)) / (fromIntegral (y1 - y2))

quadrant :: Pos -> Pos -> Int
quadrant (x1, y1) (x2, y2)
  | (x1 < x2) && (y1 < y2) = 1
  | (x1 > x2) && (y1 < y2) = 2
  | (x1 < x2) && (y1 < y2) = 3
  | (x1 > x2) && (y1 > y2) = 4
  | (x1 == x2) =
    if (y1 < y2)
      then 5
      else if (y1 > y2)
             then 6
             else 7
  | otherwise = 7

coords :: Space -> [Pos]
coords space = (,) <$> [1 .. (nrows space)] <*> [1 .. (ncols space)]

isAsteroid :: Space -> Pos -> Int
isAsteroid space pos =
  if space M.! pos == '#'
    then 1
    else 0

allSlopes space pos =
  fmap
    (\p -> (quadrant p pos, slope p pos, isAsteroid space p))
    (filter (\x -> x /= pos) $ coords space)

type Slopes = Map.Map (Int, Float) Int

toMap :: [(Int, Float, Int)] -> Slopes
toMap slopes =
  foldl
    (\acc (quad, slope, val) -> Map.insertWith (+) (quad, slope) val acc)
    Map.empty
    slopes

countDifferent :: Slopes -> Int
countDifferent slopes =
  sum $
  fmap
    (\(_, val) ->
       if val > 0
         then 1
         else 0)
    (Map.toList slopes)

withAsteroids :: Space -> [Pos]
withAsteroids space = filter (\c -> isAsteroid space c == 1) (coords space)

countAsteroids :: Space -> Int
countAsteroids space = length $ filter (\x -> x == '#') $ M.toList space

main :: IO ()
main = do
  input <- readFile "day10.txt"
  let rows = lines input
  let space = M.fromLists rows
  let possible = withAsteroids space
  let all =
        fmap (\p -> (p, countDifferent $ toMap $ allSlopes space p)) possible
  print $ maximumBy (comparing snd) all
  --print $ allSlopes space (9, 6)
