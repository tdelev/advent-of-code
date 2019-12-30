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
  | (y1 == y2) = if x1 < x2 then 1 else if x1 > x2 then 5 else 9
  | (y1 > y2) = if x1 > x2 then 4 else if x1 < x2 then 2 else 3
  | (y1 < y2) = if x1 < x2 then 8 else if x1 > x2 then 6 else 7

coords :: Space -> [Pos]
coords space = (,) <$> [1 .. (nrows space)] <*> [1 .. (ncols space)]

isAsteroid :: Space -> Pos -> Int
isAsteroid space pos =
  if space M.! pos == '#'
    then 1
    else 0

allSlopes space pos =
  fmap
    (\p -> (quadrant p pos, slope p pos, isAsteroid space p, p))
    (filter (/= pos) $ coords space)

type Slopes = Map.Map (Int, Float) Int

toSlopes :: [(Int, Float, Int, Pos)] -> Slopes
toSlopes slopes =
  foldl
    (\acc (quad, slope, val, pos) -> Map.insertWith (+) (quad, slope) val acc)
    Map.empty
    slopes

slopesIndex :: [(Int, Float, Int, Pos)] -> Map.Map (Int, Float) [Pos]
slopesIndex slopes =
  foldl
    (\acc (quad, slope, val, pos) -> Map.insertWith (++) (quad, slope) (if val > 0 then [pos] else []) acc)
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

--ordered :: [(Int, Float, Int, Pos)] -> Pos -> [(Float, Pos)]
--ordered xs start = fmap (\(quad, slope, _, pos) -> (distance (slope, quad, pos) start)) $ filter (\(_, _, c, _) -> c > 0) xs

dd :: Pos -> Pos -> Float
dd (x1, y1) (x2, y2) = fromIntegral ((abs x1 - x2)^2 + (abs y1 - y2)^2)

distance :: (Int, Float, Int) -> Float
distance (quad, slope, depth) = (fromIntegral (depth * 10000)) + (fromIntegral (quad * 1000)) + normSlope slope

normSlope :: Float -> Float
normSlope a = if a == -1/0 || a == 1/0 then 0 else a

weighted :: [a] -> (Int, Float) -> [(Float, a)]
weighted xs (quad, slope) = zipWith (\x i -> ((distance (quad, slope, i)), x)) xs [0..]

main :: IO ()
main = do
  input <- readFile "day10.txt"
  let rows = lines input
  let space = M.fromLists rows
  let candidateLocations = withAsteroids space
  let locationSlopes =
        fmap (\p -> (p, countDifferent $ toSlopes $ allSlopes space p)) candidateLocations
  let tower = maximumBy (comparing snd) locationSlopes
  -- print tower -- Part 1
  let asteroids = allSlopes space $ fst tower
  let ordered = sortBy (comparing fst) $ filter (\(a, b) -> length b > 0) $ Map.toList $ slopesIndex asteroids
  --print ordered
  let wo = sortBy (comparing fst) $ concat $ fmap (\(a, b) -> weighted (sortBy (comparing (dd (fst tower))) b) a) ordered
  print $ wo!!199
