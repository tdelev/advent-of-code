module Day10 where

import           Data.Matrix as M

type Pos = (Int, Int)

type Space = M.Matrix Char

inLOS :: Pos -> Pos -> Space -> Bool
inLOS (x1, y1) (x2, y2) m
  | x1 == (x2 - 1) = True
  | x1 == (x2 + 1) = True
  | y1 == (y2 - 1) = True
  | y1 == (y2 + 1) = True
  | otherwise = False

coords :: Space -> [Pos]
coords space = (,) <$> [1 .. (nrows space)] <*> [1 .. (ncols space)]

los :: Space -> Pos -> Pos -> Int
los space p1 p2 =
  if (inLOS p1 p2 space) && (space ! p2 == '#')
    then 1
    else 0

isAsteroid :: Space -> Pos -> Int
isAsteroid space pos =
  if space ! pos == '#'
    then 1
    else 0

positionsUp (x, y) _ = [(x', y) | x' <- [1 .. (x - 1)]]

positionsDown (x, y) space = [(x', y) | x' <- [(x + 1) .. (nrows space)]]

positionsLeft (x, y) _ = [(x, y') | y' <- [1 .. (y - 1)]]

positionsRight (x, y) space = [(x, y') | y' <- [(y + 1) .. (ncols) space]]

count :: (Pos -> Space -> [Pos]) -> Space -> Pos -> Int
count genPositions space pos =
  let total = sum $ fmap (isAsteroid space) $ genPositions pos space
   in if total > 0
        then 1
        else 0

countUp = count positionsUp

countDown = count positionsDown

countLeft = count positionsLeft

countRight = count positionsRight

allDir = [countUp, countDown, countLeft, countRight]

countAll space pos = sum $ fmap (\f -> f space pos) allDir

countLOS :: Pos -> Space -> Int
countLOS pos space =
  let all = coords space
   in sum $ fmap (\x -> los space pos x) all

countAsteroids :: Space -> Int
countAsteroids space = length $ filter (\x -> x == '#') $ M.toList space

main :: IO ()
main = do
  input <- readFile "day10test.txt"
  let rows = lines input
  let matrix = M.fromLists rows
  print $ countAll matrix (3, 5)
