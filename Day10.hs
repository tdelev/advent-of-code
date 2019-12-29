module Day10 where

import           Data.Matrix as M

type Pos = (Int, Int)

type Space = M.Matrix Char

isAsteroid :: Pos -> Space -> Bool
isAsteroid pos m = m ! pos == '#'

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
  print $ countLOS (1, 1) matrix
