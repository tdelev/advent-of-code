module Day12 where

import           Data.List
import           Data.List.Split

data Position =
  Pos
    { x :: Int
    , y :: Int
    , z :: Int
    }
  deriving (Show)

data Velocity =
  Vel
    { vx :: Int
    , vy :: Int
    , vz :: Int
    }
  deriving (Show)

data Planet =
  Planet
    { position :: Position
    , velocity :: Velocity
    }
  deriving (Show)

change :: Int -> Int -> Int
change a b
  | a < b = 1
  | a > b = -1
  | otherwise = 0

gravity :: Planet -> Planet -> Planet
gravity (Planet (Pos x1 y1 z1) (Vel vx vy vz)) (Planet (Pos x2 y2 z2) _) =
  let dx = change x1 x2
      dy = change y1 y2
      dz = change z1 z2
   in Planet (Pos x1 y1 z1) (Vel (vx + dx) (vy + dy) (vz + dz))

toInt :: String -> Int
toInt s = read s

strip :: String -> String
strip (_:xs) = take (length xs - 1) xs

toPlanet :: String -> Planet
toPlanet row =
  let parts = splitOn ", " row
      [x, y, z] = fmap (toInt . head . tail . splitOn "=") parts
   in Planet (Pos x y z) (Vel 0 0 0)

planetGravity :: Planet -> [Planet] -> Planet
planetGravity = foldl gravity

applyGravity :: [Planet] -> [Planet]
applyGravity xs = fmap (\x -> planetGravity x xs) xs

velocityF :: Planet -> Planet
velocityF (Planet (Pos x y z) (Vel vx vy vz)) =
  (Planet (Pos (x + vx) (y + vy) (z + vz)) (Vel vx vy vz))

applyVelocity :: [Planet] -> [Planet]
applyVelocity xs = fmap velocityF xs

potentialEnergy :: Planet -> Int
potentialEnergy (Planet (Pos x y z) _) = sum $ fmap abs [x, y, z]

kineticEnergy :: Planet -> Int
kineticEnergy (Planet _ (Vel x y z)) = sum $ fmap abs [x, y, z]

totalEnergy :: Planet -> Int
totalEnergy planet = (potentialEnergy planet) * (kineticEnergy planet)

step :: [Planet] -> [Planet]
step = applyVelocity . applyGravity

steps :: Int -> [Planet] -> [Planet]
steps 0 planets = planets
steps n planets = steps (n - 1) $ step planets

total :: [Planet] -> Int
total xs = sum $ fmap totalEnergy xs

getPos :: [Planet] -> (Position -> Int) -> [Int]
getPos planets f = fmap (f . position) planets

findCicle :: [Planet] -> Int -> [Int] -> (Position -> Int) -> Int
findCicle planets n start f =
  let next = (step planets)
      pos = getPos next f
   in if pos == start
        then n + 1
        else findCicle next (n + 1) start f

lcmm :: Int -> Int -> Int -> Int
lcmm a b c = lcm (lcm a b) c

main :: IO ()
main = do
  input <- readFile "day12.txt"
  let rows = fmap strip $ lines input
  let planets = fmap toPlanet rows
  --let second = head $ tail planets
  let startX = getPos planets x
  let startY = getPos planets y
  let startZ = getPos planets z
  let cx = findCicle planets 1 startX x
  let cy = findCicle planets 1 startY y
  let cz = findCicle planets 1 startZ z
  print $ startX
  print $ startY
  print $ startZ
  print $ lcmm cx cy cz
