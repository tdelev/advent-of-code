module Day17 where

import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.List.Split
import           Data.Map        as M hiding (drop, filter, foldl, take)
import           Data.Sequence   as S hiding (filter, replicate, zip)
import           Debug.Trace
import           Intcode         hiding (main)
import           Prelude         hiding (Left, Right)

data Tile
  = Open
  | Scafold
  | Up
  | Down
  | Left
  | Right
  deriving (Eq, Show, Enum)

type Position = (Int, Int)

type View = M.Map Position Tile

viewSize = 61

combine :: (View, Position) -> Int -> (View, Position)
combine (view, (x, y)) c =
  case chr c of
    '#'  -> (M.insert (x, y) Scafold view, next)
    '.'  -> (M.insert (x, y) Open view, next)
    '\n' -> (view, (1, y + 1))
    '^'  -> (M.insert (x, y) Up view, next)
    'v'  -> (M.insert (x, y) Down view, next)
    '<'  -> (M.insert (x, y) Left view, next)
    '>'  -> (M.insert (x, y) Right view, next)
  where
    next = (x + 1, y)

asView :: [Int] -> View
asView xs = fst $ foldl combine (M.empty, (1, 1)) xs

drawOutput :: [Int] -> String
drawOutput xs = fmap chr xs

isIntersection :: View -> (Position, Tile) -> Bool
isIntersection view ((x, y), Scafold) =
  x > 1 &&
  x < viewSize &&
  y > 1 &&
  y < viewSize &&
  (view ! (x - 1, y) == Scafold) &&
  (view ! (x + 1, y) == Scafold) &&
  (view ! (x, y - 1) == Scafold) && (view ! (x, y + 1) == Scafold)
isIntersection _ _ = False

drawTile :: Tile -> Char
drawTile Open    = '.'
drawTile Scafold = '#'
drawTile Up      = '^'
drawTile Down    = 'v'
drawTile Left    = '<'
drawTile Right   = '>'

drawView :: View -> String
drawView view =
  let positions = M.keys view
      minx = minimum $ fmap fst positions
      maxx = maximum $ fmap fst positions
      miny = minimum $ fmap snd positions
      maxy = maximum $ fmap snd positions
      lines =
        [ [(drawTile $ view ! (x, y)) | x <- [minx .. maxx]]
        | y <- [miny .. maxy]
        ]
   in unlines lines

sumAlignment :: [(Position)] -> Int
sumAlignment xs = sum $ fmap (\(x, y) -> ((x - 1) * (y - 1))) xs

main :: IO ()
main = do
  input <- readFile "day17.txt"
  let opcodes = getOpcodes input
  let memory = load opcodes
  let comp = boot memory
  let output = computerOutput $ run comp {computerState = Running}
  let view = asView output
  let intersections = filter (isIntersection view) $ M.toList view
  -- putStrLn $ drawOutput output
  -- print $ view
  -- putStrLn $ drawView view
  print $ sumAlignment $ fmap fst intersections
