module Day17 where

import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.List.Split
import           Data.Map        as M hiding (drop, filter, foldl, null, take)
import           Data.Sequence   as S hiding (filter, length, null, replicate,
                                       reverse, zip, zipWith)
import           Data.Set        (Set)
import qualified Data.Set        as Set
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

xyDelta = [(1, 0), (-1, 0), (0, 1), (0, -1)]

isIntersection :: View -> (Position, Tile) -> Bool
isIntersection view ((x, y), Scafold) =
  x > 1 &&
  x < viewSize view &&
  y > 1 &&
  y < viewSize view &&
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

viewSize :: View -> Int
viewSize view =
  let positions = M.keys view
   in maximum $ fmap snd positions

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

inBounds :: View -> Position -> Bool
inBounds view (x, y) =
  x >= 0 && x <= viewSize view && y >= 0 && y <= viewSize view

isValid :: View -> Position -> Bool
isValid view position =
  inBounds view position && M.member position view && view ! position == Scafold

findVacuum :: View -> Position
findVacuum view =
  fst $ head $ filter (\(_, tile) -> (fromEnum tile > 1)) $ M.toList view

asOutput :: String -> [Int]
asOutput xs = fmap ord xs

type Move = (Tile, Int)

turn :: View -> Position -> Tile -> (Tile, Tile)
turn view (x, y) tile =
  let valid = isValid view
   in case tile of
        Up ->
          if valid (x + 1, y)
            then (Right, Right)
            else if valid (x - 1, y)
                   then (Left, Left)
                   else (Open, Open)
        Down ->
          if valid (x - 1, y)
            then (Right, Left)
            else if valid (x + 1, y)
                   then (Left, Right)
                   else (Open, Open)
        Right ->
          if valid (x, y + 1)
            then (Right, Down)
            else if valid (x, y - 1)
                   then (Left, Up)
                   else (Open, Open)
        Left ->
          if valid (x, y - 1)
            then (Right, Up)
            else if valid (x, y + 1)
                   then (Left, Down)
                   else (Open, Open)

turn' view position tile =
  trace
    ("turn : pos = " ++ (show position) ++ " ; tile = " ++ (show tile))
    turn
    view
    position
    tile

advance :: View -> Position -> Tile -> (Position, Bool)
advance view (x, y) direction =
  let next =
        case direction of
          Up    -> (x, y - 1)
          Right -> (x + 1, y)
          Down  -> (x, y + 1)
          Left  -> (x - 1, y)
   in if isValid view next
        then (next, True)
        else ((x, y), False)

vacuum :: View -> Position -> (Tile, Tile) -> [Move] -> [Move]
vacuum view start (move, direction) [] =
  let (position, success) = advance view start direction
   in if success
        then vacuum view position (move, direction) [(move, 1)]
        else let tile' = turn view position direction
              in vacuum view position tile' [(fst tile', 1)]
vacuum view start (move, direction) moves =
  let (position, success) = advance view start direction
      ((move, count):xs) = moves
   in if success
        then vacuum view position (move, direction) ((move, count + 1) : xs)
        else let (move', direction') = turn view position direction
              in if move' == Open
                   then moves
                   else vacuum
                          view
                          position
                          (move', direction')
                          ((move', 0) : moves)

moveToStr :: (Tile, Int) -> String
moveToStr (Right, count) = "R" ++ (show count)
moveToStr (Left, count)  = "L" ++ (show count)

arrayToAscii :: [Char] -> [Int]
arrayToAscii xs = (fmap ord xs) ++ [10]

main :: IO ()
main = do
  input <- readFile "day17.txt"
  let opcodes = getOpcodes input
  let memory = load opcodes
  let comp = boot memory
  -- let output = computerOutput $ run comp {computerState = Running}
  -- let view = asView output
  -- let intersections = filter (isIntersection view) $ M.toList view
  -- putStrLn $ drawOutput output
  -- print $ length intersections
  -- putStrLn $ drawView view
  -- let start = findVacuum view
  -- let move = (view ! start, view ! start)
  -- let moves = vacuum view start move []
  -- print $ concat $ fmap moveToStr $ reverse moves
  let main = "A,B,A,A,B,C,B,C,C,B"
  let a = "L,12,R,8,L,6,R,8,L,6"
  let b = "R,8,L,12,L,12,R,8"
  let c = "L,6,R,6,L,12"
  let d = "n"
  let input = concat $ fmap arrayToAscii [main, a, b, c, d]
  let comp' = patchMemory comp 0 2
  let output = computerOutput $ run comp' { 
    computerState = Running,
    computerInput = input
  }
  print $ output
