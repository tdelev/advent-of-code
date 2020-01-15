module Day17 where

import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.List.Split
import           Data.Map        as M hiding (drop, filter, foldl, null, take)
import           Data.Sequence   as S hiding (filter, length, null, replicate,
                                       zip, zipWith)
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

isPosIntersection :: View -> Position -> Bool
isPosIntersection view position = isIntersection view (position, Scafold)

isCrossroad :: View -> Position -> Bool
isCrossroad view position =
  let possible = explorePosition view position
   in (length $ filter (\p -> view ! p == Scafold) possible) > 2

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

type Visited = Set Position

type IntersectionEntered = Set (Position, Position)

type SearchState = (Visited, [Position], [Position])

notVisited :: SearchState -> View -> Position -> Bool
notVisited (visited, (current:_), _) view position = Set.notMember position visited

notVisited' state view position =
  trace
    ("not visited : \nstate = " ++
     (show state) ++ "\nposition = " ++ (show position))
    notVisited
    state
    view
    position

inBounds :: View -> Position -> Bool
inBounds view (x, y) =
  x >= 0 && x <= viewSize view && y >= 0 && y <= viewSize view

isValid :: View -> Position -> Bool
isValid view position =
  inBounds view position && M.member position view && view ! position == Scafold

plus :: Position -> Position -> Position
plus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

explorePosition :: View -> Position -> [Position]
explorePosition view position =
  let filterFn = isValid view
   in filter filterFn $ zipWith plus xyDelta $ replicate 4 position

explorePosition' view position = trace ("explore position : pos = " ++ (show position)) explorePosition view position

findVacuum :: View -> Position
findVacuum view =
  fst $ head $ filter (\(_, tile) -> (fromEnum tile > 1)) $ M.toList view

updateState :: SearchState -> View -> Position -> SearchState
updateState (visited, current, intersection) view position =
  (visited', position : current, intersection')
  where
    visited' = Set.insert position visited
    intersection' =
      if isCrossroad view position
        then position:intersection
        else intersection

findPaths :: View -> SearchState -> Set Position -> [[Position]]
findPaths view state dust =
  let (visited, current, intersection) = state
      position = head current
      -- next = explorePosition view position
      next = filter (notVisited state view) $ explorePosition view position
   in if null next
        then if isComplete current dust
               then [current]
               else if null intersection
                      then []
                      else findPaths
                             view
                             (visited, (head intersection) : current, tail intersection)
                             dust
        else let states = fmap (updateState state view) next
              in concat $ fmap (\s -> findPaths view s dust) states

asOutput :: String -> [Int]
asOutput xs = fmap ord xs

findDust :: View -> Set Position
findDust view =
  let dust = fmap fst $ filter (\(p, tile) -> tile == Scafold) $ M.toList view
   in foldl (\s d -> Set.insert d s) Set.empty dust

isComplete :: [Position] -> Set Position -> Bool
isComplete [] set     = Set.null set
isComplete (x:xs) set = isComplete xs $ Set.delete x set

isComplete' p set = trace ("is complete : \np = " ++ (show p)) isComplete p set

main :: IO ()
main
  -- input <- readFile "day17.txt"
  -- let opcodes = getOpcodes input
  -- let memory = load opcodes
  -- let comp = boot memory
  -- let output = computerOutput $ run comp {computerState = Running}
  -- print $ output
 = do
  input <- readFile "day17test.txt"
  let output = asOutput input
  let view = asView output
  let intersections = filter (isIntersection view) $ M.toList view
  -- putStrLn $ drawOutput output
  print $ length intersections
  putStrLn $ drawView view
  let dust = findDust view
  print $ dust --fmap (isCrossroad view) $ Set.toList dust
  -- print $ isComplete [(13,8),(13,7),(13,6),(13,5),(12,5),(11,5),(11,4),(11,3),(11,2),(11,1),(12,1),(13,1),(14,1),(15,1),(15,2),(15,3),(15,4),(15,5),(15,6),(15,7),(15,8),(15,9),(14,9),(13,9),(13,10),(13,11),(12,11),(11,11),(10,11),(9,11),(9,10),(9,9),(8,9),(7,9),(7,8),(7,7),(6,7),(5,7),(4,7),(3,7),(2,7),(1,7)] dust
  -- print $ sumAlignment $ fmap fst intersections
  let start = findVacuum view
  let first = head $ explorePosition view start
  print $ first
  print $ dust
  let state = (Set.empty, [first], [])
  -- print $ explorePosition view (9, 8)
  print $ head $ findPaths view state dust
