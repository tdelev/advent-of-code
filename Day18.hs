module Day18 where

import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.List.Split
import           Data.Map        as M hiding (drop, filter, foldl, null, take)
import           Data.Sequence   as S hiding (filter, replicate, zip)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Debug.Trace

type Position = (Int, Int)

type Maze = (M.Map Position Char, (Int, Int))

type Queue a = S.Seq a

emptyQueue :: Queue a
emptyQueue = S.empty

isEmpty :: Queue a -> Bool
isEmpty queue = S.null queue

enqueue :: a -> Queue a -> Queue a
enqueue a queue = queue |> a

enqueueAll :: [a] -> Queue a -> Queue a
enqueueAll xs queue = foldl (\q x -> enqueue x q) queue xs

pop :: Queue a -> (Queue a, a)
pop queue =
  let list = Data.Foldable.toList queue
   in (S.fromList $ tail list, head list)

type Visited = Set SearchState

type KeysDoors = Set Char

data SearchState =
  SS
    { ssPosition :: Position
    , ssKeys     :: KeysDoors
    }
  deriving (Eq, Show, Ord)

initSearchState :: Maze -> SearchState
initSearchState (maze, _) =
  let items = M.toList maze
      position = fst $ head $ filter (\(_, c) -> c == '@') items
      keys =
        Set.fromList $
        fmap snd $ filter (\(_, c) -> c /= '#' && c /= '.' && c /= '@') items
   in SS {ssPosition = position, ssKeys = keys}

combine :: (M.Map Position Char, Position) -> Char -> (M.Map Position Char, Position)
combine (maze, (x, y)) c =
  if c == '\n'
    then (maze, (1, y + 1))
    else (M.insert (x, y) c maze, (x + 1, y))

initMaze :: String -> Maze
initMaze input =
  let maze = fst $ foldl combine (M.empty, (1, 1)) input
      items = M.toList maze
      width = maximum $ fmap fst $ fmap fst items
      height = maximum $ fmap snd $ fmap fst items
   in (maze, (width, height))

open :: KeysDoors -> Char -> KeysDoors
open keys c = Set.delete (toUpper c) $ Set.delete c keys

visit :: Maze -> SearchState -> SearchState
visit (maze, _) state =
  let c = maze ! (ssPosition state)
   in if isLower c
        then state { ssKeys = open (ssKeys state) c}
        else state

isGoal :: SearchState -> Bool
isGoal state = Set.null $ ssKeys state

inBounds :: Maze -> Position -> Bool
inBounds (_, (width, height)) (x, y) =
  x >= 1 && x <= width && y >= 1 && y <= height

openDoor :: KeysDoors -> Char -> Bool
openDoor keys c =
  if isLower c
    then True
    else Set.notMember c keys

isValid :: KeysDoors -> Maze -> Position -> Bool
isValid keys maze position =
  let c = (fst maze) ! position
   in inBounds maze position && (c == '.' || c == '@' || openDoor keys c)

successors :: Maze -> (SearchState, Int) -> [(SearchState, Int)]
successors maze (state, count) =
  let (x, y) = ssPosition state
      keys = ssKeys state
      possible = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
      valid = filter (isValid keys maze) possible
   in fmap (\p -> (state { ssPosition = p}, count + 1)) valid

notVisited :: SearchState -> Visited -> Bool
notVisited = Set.notMember

bfs ::
     Maze
  -> Queue (SearchState, Int)
  -> Visited
  -> (SearchState, Int)
  -> (SearchState, Int)
bfs maze queue visited (state, count) =
  if isEmpty queue
    then (state, count)
    else let (queue', (state', count')) = pop queue
             state'' = visit maze state'
             visited' = Set.insert state'' visited
          in if isGoal state''
               then (state'', count')
               else let next = filter notVisited visited' $ successors (state'', count')
                        queue'' = enqueueAll next queue'
                     in bfs maze queue'' visited' (state'', count')

main :: IO ()
main = do
  input <- readFile "day18test.txt"
  let maze = initMaze input
  print $ maze
  let state = initSearchState maze
  print $ state
  print $ Set.member 'x' (ssKeys state)
