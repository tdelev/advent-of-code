module Day18 where

import           Data.Char
import           Data.Foldable
import qualified Data.Heap            as DH
import           Data.List
import           Data.List.Split
import           Data.Map             as M hiding (drop, filter, foldl, null,
                                            take)
import qualified Data.MemoCombinators as Memo
import           Data.Sequence        as S hiding (filter, replicate, zip)
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Debug.Trace

type Position = (Int, Int)

type Maze = (M.Map Position Char, (Int, Int))

type Queue a = S.Seq a

type PQueue a = DH.MinPrioHeap Int a

emptyQueue :: Queue a
emptyQueue = S.empty

emptyHeap :: PQueue a
emptyHeap = DH.empty

isEmpty :: Queue a -> Bool
isEmpty queue = S.null queue

isEmpty' :: PQueue a -> Bool
isEmpty' queue = DH.null queue

enqueue :: a -> Queue a -> Queue a
enqueue a queue = queue |> a

enqueue' :: (Int, a) -> PQueue a -> PQueue a
enqueue' (p, a) queue = DH.insert (p, a) queue

enqueueAll :: [a] -> Queue a -> Queue a
enqueueAll xs queue = foldl (\q x -> enqueue x q) queue xs

enqueueAll' :: [(Int, a)] -> PQueue a -> PQueue a
enqueueAll' xs queue = foldl (\q x -> enqueue' x q) queue xs

pop :: Queue a -> (Queue a, a)
pop queue =
  let list = Data.Foldable.toList queue
   in (S.fromList $ tail list, head list)

pop' :: PQueue a -> (PQueue a, a)
pop' queue = (DH.drop 1 queue, snd $ head $ DH.take 1 queue)

type Visited = Set SearchState

type KeysDoors = Set Char

-- type Optimal = M.Map SearchState Int
-- isOptimal :: SearchState -> Int -> Optimal -> Bool
-- isOptimal state count optimal =
--   if M.notMember position optimal
--     then True
--     else m <= count
--   where
--     m = optimal ! position
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

combine ::
     (M.Map Position Char, Position) -> Char -> (M.Map Position Char, Position)
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
        then state {ssKeys = open (ssKeys state) c}
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
    else if isUpper c
           then Set.notMember c keys
           else False

isValid :: KeysDoors -> Maze -> Position -> Bool
isValid keys maze position =
  let c = (fst maze) ! position
   in inBounds maze position && (c == '.' || c == '@' || openDoor keys c)

successors :: Maze -> (SearchState, Int) -> [(SearchState, Int, Int)]
successors maze (state, count) =
  let (x, y) = ssPosition state
      keys = ssKeys state
      possible = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
      valid = filter (isValid keys maze) possible
   in fmap
        (\p ->
           ( state {ssPosition = p}
           , count + 1
           , (count * 100) + (Set.size $ keys)))
        valid

successors' :: Maze -> (Position, Int) -> [(Position, Int)]
successors' maze (position, count) =
  let (x, y) = position
      possible = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
      valid = inBounds maze
   in fmap (\p -> (p, count + 1)) $ filter valid possible

notVisited :: SearchState -> Visited -> Bool
notVisited = Set.notMember

combine1 ::
     Maze
  -> (Visited, [(SearchState, Int)])
  -> (SearchState, Int, Int)
  -> (Visited, [(SearchState, Int)])
combine1 maze (visited, acc) (xstate, xcount, _) =
  if notVisited xstate visited
    then (visited', acc ++ [(state', xcount)])
    else (visited, acc)
  where
    state' = visit maze xstate
    visited' = Set.insert state' visited

bfs ::
     Maze
  -> Queue (SearchState, Int)
  -> Visited
  -- -> Optimal
  -> (SearchState, Int) --, PQueue (SearchState, Int))
  -> (SearchState, Int) --, PQueue (SearchState, Int))
bfs maze queue visited (state, count) =
  if isEmpty queue
    then (state, count) --, queue)
    else let (queue', (state', count')) = pop queue
            --  state'' = visit maze state'
            --  optimal' = M.insert (ssPosition state') count' optimal
            --  visited' = Set.insert state'' visited
          in if isGoal state' -- || count' > 40
               then (state', count') --, queue')
               else let (visited', next) =
                          foldl (combine1 maze) (visited, []) $
                          successors maze (state', count')
                        queue'' = enqueueAll next queue'
                     in bfs maze queue'' visited' (state', count') --, queue'')

bfs' m q visited (state, count) =
  trace
    ("bfs : q = " ++ (show q)) -- ++ " ; visited = " ++ (show visited))
    bfs
    m
    q
    visited
    (state, count)

isChar :: Char -> Maze -> Position -> Bool
isChar c (maze, _) pos = maze ! pos == c

isPath = isChar '.'

isStart = isChar '@'

isKeyOrDoor :: (Char -> Bool) -> Maze -> Position -> Bool
isKeyOrDoor f (maze, _) pos = f $ maze ! pos

isKey = isKeyOrDoor isLower

isDoor = isKeyOrDoor isUpper

getKey :: Maze -> Position -> Char
getKey (maze, _) position = maze ! position

openedDoor :: Maze -> Position -> Keys -> Bool
openedDoor maze position keys = elem (toLower key) keys
  where
    key = getKey maze position

canExplore :: Maze -> Position -> Keys -> Set Position -> Bool
canExplore maze position keys visited =
  Set.notMember position visited &&
  (isKey maze position ||
   isPath maze position ||
   openedDoor maze position keys || isStart maze position)

dfs ::
     Maze
  -> (Position, Int)
  -> Keys
  -> Set Position
  -> [(Char, (Position, Int))]
dfs maze (position, count) keys visited =
  if isKey maze position && not (elem (getKey maze position) keys)
    then [(getKey maze position, (position, count))]
    else let next =
               filter (\(p, _) -> canExplore maze p keys visited) $
               successors' maze (position, count)
             visited' = foldl (\v (p, _) -> Set.insert p v) visited next
          in if Data.List.null next
               then []
               else concat $ fmap (\p -> dfs maze p keys visited') next

type Keys = [Char]

shortestPath :: Maze -> Int -> Int -> Keys -> Int
shortestPath maze x y keys =
  let reachableKeys = dfs' maze ((x, y), 0) keys Set.empty
   in if Data.List.null reachableKeys
        then 0
        else minimum $
             fmap
               (\(key, ((x', y'), distance)) ->
                  distance + (sp' maze x' y' (Data.List.sort (keys ++ [key]))))
               reachableKeys

csum a b = a + b

csum' a b = trace ("csum : a = " ++ (show a) ++ " ; b = " ++ (show b)) csum a b

testMemo :: Int -> Int -> Int
testMemo a b =
  if a + b > 100
    then 1
    else csum' (testM a (a + b)) (testM (a + b) a)

testM = Memo.memo2 Memo.integral Memo.integral testMemo

sp' maze =
  Memo.memo3
    Memo.integral
    Memo.integral
    (Memo.list Memo.char)
    (shortestPath maze)

dfs' maze (p, c) keys visited =
  trace
    ("dfs: p = " ++ (show p) ++ " ; keys = " ++ (show keys))
    dfs
    maze
    (p, c)
    keys
    visited

main :: IO ()
main = do
  input <- readFile "day18.txt"
  let maze = initMaze input
  -- Part 1
  let state = initSearchState maze
  let queue = enqueue (state, 0) emptyQueue
  print $ bfs maze queue Set.empty (state, 0)
