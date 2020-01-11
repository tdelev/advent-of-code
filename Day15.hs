module Day15 where

import           Data.Foldable
import           Data.List
import           Data.List.Split
import           Data.Map        as M hiding (drop, filter, foldl, take, toList)
import           Data.Sequence   as S hiding (filter, replicate, zip)
import           Debug.Trace
import           Intcode         hiding (main)

data Tile
  = Wall
  | Open
  | Target
  deriving (Eq, Show, Enum)

data Move
  = North
  | South
  | West
  | East
  deriving (Eq, Show, Enum)

asCommand :: Move -> Int
asCommand = (+ 1) . fromEnum

directions :: [Move]
directions = [North ..]

type Position = (Int, Int)

type Area = M.Map Position Tile

data Droid =
  Droid
    { droidPosition :: Position
    , droidComp     :: Computer
    , droidTarget   :: Bool
    , droidVisited  :: Int
    }
  deriving (Eq, Show)

type Queue = S.Seq SearchState

type SearchState = (Move, Droid)

emptyQueue :: Queue
emptyQueue = S.empty

isEmpty :: Queue -> Bool
isEmpty queue = S.null queue

enqueue :: SearchState -> Queue -> Queue
enqueue state queue = queue |> state

enqueueAll :: [SearchState] -> Queue -> Queue
enqueueAll moves queue = foldl (\q move -> enqueue move q) queue moves

pop :: Queue -> (Queue, SearchState)
pop queue =
  let list = toList queue
   in (S.fromList $ tail list, head list)

coverArea :: Position -> Tile -> Area -> Area
coverArea = M.insert

getPosition :: Move -> Position -> Position
getPosition move (x, y) =
  let (deltaX, deltaY) =
        case move of
          North -> (0, 1)
          South -> (0, -1)
          West  -> (-1, 0)
          East  -> (1, 0)
   in (x + deltaX, y + deltaY)

processStatus ::
     Move
  -> Int
  -> Position
  -> Droid
  -> Area
  -> Queue
  -> Computer
  -> (Droid, Area, Queue)
processStatus move status pos droid area queue comp =
  let position = getPosition move $ pos
      tile = toEnum status
      area' = coverArea position tile $ area
      visited = droidVisited droid
      droid' =
        droid
          { droidPosition = position
          , droidVisited = (visited + 1)
          , droidComp = comp
          }
      queue' = enqueueAll (zip directions (rep droid')) queue
   in case tile of
        Wall   -> (droid {droidComp = comp}, area', queue)
        Open   -> (droid', area', queue')
        Target -> (droid' {droidTarget = True}, area', queue')

rep :: Droid -> [Droid]
rep position = replicate 4 position

notVisited :: Area -> Move -> Position -> Bool
notVisited area move pos =
  let position = getPosition move $ pos
   in M.member position area == False

moveDroid :: Droid -> Area -> Queue -> Move -> Position -> (Droid, Area, Queue)
moveDroid droid area queue move position =
  let command = asCommand move
      comp = droidComp droid
      result =
        run
          comp
            { computerInput = [command]
            , computerState = Running
            , computerOutput = []
            }
      status = toEnum $ head $ computerOutput result
   in processStatus move status position droid area queue result

initDroid :: Computer -> Droid
initDroid comp =
  Droid
    { droidPosition = (0, 0)
    , droidComp = comp
    , droidTarget = False
    , droidVisited = 0
    }

initArea :: Area
initArea = coverArea (0, 0) Open M.empty

bfs :: Area -> Queue -> Droid -> (Droid, Area)
bfs area queue d =
  if isEmpty queue
    then (d, area)
    else let (queue', (move, droid)) = pop queue
             position = droidPosition droid
             (droid', area', queue'') =
               if notVisited area move position
                 then moveDroid droid area queue' move position
                 else (droid, area, queue')
             targetVisited = droidTarget droid'
          in if targetVisited
               then (droid', area')
               else bfs area' queue'' droid'

drawTile :: Area -> Position -> Char
drawTile area position =
  if position == (0, 0)
    then 'O'
    else if M.member position area
           then let tile = area ! position
                 in case tile of
                      Open   -> '.'
                      Wall   -> '#'
                      Target -> 'x'
           else ' '

drawArea :: Area -> String
drawArea area =
  let positions = M.keys area
      minx = minimum $ fmap fst positions
      maxx = maximum $ fmap fst positions
      miny = minimum $ fmap snd positions
      maxy = maximum $ fmap snd positions
      lines =
        [[(drawTile area (x, y)) | x <- [minx .. maxx]] | y <- [miny .. maxy]]
   in unlines lines

main :: IO ()
main = do
  input <- readFile "day15.txt"
  let opcodes = getOpcodes input
  let memory = load opcodes
  let comp = boot memory
  let droid = initDroid comp
  let startQueue = enqueueAll (zip directions (rep droid)) emptyQueue
  let (droid', area) = bfs initArea startQueue droid
  print $ droid'
  putStrLn $ drawArea area
