module Day15 where

import           Data.Foldable
import           Data.List
import           Data.List.Split
import           Data.Map        as M hiding (drop, filter, foldl, take)
import           Data.Sequence   as S hiding (filter, replicate, zip)
import           Debug.Trace
import           Intcode         hiding (main)

data Tile
  = Wall
  | Open
  | Target
  | Oxigen
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

type Queue a = S.Seq a

type SearchState = (Move, Droid)

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
  -> Queue SearchState
  -> Computer
  -> (Droid, Area, Queue SearchState)
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

moveDroid ::
     Droid
  -> Area
  -> Queue SearchState
  -> Move
  -> Position
  -> (Droid, Area, Queue SearchState)
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

bfs :: Area -> Queue SearchState -> Droid -> (Droid, Area)
bfs area queue d =
  if isEmpty queue
    then (d, area)
    else let (queue', (move, droid)) = pop queue
             position = droidPosition droid
             (droid', area', queue'') =
               if notVisited area move position
                 then moveDroid droid area queue' move position
                 else (droid, area, queue')
             targetVisited = False -- droidTarget droid'
          in if targetVisited
               then (droid', area')
               else bfs area' queue'' droid'

type FillState = (Position, Int)

canMove :: Area -> Position -> Bool
canMove area position =
  let exist = M.member position area
      isOpen = area ! position == Open
   in exist && isOpen

bfsFill :: Area -> Queue FillState -> Int -> (Area, Int)
bfsFill area queue minute =
  if isEmpty queue
    then (area, minute)
    else let (queue', (position, minute')) = pop queue
             area' = coverArea position Oxigen area
             next = filter (canMove area') $ nextPositions position
             toQueue = fmap (\x -> (x, minute' + 1)) next
             queue'' = enqueueAll toQueue queue'
          in bfsFill area' queue'' minute'

nextPositions :: Position -> [Position]
nextPositions position = fmap (\move -> getPosition move position) directions

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
                      Oxigen -> 'o'
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

findStart :: Area -> Position
findStart area = fst $ head $ filter (\(key, value) -> value == Target) $ (M.toList area)

main :: IO ()
main = do
  input <- readFile "day15.txt"
  let opcodes = getOpcodes input
  let memory = load opcodes
  let comp = boot memory
  let droid = initDroid comp
  let startQueue = enqueueAll (zip directions (rep droid)) emptyQueue
  let (droid', area) = bfs initArea startQueue droid
  --print $ droid'
  --putStrLn $ drawArea area
  let start = findStart area
  let startQueue = enqueue (start, 0) emptyQueue
  let result = bfsFill area startQueue 0
  putStrLn $ drawArea $ fst result
  print $ snd result
