module Day11 where

import           Prelude         hiding (Left, Right)

import           Data.List
import           Data.List.Split
import           Data.Map        as M hiding (drop, take)
import           Day5            hiding (main)
import           Debug.Trace

data Direction
  = Up
  | Down
  | Right
  | Left
  deriving (Enum, Show)

data Color
  = Black
  | White
  deriving (Enum, Show)

type Position = (Int, Int)

type Grid = M.Map Position Color

data Robot =
  R
    { position  :: Position
    , direction :: Direction
    , comp      :: Computer
    }
  deriving (Show)

getColor :: Grid -> Robot -> Color
getColor grid (R position _ _) = findWithDefault Black position grid

updateComputer :: Robot -> Computer -> Robot
updateComputer (R position direction comp) computer =
  (R position direction computer)

getInput :: Grid -> Robot -> [Int]
getInput grid robot = [fromEnum $ getColor grid robot]

paintColor :: Position -> Int -> Grid -> Grid
paintColor pos color = M.insert pos (toEnum color)

moveRobot :: Robot -> Int -> Robot
moveRobot robot 0 =
  case robot of
    (R (x, y) Up comp)    -> (R (x - 1, y) Left comp)
    (R (x, y) Right comp) -> (R (x, y + 1) Up comp)
    (R (x, y) Down comp)  -> (R (x + 1, y) Right comp)
    (R (x, y) Left comp)  -> (R (x, y - 1) Down comp)
moveRobot robot 1 =
  case robot of
    (R (x, y) Up comp)    -> (R (x + 1, y) Right comp)
    (R (x, y) Right comp) -> (R (x, y - 1) Down comp)
    (R (x, y) Down comp)  -> (R (x - 1, y) Left comp)
    (R (x, y) Left comp)  -> (R (x, y + 1) Up comp)

start :: Computer -> (Grid, Robot)
start comp = (M.empty, R (0, 0) Up comp)

processOutput :: Int -> (Grid, Robot, [Int]) -> (Grid, Robot, [Int], Int)
processOutput offset (grid, robot, currentInput) =
  let toProcess = drop offset $ output $ comp robot
      robotPosition = position robot
      canProcess = (length toProcess) >= 2
   in if canProcess
        then let [color, move] = take 2 toProcess
                 nextGrid = paintColor robotPosition color grid
                 nextRobot = moveRobot robot move
                 input = getInput nextGrid nextRobot
              in processOutput
                   (offset + 2)
                   (nextGrid, nextRobot, currentInput ++ input)
        else (grid, robot, currentInput, offset)

processOutput' offset (grid, robot, currentInput) =
  trace
    ("process output : \noffset = " ++
     (show offset) ++ ("\ncurrentInput = " ++ (show currentInput)))
    processOutput
    offset
    (grid, robot, currentInput)

paint :: (Grid, Robot, [Int], Int) -> Grid
paint (grid, robot, computerInput, outputOffset) =
  let (result, nextInput, isHalt, waitOnInput) =
        execInstruction computerInput $ comp robot
      (nextGrid, nextRobot, inputToProcess, nextOffset) =
        processOutput outputOffset (grid, robot, nextInput)
   in if isHalt -- || outputOffset > 40000
        then nextGrid
        else paint
               ( nextGrid
               , updateComputer nextRobot result
               , inputToProcess
               , nextOffset)

paint' (grid, robot, input, offset) =
  trace
    ("paint : \ngrid = " ++
     (show grid) ++
     "\nrobot = " ++
     (show robot) ++
     "\ninput = " ++ (show input) ++ "\noffset = " ++ (show offset))
    paint
    (grid, robot, input, offset)

toString 0 = '.'
toString 1 = '#'

drawLine :: Int -> Int -> Grid -> String
drawLine y n grid = let line = fmap (\x -> toString $ fromEnum $ findWithDefault Black (x, y) grid) [(-n)..n]
 in line ++ "\n"

drawGrid :: Grid -> Int -> String
drawGrid grid n = concat $ fmap (\y -> drawLine y n grid) [(-n)..n] 
  
      
main :: IO ()
main = do
  input <- readFile "day11.txt"
  let opcodes = getOpcodes input
  --let opcodes = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
  let code = makeProgram opcodes
  let comp = loadComputer code
  let (grid, robot) = start comp
  let painted = paint (grid, robot, [1], 0)
  putStrLn $ drawGrid painted 80
