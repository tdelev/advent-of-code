module Day13 where

import           Data.List
import           Data.List.Split
import           Data.Map        as M hiding (drop, filter, take)
import           Debug.Trace
import           IntComp         hiding (main)

data Tile
  = Empty
  | Wall
  | Block
  | Paddle
  | Ball
  deriving (Eq, Show, Enum)

type Position = (Int, Int)

type Screen = M.Map Position Tile

data GameState =
  GS
    { screen :: Screen
    , score  :: Int
    , ball   :: Int
    , paddle :: Int
    }
  deriving (Show)

toScreen :: [Int] -> Int -> Screen -> (Screen, Int)
toScreen [] score screen = (screen, score)
toScreen (-1:0:score:xs) _ screen = toScreen xs score screen
toScreen (x:y:tile:xs) score screen =
  let result = M.insert (x, y) (toEnum tile) screen
   in toScreen xs score result

updateScreen :: (Int, Int, Int) -> (Screen, Int) -> (Screen, Int)
updateScreen (-1, 0, score) (screen, _) = (screen, score)
updateScreen (x, y, tile) (screen, score) =
  (M.insert (x, y) (toEnum tile) screen, score)

toScreen' xs score screen =
  trace ("to screen : \nin = " ++ (show xs)) toScreen xs score screen

countBlack :: [Int] -> Int
countBlack [] = 0
countBlack (_:_:x:xs) =
  if x == 2
    then 1 + (countBlack xs)
    else countBlack xs

width = 21

height = 36

total = (width + 1) * (height * 1)

drawTile :: Tile -> Char
drawTile tile =
  case tile of
    Empty  -> ' '
    Wall   -> '#'
    Block  -> '*'
    Paddle -> '='
    Ball   -> 'o'

drawLine :: Int -> Screen -> String
drawLine y screen =
  let line = fmap (\x -> drawTile $ screen ! (x, y)) [0 .. height]
   in line ++ "\n"

findItemX :: Tile -> Screen -> Int
findItemX item screen =
  fst $ fst $ head $ filter (\(key, val) -> val == item) $ M.toList screen

findBallX = findItemX Ball

findPaddleX = findItemX Paddle

drawScreen :: (Screen, Int) -> String
drawScreen (screen, score) =
  let game = concat $ fmap (\y -> drawLine y screen) [0 .. width]
   in game ++ "\nScore = " ++ (show score)

nextMove :: Int -> Int -> Int
nextMove a b
  | a == b = 0
  | a < b = 1
  | otherwise = -1

nextMove' a b =
  trace ("next move : a = " ++ (show a) ++ " ; b = " ++ (show b)) nextMove' a b

gameHistory :: [Int] -> (Screen, Int) -> Bool -> String
gameHistory xs _ True =
  let initSize = (width + 1) * (height + 1) * 3
      screenScore = toScreen (take initSize xs) 0 M.empty
   in (drawScreen screenScore) ++
      "\n" ++ (gameHistory (drop initSize xs) screenScore False)
gameHistory (x:y:tile:xs) screenScore False =
  let updated = updateScreen (x, y, tile) screenScore
   in (drawScreen updated) ++ "\n" ++ (gameHistory xs updated False)
gameHistory [] screen False = ""

playGame :: GameState -> Computer -> Int -> (Computer, Int)
playGame gs comp offset =
  let move = nextMove (paddle gs) (ball gs)
      next = run comp {input = [move], state = Running}
      stateResult = state next
      outputResult = output next
      --nextOffset = length $ outputResult
      -- outputToProcess = drop offset outputResult
      (screenResult, scoreResult) =
        toScreen outputResult (score gs) (screen gs)
      nextGS =
        gs
          { screen = screenResult
          , score = scoreResult
          , ball = findBallX screenResult
          , paddle = findPaddleX screenResult
          }
   in if stateResult == Halted  -- || offset > 20000
        then (comp, scoreResult)
        else playGame nextGS next { output = [] } 0

playGame' gs comp offset =
  trace
    ("play game : offset = " ++
     (show offset) ++
     " ; p = " ++ (show $ paddle gs) ++ " ; b = " ++ (show $ ball gs))
    playGame
    gs
    comp
    offset

main :: IO ()
main = do
  input <- readFile "day13.txt"
  let opcodes = getOpcodes input
  let memory = load opcodes
  let comp = boot memory
  let started = patchMemory comp 0 2
  let initGame = run started {state = Running}
  --let next = run initGame {input = [-1], state = Running}
  let (screen, score) = toScreen (output initGame) 0 M.empty
  let game =
        GS
          { screen = screen
          , score = score
          , ball = findBallX screen
          , paddle = findPaddleX screen
          }
  print $ playGame game initGame (length $ output initGame)
