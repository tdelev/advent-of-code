module Day20 where

import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.List.Split
import           Data.Map        as M hiding (drop, filter, foldl, take)
import           Data.Maybe
import           Data.Sequence   as S hiding (filter, replicate, zip)
import           Debug.Trace

type Position = (Int, Int)

type Maze = M.Map Position Char

type Portals = M.Map Position Position

combine :: (Maze, Position) -> Char -> (Maze, Position)
combine (maze, (x, y)) c =
  if c == '\n'
    then (maze, (1, y + 1))
    else (M.insert (x, y) c maze, (x + 1, y))

initMaze :: String -> Maze
initMaze input = fst $ foldl combine (M.empty, (1, 1)) input

directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]

positions :: Position -> [(Position, Position)]
positions (x, y) =
  fmap (\(x', y') -> ((x' + x, y' + y), (x' + x' + x, y' + y' + y))) directions

isMarked :: Maze -> Position -> [Maybe Char]
isMarked maze position = fmap (checkPosition maze) $ positions position

findLetter :: Maze -> Char -> [Position]
findLetter maze letter =
  fmap fst $ filter (\c -> (snd c) == letter) $ M.toList maze

hasPassage :: Maze -> Position -> Bool
hasPassage maze position =
  any (== '.') $ fmap (\(p, _) -> maze ! p) $ positions position

findPortal :: Maze -> Char -> Position
findPortal maze letter =
  head $ filter (hasPassage maze) $ findLetter maze letter

checkPosition :: Maze -> (Position, Position) -> Maybe Char
checkPosition maze (p, p') =
  if isUpper $ maze ! p
    then Just $ maze ! p'
    else Nothing

checkPortal :: [Maybe Char] -> Maybe Char
checkPortal options =
  let filtered = filter isJust options
   in if Data.List.null filtered
        then Nothing
        else head filtered

isPortal :: Maze -> Position -> Maybe Char
isPortal maze position = checkPortal $ isMarked maze position

main :: IO ()
main = do
  input <- readFile "day20test.txt"
  let maze = initMaze input
  -- print $ maze
  print $ fmap (== 'A') $ isPortal maze (10, 3)
  -- print $ findPortal maze 'E'
