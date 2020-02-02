module Day19 where

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
import           Intcode              hiding (main)

type Position = (Int, Int)

type BeamMap = M.Map Position Int

combine :: BeamMap -> (Computer, Position) -> BeamMap
combine bmap (comp, (x, y)) = M.insert (x, y) out bmap
  where
    out = explorePos comp (x, y)

explorePos :: Computer -> Position -> Int
explorePos comp (x, y) =
  head $
  computerOutput $ run comp {computerState = Running, computerInput = [x, y]}

explore :: Computer -> Int -> Int -> BeamMap
explore comp maxX maxY = foldl combine M.empty xs
  where
    xy = (,) <$> [0 .. maxX] <*> [0 .. maxY]
    xs = fmap (\p -> (comp, p)) xy

findX :: Computer -> Int -> Int -> Int
findX comp y offset = find comp (0 + offset) y 10000
  where
    find comp 10000 y limit = (-1)
    find comp x y limit =
      let out = explorePos comp (x, y)
       in if out == 1
            then x
            else find comp (x + 1) y limit

checkSquare :: Computer -> Int -> Int -> Bool
checkSquare comp x y =
  let a = explorePos comp (x, y - 99)
      b = explorePos comp (x + 99, y - 99)
      c = explorePos comp (x + 99, y)
   in a == 1 && b == 1 && c == 1

-- firstSquare :: Computer -> Int -> Int
-- firstSquare comp start =
--   let a = findY comp start
--       b = explorePos comp (start, a + 99)
--    in if b == 1
--         then start
--         else firstSquare comp (start + 1)

drawTile :: Int -> Char
drawTile 0 = '.'
drawTile 1 = '#'

draw :: BeamMap -> String
draw bmap =
  let positions = M.keys bmap
      minx = minimum $ fmap fst positions
      maxx = maximum $ fmap fst positions
      miny = minimum $ fmap snd positions
      maxy = maximum $ fmap snd positions
      lines =
        [ [(drawTile $ bmap ! (y, x)) | x <- [minx .. maxx]]
        | y <- [miny .. maxy]
        ]
   in unlines lines

-- findInRange :: Computer -> Int -> Int -> Int
-- findInRanage comp low hi =
--   let mid = low + ((hi - low) `div` 2)
--       x = findX comp mid (mid `div` 2)
--       isSquare = checkSquare comp x mid
--     in if isSquare then mid
--     else 


main :: IO ()
main = do
  input <- readFile "day19.txt"
  let opcodes = getOpcodes input
  let memory = load opcodes
  let comp = boot memory
  -- Part 1
  let beamMap = explore comp 49 49
  -- let count = sum $ fmap snd $ M.toList beamMap
  -- print $ count
  putStrLn $ draw beamMap
  -- Part 2
  -- print $ firstSquare comp 810
  -- print $ findX comp 2169 1100
  -- print $ checkSquare comp 1734 2169
  print $ explorePos comp (0, 0)
  print $ explorePos comp (945, 1283)
  print $ explorePos comp (1283, 945)
  print $ explorePos comp (944, 1282)
  print $ explorePos comp (1282, 943)
  -- print $ explorePos comp (1734, 2071)
  -- print $ explorePos comp (1833, 2070)
  -- print $ explorePos comp (1734, 2166)
  -- print $ explorePos comp (1734, 2167)
  -- print $ explorePos comp (1734, 2168)
  -- print $ explorePos comp (1734, 2169)
  -- print $ explorePos comp (1833, 2169)
