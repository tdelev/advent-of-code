module Day7 where

import           Data.List
import           Data.List.Split
import           Day5            hiding (main)

testSequence :: [Int] -> Int -> Computer -> Int
testSequence (a:[]) b comp = head $ output $ run [a, b] comp
testSequence (a:xs) b comp =
  let input = head $ output $ run [a, b] comp
   in testSequence xs input comp

findMax :: Computer -> Int
findMax comp =
  let all = permutations [0 .. 4]
   in maximum $ fmap (\x -> testSequence x 0 comp) all

main :: IO ()
main = do
  input <- readFile "day7.txt"
  let opcodes = getOpcodes input
  --let opcodes = [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0]
  let code = makeProgram opcodes
  let comp = loadComputer code
  --print ins
  print $ findMax comp
