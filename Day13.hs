module Day13 where

import           Data.List
import           Data.List.Split
import           Data.Map        as M hiding (drop, take)
import           Day5            hiding (main)
import           Debug.Trace

countBlack :: [Int] -> Int
countBlack [] = 0
countBlack (_:_:x:xs) =
  if x == 2
    then 1 + (countBlack xs)
    else countBlack xs

main :: IO ()
main = do
  input <- readFile "day13.txt"
  let opcodes = getOpcodes input
  --let opcodes = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
  let code = makeProgram opcodes
  let comp = loadComputer code
  let result = run [] comp
  print $ countBlack $ output result
