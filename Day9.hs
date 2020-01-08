module Day9 where

import           Data.List
import           Data.List.Split
import           Debug.Trace
import           Intcode         hiding (main)

main :: IO ()
main = do
  input <- readFile "day9.txt"
  let opcodes = getOpcodes input
  --let opcodes = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
  let memory = load opcodes
  let comp = boot memory
  print $ run comp {computerInput = [2], computerState = Running}
