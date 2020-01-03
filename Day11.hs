module Day11 where

import           Data.List
import           Data.List.Split
import           Day5            hiding (main)
import           Debug.Trace

main :: IO ()
main = do
  input <- readFile "day11.txt"
  let opcodes = getOpcodes input
  --let opcodes = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
  let code = makeProgram opcodes
  let comp = loadComputer code
  --let (n, _, _, _) = execInstruction [] comp
  --print $ n
  print $ run [2] comp

