module Day9 where

import           Data.List
import           Data.List.Split
import           Day5            hiding (main)
import           Debug.Trace

main :: IO ()
main = do
  input <- readFile "day9.txt"
  let opcodes = getOpcodes input
  --let opcodes = [1102,34915192,34915192,7,4,7,99,0]
  let code = makeProgram opcodes
  let comp = loadComputer code
  --let (n, _, _, _) = execInstruction [] comp
  --print $ n
  print $ run [1] comp

