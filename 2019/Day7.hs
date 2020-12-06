module Day7 where

import           Data.List
import           Data.List.Split
import           Day5            hiding (main)
import           Debug.Trace

combineInput :: [Int] -> Int -> [Int]
combineInput [] y    = [y]
combineInput (x:_) y = [x, y]

hasOutput :: Computer -> Bool
hasOutput comp = (length $ output comp) > 0

testSequence :: [Int] -> Int -> Computer -> Int
testSequence (a:[]) b comp = head $ output $ run [a, b] comp
testSequence (a:xs) b comp =
  let input = head $ output $ run [a, b] comp
   in testSequence xs input comp

willHalt :: [Int] -> Computer -> (Computer, Bool)
willHalt input comp =
  let (resComp, resInput, resHalt, waitOnInput) = execInstruction input comp
   in if waitOnInput
        then (resComp, False)
        else if resHalt
               then (resComp, True)
               else willHalt resInput resComp

willHalt' input comp = trace ("will halt : " ++ (show comp) ++ (" ; input = " ++ (show input))) willHalt input comp               

testSequence' :: [Int] -> Int -> [(Int, Computer)] -> Int
testSequence' input x amp =
  let combined = combineInput input x
      (index, comp) = head amp
      (result, resInput, isHalt, waitOnInput) = runToOutputOrHalt combined comp
      (haltResult, willHaltEventually) = if index == 5 then willHalt resInput result else (comp, False)
      nextInput =
        if null input
          then []
          else tail input
   in if isHalt
        then head $ output result
        else if willHaltEventually then head $ output result
        else testSequence'
               nextInput
               (head $ output result)
               (tail amp ++ [(index, result)])

testSequence'' input x amp =
  trace
    ("test input = " ++ (show input) ++ " ; x = " ++ (show x) ++ " ; comp = " ++ (show $ fst $ head amp))
    (testSequence' input x amp)

explode :: [Int] -> [[Int]]
explode xs = fmap (\x -> [x]) xs
  --let stepResult = foldl (\inputxs -> )
   --in stepResult

runToOutputOrHalt :: [Int] -> Computer -> (Computer, [Int], Bool, Bool)
runToOutputOrHalt input comp =
  let (resComp, resInput, resHalt, waitOnInput) = execInstruction input comp
   in if resHalt
        then (resComp, resInput, True, waitOnInput)
        else if hasOutput resComp
               then (resComp, resInput, False, waitOnInput)
               else runToOutputOrHalt resInput resComp

runToOutputOrHalt' input comp =
  trace
    ("run to halt = " ++ (show input) ++ (" ; comp = " ++ (show comp)))
    runToOutputOrHalt
    input
    comp

findMax :: Computer -> [Int] -> Int
findMax comp phases =
  let all = permutations phases
   in maximum $ fmap (\x -> testSequence x 0 comp) all


findMax' :: [(Int,Computer)] -> [Int] -> Int
findMax' amp phases =
  let all = permutations phases
   in maximum $ fmap (\x -> testSequence' x 0 amp) all

main :: IO ()
main = do
  input <- readFile "day7.txt"
  let opcodes = getOpcodes input
  --let opcodes = []
  let code = makeProgram opcodes
  let comp = loadComputer code
  let amp = zip [1..5] (repeat comp)
  --print ins
  print $ findMax' amp [5..9]
