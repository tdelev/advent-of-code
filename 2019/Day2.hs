module Day2 where

import           Data.List.Split
import           Data.Map        as M

type Code = M.Map Int Int

toInt :: String -> Int
toInt s = read s

makeProgram :: [Int] -> Code
makeProgram xs = M.fromList $ zip [0 ..] xs

updateCode :: Code -> Int -> Int -> Code
updateCode code position value = M.insert position value code

run :: Code -> Int -> Int
run code (-1) = code ! 0
run code n =
  let step = run_step code n
   in run (fst step) (snd step)

run_step :: Code -> Int -> (Code, Int)
run_step code cursor = run_operation code (code ! cursor) cursor

run_operation :: Code -> Int -> Int -> (Code, Int)
run_operation code opcode cursor
  | opcode <= 2 =
    let cvalue = value code
        left = cvalue $ cursor + 1
        right = cvalue $ cursor + 2
        position = code ! (cursor + 3)
        result =
          if opcode == 1
            then left + right
            else left * right
     in (updateCode code position result, cursor + 4)
  | otherwise = (code, (-1))

value :: Code -> Int -> Int
value code position = code ! (code ! position)

init_code :: Code -> Int -> Int -> Code
init_code code noun verb = Day2.updateCode (Day2.updateCode code 1 noun) 2 verb

noun_verb :: Code -> Int -> Int
noun_verb code expected =
  let pairs = (,) <$> [0..99] <*> [0..99] --[(x, y) | x <- [0 .. 99], y <- [0 .. 99]]
   in find_nv code pairs expected

find_nv :: Code -> [(Int, Int)] -> Int -> Int
find_nv code ((noun, verb):xs) expected =
  let result = run (init_code code noun verb) 0
   in if result == expected
        then 100 * noun + verb
        else find_nv code xs expected

main :: IO ()
main = do
  input <- readFile "day2.txt"
  let opcodes = fmap toInt $ splitOn "," input
    --let opcodes = [1,1,1,4,99,5,6,0,99]
    --print $ makeProgram opcodes
  let code = makeProgram opcodes
    --print $ code
  --let final_code = init_code code 12 2
  --print $ run final_code 0
  print $ noun_verb code 19690720
