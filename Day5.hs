module Day5 where

import           Data.List.Split
import           Data.Map        as M

newtype Address =
  A
    { asValue :: Int
    }
  deriving (Eq, Ord, Show)

type Code = M.Map Address Int

data Instruction
  = Add
  | Mul
  | Input
  | Output
  | Halt
  deriving (Eq, Show)

data Access
  = Memory
  | Direct
  deriving (Enum, Show)

data Computer =
  Comp
    { pointer :: Address
    , code    :: Code
    , output  :: [Int]
    }

instance Show Computer where
  show (Comp pointer code output) =
    "Computer : code = " ++
    (show code) ++
    " ; pointer = " ++ (show pointer) ++ " output = " ++ (show output)

type Decoded = (Instruction, [Access], [Int], [Int])

toInt :: String -> Int
toInt s = read s

decode :: Int -> Instruction
decode 1  = Add
decode 2  = Mul
decode 3  = Input
decode 4  = Output
decode 99 = Halt

makeProgram :: [Int] -> Code
makeProgram xs = M.fromList $ zip addressList xs
  where
    addressList = fmap A [0 ..]

updateCode :: Code -> Address -> Int -> Code
updateCode code address value = M.insert address value code

readAddress :: Code -> Address -> Int
readAddress code address = code ! (A (code ! address))

readDirect :: Code -> Address -> Int
readDirect code address = code ! address

readDirectA :: Code -> Address -> Address
readDirectA code address = A $ readDirect code address

asAccess :: Int -> Access
asAccess = toEnum

getAccess :: Int -> [Access]
getAccess n = fmap (asAccess . fromEnum . odd) [n, n `div` 10, n `div` 100]

getInputOutput :: Instruction -> ([Int], [Int])
getInputOutput i =
  case i of
    Add    -> ([1, 2], [3])
    Mul    -> ([1, 2], [3])
    Input  -> ([], [1])
    Output -> ([1], [])
    Halt   -> ([], [])

decodeInstruction :: Int -> Decoded
decodeInstruction n =
  let instruction = decode $ n `mod` 100
      access = getAccess $ n `div` 100
      (input, output) = getInputOutput instruction
   in (instruction, access, input, output)

loadComputer :: Code -> Computer
loadComputer code = Comp (A 0) code []

getInstruction :: Computer -> Decoded
getInstruction (Comp pointer code _) = decodeInstruction $ code ! pointer

movePointer :: Address -> Int -> Address
movePointer a i = A ((asValue a) + i)

--executeInstruction :: Int -> Computer -> Computer
--executeInstruction input comp = let decoded = getInstruction comp
--                                let
exec :: Int -> Decoded -> Computer -> Computer
exec input (Input, access:_, _, _) (Comp pointer code output) =
  Comp
    (movePointer pointer 2)
    (updateCode code (readDirectA code $ movePointer pointer 1) input)
    []
exec input (Output, access:_, _, _) (Comp pointer code output) =
  Comp (movePointer pointer 2) code [readAddress code $ movePointer pointer 1]

isHalt :: Decoded -> Bool
isHalt (ins, _, _, _) =
  if ins == Halt
    then True
    else False

run :: Int -> Computer -> Computer
run input comp =
  let ins = getInstruction comp
   in if isHalt ins
        then comp
        else run input (exec input ins comp)

main :: IO ()
main = do
  input <- readFile "day5.txt"
  --let opcodes = fmap toInt $ splitOn "," input
    --let opcodes = [1,1,1,4,99,5,6,0,99]
  let opcodes = [3, 0, 4, 0, 99]
    --print $ makeProgram opcodes
  let code = makeProgram opcodes
  let comp = loadComputer code
  --let ins = getInstruction comp
  print $ run 12 comp
    --let final_code = init_code code 12 2
    --print $ run final_code 0
    --print $ noun_verb code 19690720
