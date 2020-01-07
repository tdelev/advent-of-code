module IntComp where

import           Data.List.Split
import           Data.Map        as M
import           Debug.Trace

newtype Address =
  A
    { asValue :: Int
    }
  deriving (Eq, Ord, Show)

type Memory = M.Map Address Int

data Instruction
  = Add
  | Mul
  | Input
  | Output
  | JumpTrue
  | JumpFalse
  | LessThen
  | Equals
  | AdjustBase
  | Halt
  deriving (Eq, Show)

data Access
  = Memory
  | Direct
  | Relative
  deriving (Enum, Show, Eq)

data State
  = Running
  | Wait
  | Halted
  deriving (Enum, Show, Eq)

data Computer =
  Comp
    { pc           :: Address
    , memory       :: Memory
    , input        :: [Int]
    , output       :: [Int]
    , relativeBase :: Int
    , state        :: State
    }
  deriving (Eq, Show)

type Decoded = (Instruction, [Access])

toInt :: String -> Int
toInt s = read s

decode :: Int -> Instruction
decode 1  = Add
decode 2  = Mul
decode 3  = Input
decode 4  = Output
decode 5  = JumpTrue
decode 6  = JumpFalse
decode 7  = LessThen
decode 8  = Equals
decode 9  = AdjustBase
decode 99 = Halt

decode' x = trace ("decode : i = " ++ show x) (decode x)

load :: [Int] -> Memory
load xs = M.fromList $ zip addressList xs
  where
    addressList = fmap A [0 ..]

updateMemory :: Address -> Int -> Memory -> Memory
updateMemory = M.insert

patchMemory :: Computer -> Int -> Int -> Computer
patchMemory c a x = c {memory = updateMemory (A a) x (memory c)}

readMemory :: Memory -> Int -> Access -> Address -> Int
readMemory memory _ Direct address = findWithDefault 0 address memory
readMemory memory _ Memory address =
  findWithDefault 0 (A (memory ! address)) memory
readMemory memory base Relative address =
  findWithDefault 0 (A ((memory ! address) + base)) memory

asAccess :: Int -> Access
asAccess = toEnum

getAccess :: Int -> [Access]
getAccess n = fmap (asAccess . (`mod` 10)) [n, n `div` 10, n `div` 100]

decodeInstruction :: Int -> Decoded
decodeInstruction n =
  let instruction = decode $ n `mod` 100
      access = getAccess $ n `div` 100
   in (instruction, access)

boot :: Memory -> Computer
boot memory = Comp (A 0) memory [] [] 0 Wait

getInstruction :: Computer -> Decoded
getInstruction (Comp pc memory _ _ _ _) = decodeInstruction $ memory ! pc

movePC :: Address -> Int -> Address
movePC address delta = A ((asValue address) + delta)

binaryOp :: (Int -> Int -> Int) -> Computer -> Computer
binaryOp op c =
  c {pc = movePC (pc c) 4, memory = updateMemory address (op a b) (memory c)}
  where
    a = readValue c 1
    b = readValue c 2
    address = readA c 3

outputOp :: Computer -> Computer
outputOp c = c {pc = movePC (pc c) 2, output = current ++ [a]}
  where
    current = output c
    a = readValue c 1

inputOp :: Computer -> Computer
inputOp c =
  if inputSize == 0
    then c {state = Wait}
    else c
           { pc = movePC (pc c) 2
           , memory = updateMemory a (head (input c)) (memory c)
           , input = tail (input c)
           , state = Running
           }
  where
    inputSize = length $ input c
    a = readA c 1

inputOp' c = trace ("input Op : " ++ (show c)) inputOp c

jumpOp :: (Int -> Bool) -> Computer -> Computer
jumpOp f c = c {pc = resultPC}
  where
    a = readValue c 1
    b = A $ readValue c 2
    resultPC =
      if f a
        then b
        else movePC (pc c) 3

compareOp :: (Int -> Int -> Bool) -> Computer -> Computer
compareOp f c = c {pc = movePC (pc c) 4, memory = resultMemory}
  where
    a = readValue c 1
    b = readValue c 2
    storeAddress = readA c 3
    result =
      if f a b
        then 1
        else 0
    resultMemory = updateMemory storeAddress result (memory c)

adjustBase :: Computer -> Computer
adjustBase c = c {pc = movePC (pc c) 2, relativeBase = base}
  where
    a = readValue c 1
    base = (relativeBase c) + a

accessAt :: [Access] -> Int -> Access
accessAt (x:y:z:_) n =
  case n of
    1 -> x
    2 -> y
    3 -> z

readValue :: Computer -> Int -> Int
readValue computer n = readMemory cMemory cBase cAccess cAddress
  where
    (_, access) = getInstruction computer
    cAccess = accessAt access n
    cMemory = memory computer
    cBase = relativeBase computer
    cAddress = movePC (pc computer) n

readA :: Computer -> Int -> Address
readA computer n = A $ (cMemory ! cAddress) + cBase
  where
    (_, access) = getInstruction computer
    cMemory = memory computer
    cAccess = accessAt access n
    cBase =
      if cAccess == Relative
        then relativeBase computer
        else 0
    cAddress = movePC (pc computer) n

readA' c x =
  trace ("readA : c = " ++ (show c) ++ " ; x = " ++ (show x)) readA c x

exec :: Computer -> Computer
exec computer =
  let (instruction, access) = getInstruction computer
   in case instruction of
        Output     -> outputOp computer
        Input      -> inputOp computer
        Add        -> binaryOp (+) computer
        Mul        -> binaryOp (*) computer
        JumpTrue   -> jumpOp (/= 0) computer
        JumpFalse  -> jumpOp (== 0) computer
        LessThen   -> compareOp (<) computer
        Equals     -> compareOp (==) computer
        AdjustBase -> adjustBase computer
        Halt       -> computer {state = Halted}

run :: Computer -> Computer
run computer =
  let currentState = state computer
   in if currentState == Wait || currentState == Halted
        then computer
        else run $ exec computer

run' comp = trace ("run : input = " ++ (show $ input comp)) run comp

exec' comp = trace ("exec : " ++ (show comp)) exec comp

getOpcodes :: String -> [Int]
getOpcodes input = fmap toInt $ splitOn "," input

main :: IO ()
main = do
  input <- readFile "day5.txt"
  --let opcodes = getOpcodes input
  let opcodes = [1102, 34915192, 34915192, 7, 4, 7, 99, 0]
  let memory = load opcodes
  let comp = boot memory
  print $ run comp {state = Running, input = [5]}
