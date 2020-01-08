module Intcode where

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
    { computerPc     :: Address
    , computerMemory :: Memory
    , computerInput  :: [Int]
    , computerOutput :: [Int]
    , computerBase   :: Int
    , computerState  :: State
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
patchMemory c a x = c {computerMemory = updateMemory (A a) x (computerMemory c)}

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

movePC :: Computer -> Int -> Address
movePC c delta = A ((asValue (computerPc c)) + delta)

binaryOp :: (Int -> Int -> Int) -> Computer -> Computer
binaryOp op c =
  c
    { computerPc = pc
    , computerMemory = updateMemory address (op a b) (computerMemory c)
    }
  where
    a = readValue c 1
    b = readValue c 2
    pc = movePC c 4
    address = readA c 3

outputOp :: Computer -> Computer
outputOp c = c {computerPc = pc, computerOutput = current ++ [a]}
  where
    current = computerOutput c
    a = readValue c 1
    pc = movePC c 2

inputOp :: Computer -> Computer
inputOp c =
  if inputSize == 0
    then c {computerState = Wait}
    else c
           { computerPc = pc
           , computerMemory = updateMemory a (head input) (computerMemory c)
           , computerInput = tail input
           , computerState = Running
           }
  where
    input = computerInput c
    inputSize = length $ input
    a = readA c 1
    pc = movePC c 2

inputOp' c = trace ("input Op : " ++ (show c)) inputOp c

jumpOp :: (Int -> Bool) -> Computer -> Computer
jumpOp f c = c {computerPc = pc}
  where
    a = readValue c 1
    b = A $ readValue c 2
    pc =
      if f a
        then b
        else movePC c 3

compareOp :: (Int -> Int -> Bool) -> Computer -> Computer
compareOp f c = c {computerPc = pc, computerMemory = memory}
  where
    a = readValue c 1
    b = readValue c 2
    storeAddress = readA c 3
    pc = movePC c 4
    result =
      if f a b
        then 1
        else 0
    memory = updateMemory storeAddress result (computerMemory c)

adjustBase :: Computer -> Computer
adjustBase c = c {computerPc = pc, computerBase = base}
  where
    a = readValue c 1
    base = (computerBase c) + a
    pc = movePC c 2

accessAt :: [Access] -> Int -> Access
accessAt (x:y:z:_) n =
  case n of
    1 -> x
    2 -> y
    3 -> z

readValue :: Computer -> Int -> Int
readValue computer n = readMemory memory base cAccess cAddress
  where
    (_, access) = getInstruction computer
    cAccess = accessAt access n
    memory = computerMemory computer
    base = computerBase computer
    cAddress = movePC computer n

readA :: Computer -> Int -> Address
readA computer n = A $ (cMemory ! cAddress) + base
  where
    (_, access) = getInstruction computer
    cMemory = computerMemory computer
    cAccess = accessAt access n
    base =
      if cAccess == Relative
        then computerBase computer
        else 0
    cAddress = movePC computer n

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
        Halt       -> computer {computerState = Halted}

run :: Computer -> Computer
run computer =
  let state = computerState computer
   in if state == Wait || state == Halted
        then computer
        else run $ exec computer

run' comp = trace ("run : input = " ++ (show $ computerInput comp)) run comp

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
  print $ run comp {computerState = Running, computerInput = [5]}
