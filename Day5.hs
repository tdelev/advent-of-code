module Day5 where

import           Data.List.Split
import           Data.Map        as M
import           Debug.Trace

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
  deriving (Enum, Show)

data Computer =
  Comp
    { pointer      :: Address
    , code         :: Code
    , output       :: [Int]
    , relativeBase :: Int
    }

instance Show Computer where
  show (Comp pointer code output relativeBase) =
    "Computer : code = " ++
    (show code) ++
    " ; pointer = " ++
    (show pointer) ++
    " output = " ++ (show output) ++ " relativeBase = " ++ (show relativeBase)

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

decode' x = trace ("decode " ++ show x) (decode x)

makeProgram :: [Int] -> Code
makeProgram xs = M.fromList $ zip addressList xs
  where
    addressList = fmap A [0 ..]

updateCode :: Code -> Address -> Int -> Code
updateCode code address value = M.insert address value code

updateCode' code address val =
  trace
    ("\n\nupdate code: a = " ++
     (show address) ++ " val = " ++ (show val) ++ "\n\n")
    updateCode
    code
    address
    val

readValue :: Code -> Int -> Access -> Address -> Int
readValue code _ Direct address = findWithDefault 0 address code
readValue code _ Memory address = findWithDefault 0 (A (code ! address)) code
readValue code base Relative address =
  findWithDefault 0 (A ((code ! address) + base)) code

readValue' code base a address =
  trace
    ("\nread value: access = " ++
     (show a) ++
     " address = " ++ (show address) ++ " ; base = " ++ (show base) ++ "\n")
    readValue
    code
    base
    a
    address

readAddress :: Code -> Int -> Access -> Address -> Address
readAddress code base Relative address = A ((code ! address) + base)
readAddress code base access address = A $ readValue code base Direct address

asAccess :: Int -> Access
asAccess = toEnum

getAccess :: Int -> [Access]
getAccess n = fmap (asAccess . (`mod` 10)) [n, n `div` 10, n `div` 100]

decodeInstruction :: Int -> Decoded
decodeInstruction n =
  let instruction = decode $ n `mod` 100
      access = getAccess $ n `div` 100
   in (instruction, access)

decodeInstruction' n =
  trace ("Decode instruction: " ++ (show n)) $ decodeInstruction n

loadComputer :: Code -> Computer
loadComputer code = Comp (A 0) code [] 0

getInstruction :: Computer -> Decoded
getInstruction (Comp pointer code _ _) = decodeInstruction $ code ! pointer

--getInstruction comp = trace ("getInstruction: " ++ (show $ code comp)) $ getInstruction' comp
movePointer :: Address -> Int -> Address
movePointer address delta = A ((asValue address) + delta)

incrementPointer :: Decoded -> Address -> Code -> Int -> Address
incrementPointer (JumpTrue, first:second:third:_) address code relativeBase =
  if (readValue code relativeBase first $ movePointer address 1) /= 0
    then A $ readValue code relativeBase second $ movePointer address 2
    else movePointer address 3
incrementPointer (JumpFalse, first:second:third:_) address code relativeBase =
  if (readValue code relativeBase first $ movePointer address 1) == 0
    then A $ readValue code relativeBase second $ movePointer address 2
    else movePointer address 3
incrementPointer (instruction, first:second:third:_) address code _ =
  let delta =
        case instruction of
          Output     -> 2
          Input      -> 2
          Add        -> 4
          Mul        -> 4
          LessThen   -> 4
          Equals     -> 4
          AdjustBase -> 2
          Halt       -> 0
   in movePointer address delta

exec :: [Int] -> Decoded -> Computer -> (Computer, [Int], Bool, Bool)
exec input instruction (Comp pointer code output relativeBase) =
  let incremented = incrementPointer instruction pointer code relativeBase
      result = calculateResult input instruction code pointer
      resultOutput = getOutput instruction code pointer
      base = getBase instruction relativeBase
   in ( Comp incremented result resultOutput base
      , getInput instruction input
      , isHalt instruction
      , False)
  where
    calculateResult input instruction code pointer =
      let mvPtr = movePointer pointer
          readA = readAddress code relativeBase
          readV = readValue code relativeBase
       in case instruction of
            (Input, first:_) ->
              updateCode
                code
                (readAddress code relativeBase first $ mvPtr 1)
                (head input)
            (Output, _) -> code
            (Add, first:second:third:_) ->
              updateCode
                code
                (readA third $ mvPtr 3)
                ((readV first $ mvPtr 1) + (readV second $ mvPtr 2))
            (Mul, first:second:third:_) ->
              updateCode
                code
                (readA third $ mvPtr 3)
                ((readV first $ mvPtr 1) * (readV second $ mvPtr 2))
            (JumpTrue, _) -> code
            (JumpFalse, _) -> code
            (AdjustBase, _) -> code
            (LessThen, first:second:third:_) ->
              updateCode
                code
                (readA third $ mvPtr 3)
                (if (readV first $ mvPtr 1) < (readV second $ mvPtr 2)
                   then 1
                   else 0)
            (Equals, first:second:third:_) ->
              updateCode
                code
                (readA third $ mvPtr 3)
                (if (readV first $ mvPtr 1) == (readV second $ mvPtr 2)
                   then 1
                   else 0)
            (Halt, _) -> code
    getOutput instruction code pointer =
      case instruction of
        (Output, first:_) ->
          output ++ [readValue code relativeBase first $ movePointer pointer 1]
        _ -> output
    getInput instruction input =
      case instruction of
        (Input, _) -> tail input
        _          -> input
    getBase instruction base =
      case instruction of
        (AdjustBase, first:_) ->
          base + (readValue code relativeBase first $ movePointer pointer 1)
        _ -> base

exec' input ins comp =
  trace
    ("exec: input : " ++ (show input) ++ "ins: " ++ (show ins))
    exec
    input
    ins
    comp

isHalt :: Decoded -> Bool
isHalt (ins, _) =
  if ins == Halt
    then True
    else False

getOpcodes :: String -> [Int]
getOpcodes input = fmap toInt $ splitOn "," input

execInstruction :: [Int] -> Computer -> (Computer, [Int], Bool, Bool)
execInstruction input comp =
  let ins = getInstruction comp
      waitOnInput = (fst ins == Input) && Prelude.null input
   in if waitOnInput
        then (comp, input, False, True)
        else exec input ins comp

execInstruction' input comp =
  trace
    ("exec instruction : \n" ++ (show input) ++ "\ncomp = " ++ (show comp))
    execInstruction
    input
    comp

run :: [Int] -> Computer -> Computer
run input comp =
  let ins = getInstruction comp
   in if isHalt ins
        then comp
        else let (resComp, resInput, _, _) = (exec input ins comp)
              in run resInput resComp

main :: IO ()
main = do
  input <- readFile "day5.txt"
  let opcodes = getOpcodes input
    --let opcodes = [1,1,1,4,99,5,6,0,99]
  --let opcodes = [1101, 100, -1, 4, 0]
    --print $ makeProgram opcodes
  let code = makeProgram opcodes
  let comp = loadComputer code
  let ins = getInstruction comp
  --print ins
  print $ run [5] comp --ins --run 0 comp
    --let final_code = init_code code 12 2
    --print $ run final_code 0
    --print $ noun_verb code 19690720
