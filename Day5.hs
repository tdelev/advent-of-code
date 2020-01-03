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
    ("update code: a = " ++ (show address) ++ " val = " ++ (show val))
    updateCode
    code
    address
    val

readValue :: Code -> Access -> Address -> Int
readValue code Direct address = code ! address
readValue code Memory address = code ! (A (code ! address))

readValue' code a address =
  trace
    ("read value: access = " ++ (show a) ++ " a = " ++ (show address))
    readValue
    code
    a
    address

readAddress :: Code -> Access -> Address -> Address
readAddress code access address = A $ readValue code access address

asAccess :: Int -> Access
asAccess = toEnum

getAccess :: Int -> [Access]
getAccess n = fmap (asAccess . fromEnum . odd) [n, n `div` 10, n `div` 100]

decodeInstruction :: Int -> Decoded
decodeInstruction n =
  let instruction = decode $ n `mod` 100
      access = getAccess $ n `div` 100
   in (instruction, access)

decodeInstruction' n =
  trace ("Decode instruction: " ++ (show n)) $ decodeInstruction n

loadComputer :: Code -> Computer
loadComputer code = Comp (A 0) code []

getInstruction :: Computer -> Decoded
getInstruction (Comp pointer code _) = decodeInstruction $ code ! pointer

--getInstruction comp = trace ("getInstruction: " ++ (show $ code comp)) $ getInstruction' comp
movePointer :: Address -> Int -> Address
movePointer address delta = A ((asValue address) + delta)

incrementPointer :: Decoded -> Address -> Code -> Address
incrementPointer (JumpTrue, first:second:third:_) address code =
  if (readValue code first $ movePointer address 1) /= 0
    then A $ readValue code second $ movePointer address 2
    else movePointer address 3
incrementPointer (JumpFalse, first:second:third:_) address code =
  if (readValue code first $ movePointer address 1) == 0
    then A $ readValue code second $ movePointer address 2
    else movePointer address 3
incrementPointer (instruction, first:second:third:_) address code =
  let delta =
        case instruction of
          Output   -> 2
          Input    -> 2
          Add      -> 4
          Mul      -> 4
          LessThen -> 4
          Equals   -> 4
   in movePointer address delta

exec :: [Int] -> Decoded -> Computer -> (Computer, [Int], Bool, Bool)
exec input instruction (Comp pointer code output) =
  let incremented = incrementPointer instruction pointer code
      result = calculateResult input instruction code pointer
      resultOutput = getOutput instruction code pointer
   in ( Comp incremented result resultOutput
      , getInput instruction input
      , isHalt instruction, False)
  where
    calculateResult input instruction code pointer =
      let mvPtr = movePointer pointer
          readA = readAddress code Direct
          readV = readValue code
       in case instruction of
            (Input, _) ->
              updateCode code (readAddress code Direct $ mvPtr 1) (head input)
            (Output, _) -> code
            (Add, first:second:third:_) ->
              updateCode
                code
                (readA $ mvPtr 3)
                ((readV first $ mvPtr 1) + (readV second $ mvPtr 2))
            (Mul, first:second:third:_) ->
              updateCode
                code
                (readA $ mvPtr 3)
                ((readV first $ mvPtr 1) * (readV second $ mvPtr 2))
            (JumpTrue, _) -> code
            (JumpFalse, _) -> code
            (LessThen, first:second:third:_) ->
              updateCode
                code
                (readA $ mvPtr 3)
                (if (readV first $ mvPtr 1) < (readV second $ mvPtr 2)
                   then 1
                   else 0)
            (Equals, first:second:third:_) ->
              updateCode
                code
                (readA $ mvPtr 3)
                (if (readV first $ mvPtr 1) == (readV second $ mvPtr 2)
                   then 1
                   else 0)
    getOutput instruction code pointer =
      case instruction of
        (Output, _) -> [readValue code Memory $ movePointer pointer 1]
        _           -> []
    getInput instruction input =
      case instruction of
        (Input, _) -> tail input
        _          -> input

--exec input ins comp = trace ("exec: input : " ++ (show input) ++ "ins: " ++ (show ins)) exec' input ins comp
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
