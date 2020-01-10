module Day14 where

import           Data.List.Split
import           Data.Map        as M hiding (drop, filter, foldl, take)
import           Debug.Trace

toPair :: [String] -> ([String], String)
toPair (x:y:_) = (splitOn ", " x, y)
toPair _       = ([], "")

toInt :: String -> Int
toInt = read

type Chemical = (Int, String)

type ReactionMap = M.Map String (Int, [Chemical])

type SpareMap = M.Map String Int

addSpare :: String -> Int -> SpareMap -> SpareMap
addSpare = M.insertWith (+)

updateSpare :: String -> Int -> SpareMap -> SpareMap
updateSpare = M.insert

addSpare' s i sm =
  trace ("add spare : s = " ++ (show s) ++ "; i = " ++ (show i)) addSpare s i sm

toReactionMap :: ([String], String) -> ReactionMap -> ReactionMap
toReactionMap (xs, key) m =
  let (count, chemical) = asChemical key
   in M.insert chemical (count, (fmap asChemical xs)) m

asMap :: [([String], String)] -> ReactionMap
asMap xs = foldl (\m a -> toReactionMap a m) M.empty xs

asChemical :: String -> Chemical
asChemical s =
  let (a:b:_) = splitOn " " s
   in (toInt a, b)

isORE :: [String] -> Bool
isORE xs = length xs == 1 && (head xs == "ORE")

pieces :: Int -> Int -> Int -> Int -> (Int, Int)
pieces need parts available spare =
  let total = parts * available + spare
   in if need <= total
        then (parts, total - need)
        else pieces need (parts + 1) available spare

pbs need low hi available spare =
  let parts = low + ((hi - low) `div` 2)
      total = parts * available + spare
   in if need <= total && need > ((parts - 1) * available + spare)
        then (parts, total - need)
        else if need < total
               then pbs need low parts available spare
               else pbs need (parts + 1) hi available spare

pbs' n l h a s =
  trace
    ("pbs : need = " ++
     (show n) ++
     " ; parts = " ++
     (show l) ++
     " ; h = " ++ (show h) ++ " ; av = " ++ (show a) ++ " ; sp = " ++ (show s))
    pbs
    n
    l
    h
    a
    s

pieces' :: Int -> Int -> Int -> (Int, Int)
pieces' need available spare = pbs need 0 need available spare

combine :: (Int, SpareMap) -> (Int, SpareMap) -> (Int, SpareMap)
combine (ax, aSmap) (bx, _) = (ax + bx, aSmap)

findFuel :: ReactionMap -> SpareMap -> (Int, String) -> (Int, SpareMap)
findFuel rmap smap (need, key) =
  if M.member key rmap == False
    then (0, smap)
    else let (available, values) = rmap ! key
             currentSpare = smap ! key
             (multiplier, spare) = pieces' need available currentSpare
             spareMap' = updateSpare key spare smap
             needValues =
               if multiplier > 0
                 then fmap (\(n, c) -> (n * multiplier, c)) values
                 else []
             recurseResult =
               foldl
                 (\b a -> combine (findFuel rmap (snd b) a) b)
                 (0, spareMap')
                 needValues
          in if isORE (fmap snd values)
               then (multiplier * (fst $ head values), spareMap')
               else recurseResult

findFuel' rmap smap nk =
  trace
    ("\nfind fuel : \nmap = " ++
     (show rmap) ++ " ;\nspare = " ++ (show smap) ++ "\nkey = " ++ (show nk))
    findFuel
    rmap
    smap
    nk

initSpareMap :: ReactionMap -> SpareMap
initSpareMap rmap =
  foldl (\smap key -> addSpare key 0 smap) M.empty (M.keys rmap)

findMaxFuel :: ReactionMap -> SpareMap -> Int
findMaxFuel rmap smap = findMaxBinary 0 t (-1) rmap smap

t :: Int
t = 1000000000000

findMaxBinary :: Int -> Int -> Int -> ReactionMap -> SpareMap -> Int
findMaxBinary low hi prevResult rmap smap =
  let mid = low + ((hi - low) `div` 2)
      result = fst $ findFuel rmap smap (mid, "FUEL")
   in if result == prevResult
        then mid
        else if result < t
               then findMaxBinary (mid + 1) hi result rmap smap
               else findMaxBinary low mid result rmap smap

main :: IO ()
main = do
  input <- readFile "day14.txt"
  --line <- getLine
  --let n = toInt line
  let rows = lines input
  let pairs = fmap (toPair . splitOn " => ") rows
  let reactions = asMap pairs
  let spareMap = initSpareMap reactions
  print $ fst $ findFuel reactions spareMap (998536, "FUEL")
  --print $ findMaxFuel reactions spareMap
