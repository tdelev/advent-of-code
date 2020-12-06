module Day3 where 
    
import           Data.List.Split
import qualified Data.Set as Set
import           Data.Sort

toInt :: String -> Int
toInt s = read s

connect :: (Int, Int) -> String -> ([(Int, Int)], (Int, Int))
connect (x, y) (direction:number) = 
    let n = toInt number
    in case direction of
        'R' -> ([(x + x', y     ) | x' <- [1..n]], (x + n, y))
        'L' -> ([(x - x', y     ) | x' <- [1..n]], (x - n, y))
        'U' -> ([(x     , y + y') | y' <- [1..n]], (x, y + n))
        'D' -> ([(x     , y - y') | y' <- [1..n]], (x, y - n))

connect_two :: ([(Int, Int)], (Int, Int)) -> String -> ([(Int, Int)], (Int, Int))
connect_two (xs, start) wire = 
    let (result, (x, y)) = connect start wire
    in (xs ++ result, (x, y))

wiring :: [String] -> ([(Int, Int)], (Int, Int))
wiring wire = foldl connect_two ([], (0, 0)) wire

distance :: [(Int, Int)] -> (Int, Int) -> Int
distance [] _     = 0
distance (x:xs) a = if a == x then 1 else 1 + distance xs a
    
main :: IO ()
main = do
    input <- readFile "Day3.txt"
    let (wire1:wire2:_) = fmap (splitOn ",") $ lines input
    let (dots1, _) = wiring wire1 
    let (dots2, _) = wiring wire2
    let s2 = Set.fromList dots2
    let matches = filter (\x -> Set.member x s2) dots1
    -- Part 1
    -- let distances = map (\(x, y) -> (abs x) + (abs y)) matches
    -- print $ take 3 $ sort distances
    -- Part 2
    let distances1 = map (distance dots1) matches
    let distances2 = map (distance dots2) matches
    let distances = zipWith (+) distances1 distances2
    print $ minimum distances
    --print $ distances2