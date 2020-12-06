quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:[]) = [x]
quicksort (x:xs) = (quicksort [lower | lower <- xs, lower < x]) ++ [x] ++ (quicksort [upper | upper <- xs, upper >= x])

main :: IO()
main = print $ quicksort [10, 2, 23, 1, 77, 5]