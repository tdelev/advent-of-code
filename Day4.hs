module Day4 where

sixDigit :: Int -> Bool
sixDigit a = 
    let res = a `div` 100000 
    in res > 0 && res < 10

sameAdjecentDigits :: Int -> Bool
sameAdjecentDigits a 
    | a < 10                              = False
    | a `mod` 10 == (a `div` 10) `mod` 10 = True
    | otherwise                           = sameAdjecentDigits $ a `div` 10

neverDecrease :: Int -> Bool
neverDecrease a
    | a < 10                             = True
    | a `mod` 10 < (a `div` 10) `mod` 10 = False
    | otherwise                          = neverDecrease $ a `div` 10

rules :: Int -> Bool
rules a = (sixDigit a) && (sameAdjecentDigits a) && (neverDecrease a)

differentPasswords :: Int -> Int -> Int
differentPasswords a b = length $ filter rules [a..b]

digits :: Int -> [Int]
digits 0 = []
digits n = (digits $ n `div` 10) ++ [n `mod` 10]

main :: IO ()
main = print $ digits 12432 --differentPasswords 347312 805915