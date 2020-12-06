module Day4 where

import Data.List

sixDigit :: Int -> Bool
sixDigit a = 
    let res = a `div` 100000 
    in res > 0 && res < 10

sameAdjecentDigits :: Int -> Bool
sameAdjecentDigits a 
    | a < 10                              = False
    | a `mod` 10 == (a `div` 10) `mod` 10 = True
    | otherwise                           = sameAdjecentDigits $ a `div` 10

sameAdjDig :: [Int] -> Bool
sameAdjDig (a:b:c:d:[]) = a /= b && b == c && c /= d

surround :: [Int] -> [Int]
surround x = [10] ++ x ++ [10]

fours :: [Int] -> [[Int]]
fours a = fmap (take 4) (take 5 (tails (surround a)))

neverDecrease :: Int -> Bool
neverDecrease a
    | a < 10                             = True
    | a `mod` 10 < (a `div` 10) `mod` 10 = False
    | otherwise                          = neverDecrease $ a `div` 10

rules :: Int -> Bool
rules a = (sixDigit a) && (sameAdjecentDigits a) && (neverDecrease a)

digits :: Int -> [Int]
digits 0 = []
digits n = (digits $ n `div` 10) ++ [n `mod` 10]

same4 :: Int -> Bool
same4 a = any sameAdjDig $ fours $ digits a

rules' :: Int -> Bool
rules' a = (same4 a) && (neverDecrease a)

differentPasswords :: Int -> Int -> Int
differentPasswords a b = length $ filter rules' [a..b]


main :: IO ()
main = print $ differentPasswords 347312 805915