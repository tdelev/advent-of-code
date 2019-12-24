{-# LANGUAGE DeriveFunctor #-}

module Pieces where

import           Data.List
import qualified Data.Map   as Map
import           Data.Maybe

type Piece = (Int, Int)

type Chain = [Piece]

type Pool = Map.Map Int [Int]

addPiece :: Piece -> Pool -> Pool
addPiece (a, b) =
  if a /= b
    then add a b . add b a
    else add a b
  where
    add a b pool =
      case Map.lookup a pool of
        Nothing -> Map.insert a [b] pool
        Just xs -> Map.insert a (b : xs) pool

removePiece :: Piece -> Pool -> Pool
removePiece (a, b) =
  if a /= b
    then rem a b . rem b a
    else rem a b
  where
    rem a b pool =
      case fromJust $ Map.lookup a pool of
        [] -> Map.delete a pool
        xs -> Map.insert a (delete b xs) pool

order :: [Piece] -> Pool
order = foldr addPiece Map.empty

data Rose =
  NodeR Int [Rose]
  deriving (Show)

grow :: (Int, Pool) -> [(Int, Pool)]
grow (n, pool) =
  case Map.lookup n pool of
    Nothing -> []
    Just xs -> [(a, removePiece (a, n) pool) | a <- xs]

data TreeF a =
  NodeF Int [a]
  deriving (Functor, Show)

newtype Fix f =
  Fix
    { unFix :: f (Fix f)
    }

type Tree = Fix TreeF

type Coalgebra f a = a -> f a

coalg :: Coalgebra TreeF (Int, Pool)
coalg (n, pool) = 
    case Map.lookup n pool of
        Nothing -> NodeF n []
        Just xs -> NodeF n [(a, removePiece (a, n) pool) | a <- xs]

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana colag) . coalg

tree = ana coalg

type Algebra f a = f a -> a

chainAlg :: Algebra TreeF (Int, [Chain])
chainAlg (NodeF n []) = (n, [])
chainAlg (NodeF n xs) = (n concat [push (n, a) b | (a, b) <- xs])
where 
    push :: (Int, Int) -> [Chain] -> [Chain]
    push (a, b) [] = [[(a, b)]]
    push (a, b) xs = [(a, b) : x | x <- xs]

data :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix


main :: IO ()
main = print $ coalg (1, pool)
  where
    pool = order [(1, 2), (4, 1), (3, 5)]
