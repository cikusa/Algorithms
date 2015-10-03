{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.Vector as V hiding (map, sum)
import qualified Data.Vector.Algorithms.Intro as Intro
import Control.Monad.ST
import Control.Monad.Primitive

sortVec :: (Num a, Ord a, PrimMonad m) => Vector a -> m (Vector a)
sortVec vec = do
  mvec <- V.thaw vec
  Intro.sort mvec
  V.unsafeFreeze mvec

-- This algorithm works only if the elements of vector don't repeat, so as
-- the other Sum algorithms in this folder, otherwise you'll get the
-- unexpected answer.
fasterTwoSum :: (Num a, Ord a) => Vector a -> Int
fasterTwoSum vec = runST $ do
  sorted_vec <- sortVec vec
  let
    scan a b =
      if | a == b    -> 0
         | va > -vb  -> res + scan a (b - 1)
         | otherwise -> res + scan (a + 1) b
      where
        va = sorted_vec ! a
        vb = sorted_vec ! b
        res = if va == -vb then 1 else 0
  return $ scan 0 $ V.length vec - 1

fasterThreeSum :: (Num a, Ord a, Enum a) => Vector a -> Int
fasterThreeSum vec = runST $ do
  sorted_vec <- sortVec vec
  let
    scan a b n =
      if | a == b     -> 0
         | va > -vb-n -> res + scan a (b - 1) n
         | otherwise  -> res + scan (a + 1) b n
      where
        va = sorted_vec ! a
        vb = sorted_vec ! b
        res = if va == -vb-n then 1 else 0
  return $ sum $ map (\i -> scan i last_index $ sorted_vec ! i) [0..last_index]
  where last_index = V.length vec - 1

main :: IO ()
main = putStrLn "Hello world"
