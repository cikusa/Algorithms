{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as Intro
import Control.Monad.ST

binarySearch :: (Ord a) => a -> Vector a -> Maybe Int
binarySearch a vec = search 0 (V.length vec)
  where
    search u b =
      if | v == a -> Just i
         | u <= b -> Nothing
         | v >  a -> search i b
         | True   -> search u i
      where
        i = (b - u) `div` 2
        v = vec ! i

fourSum :: (Num a, Ord a, Enum a) => Vector a -> Int
fourSum vec = runST $ do
  mvec <- V.thaw vec
  Intro.sort mvec
  sorted_vec <- V.unsafeFreeze mvec
  let lists = [ binarySearch (-a-b-c) sorted_vec
              | a <- [0..vec_len], b <- [a..vec_len], c <- [b..vec_len]]
  return $ foldr count 0 lists
  where
    count (Just _) a = a + 1
    count _ a = a
    vec_len = fromIntegral $ V.length vec

main :: IO ()
main = putStrLn "Hello world"
