{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Data.Time.Clock.POSIX
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as Merge
import Text.Printf
import System.Random

doublingRatio :: (Integral n) => n -> (n -> IO POSIXTime) -> IO ()
doublingRatio initDim f = realToFrac <$> f initDim >>= test dims
  where
    dims = [2^n * initDim | n <- [1..] :: [Int]]
    test (n:ns) prev = do
      time <- realToFrac <$> f n :: IO Float
      printf "%10d %10.5f %10.1f\n" (fromIntegral n :: Int) time (time / prev)
      test ns time
    test [] _ = undefined

mergeSortTest :: Int -> IO POSIXTime
mergeSortTest dim = do
  vec <- V.unsafeThaw nums
  begin <- getPOSIXTime
  Merge.sort vec
  end <- getPOSIXTime
  return $ end - begin
  where nums = V.fromListN dim (randoms (mkStdGen 2) :: [Int])

main :: IO ()
main =
  doublingRatio 125 mergeSortTest
