{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as Intro
import Control.Monad.ST

newtype VPair a = VPair (Int, (a, a))

instance Monoid (VPair a) where
  mempty = VPair (maxBound, undefined)
  mappend a@(VPair (difa, _)) b@(VPair (difb, _))
    | difa < difb = a
    | otherwise   = b

makeVPair :: (Integral a) => Vector a -> Int -> VPair a
makeVPair vec i = VPair (abs . fromIntegral $ v1 - v2, (v1, v2))
  where v1 = vec ! i; v2 = vec ! (i + 1)

unVPair :: VPair a -> (Int, (a, a))
unVPair (VPair p) = p

findClosetPair :: (Ord a, Integral a) => Vector a -> (a, a)
findClosetPair vec = runST $ do
  mvec <- V.thaw vec
  Intro.sort mvec
  sorted_vec <- V.unsafeFreeze mvec
  return $ snd . unVPair $ foldMap (makeVPair sorted_vec) [0..V.length vec - 2]

findFarthestPair :: (Ord a) => Vector a -> (a, a)
findFarthestPair vec = scan 2 (vec ! 0) (vec ! 1)
  where
    vlen = V.length vec
    scan i minv maxv =
      if | i == vlen -> (minv, maxv)
         | iv < minv -> scan (i + 1) iv maxv
         | iv > maxv -> scan (i + 1) minv iv
         | otherwise -> scan (i + 1) minv maxv
      where iv = vec ! i

main :: IO ()
main = putStrLn "Hello world"
