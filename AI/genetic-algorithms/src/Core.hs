{-# LANGUAGE KindContraints #-}

module GeneticAlgorithms.Core where

import Control.Monad
import Control.Monad.Random as R

import qualified Data.Foldable as F
import Data.Bits as B

class Genome g where
  type Gene g a :: Contraint

  length     :: g -> Int
  fromData   :: (Gene g a, Traversable t) => t a -> g
  getRandom  :: MonadRandom m => m g

  access     :: Gene g a => Int -> g -> a
  set        :: Gene g a => Int -> a -> g -> g

{-# RULES
"Genome/set"    forall k l a b g. set k a (set l b g)  = set l b (set k a)
"Genome/access" forall k a g.     access k (set k a g) = a
#-}

class Genome g => BinaryGeneome g where
  shift :: (Genome g) => Int -> g -> g
  gand  :: (Genome g) => g -> g -> g
  gor   :: (Genome g) => g -> g -> g
  gxor  :: (Genome g) => g -> g -> g
  greverse :: (Genome g) => g -> g -> g

instance (Bits b) => Genome b where
  type Gene b a = Bool

  length b = bitSize b

  fromData t = snd $ F.foldr' (0, zeroBits) t
    where
      go (num, res) elem =
        | elem      = (num + 1, bit num .|. res)
        | otherwise = (num + 1, res)
      
