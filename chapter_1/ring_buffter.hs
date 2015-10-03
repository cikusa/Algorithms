{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main where

import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Primitive.Array

class Monad m => MonadRingBuffer e m | e -> m where
  rbAppend :: e -> m ()
  rbNext   :: Integral n => m n
  rbRead   :: Integral n => n -> m e
  rbWrite  :: Integral n => n -> a -> m ()
  rbLength :: Integral n => m n

data SRingBuffer e = SRingBuffer
  { srbData :: [e]
  , srbEnd  :: Int
  , srbLen  :: Int }

instance Functor SRingBuffer where
  fmap f m = m { srbData = fmap f $ srbData m }

instance Applicative SRingBuffer where
  pure a = SRingBuffer
    { srbData = [a]
    , srbEnd  = 1
    , srbLen  = 1 }
  f <*> a = SRingBuffer
    { srbData = srbData f <*> srbData a
    , srbEnd  = srbEnd a 
    , srbLen  = srbLen f * srbLen a }

instance Monad SRingBuffer where
  return = pure
  m >>= f = foldr1 combine $ fmap f $ srbData m
    where combine x a = SRingBuffer
            { srbData = srbData x ++ srbData a
            , srbEnd  = 

main :: IO ()
main = putStrLn "hello"
