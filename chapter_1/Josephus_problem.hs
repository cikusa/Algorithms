{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Queue.Corec
import Control.Monad.State

josephus :: (Enum a, Integral a) => a -> a -> [a]
josephus n m = reverse $ runResult $ execStateT (queue >> go 1) []
  where
    queue = foldr1 (>>) $ map (lift . enQ) [0..n-1]
    go i = do
      elmns <- get
      value <- lift $ deQ_break elmns
      if i `rem` m == 0
        then modify (value:)
        else lift $ enQ value
      go $ i + 1

main :: IO ()
main = print (josephus 7 2 :: [Int])
