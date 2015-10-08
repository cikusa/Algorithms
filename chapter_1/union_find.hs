module Main where

import qualified Data.Vector as V
import Control.Monad.State
import Text.Printf

class UnionFind u where
  count :: u -> Int
  findi :: Int -> u -> Int
  union :: Int -> Int -> u -> u

  connected :: Int -> Int -> u -> Bool
  connected a b u = findi a u == findi b u

newtype QuickFind = QuickFind (V.Vector Int)

instance UnionFind QuickFind where
  count (QuickFind vec) = V.length vec
  findi e (QuickFind vec) = vec V.! e
  union a b u@(QuickFind vec) =
    if ai == bi
      then u else QuickFind $ V.map rep vec
    where
      ai = findi a u; bi = findi b u
      rep x = if x == bi then ai else x

mkQuickFind :: Int -> QuickFind
mkQuickFind n = QuickFind $ V.generate n id

newtype QuickUnion = QuickUnion (V.Vector Int)

instance UnionFind QuickUnion where
  count (QuickUnion vec) = V.length vec
  findi e u@(QuickUnion vec) = if e == v then e else findi v u
    where v = vec V.! e
  union a b u@(QuickUnion vec) =
    if ai == bi
      then u else QuickUnion $ vec V.// [(bi, ai)]
    where ai = findi a u; bi = findi b u

mkUnionFind :: Int -> QuickUnion
mkUnionFind n = QuickUnion $ V.generate n id

unionFindTest :: (UnionFind u) => String -> StateT u IO ()
unionFindTest ins = do
  lift $ putStrLn $ "UnionFind test for " ++ ins
  u <- get
  if count u < 10
    then lift $ putStrLn "  error: insufficient length."
    else do
      mapM_ unionTest   [(0, 1), (1, 2), (3, 4), (5, 6), (5, 8)]
      mapM_ connectTest [(0, 7), (1, 4), (7, 2), (0, 2), (6, 8)]
  where
    unionTest (a, b) = do
      modify $ union a b
      lift $ printf "  union (%d, %d)\n" a b
    connectTest (a, b) = do
      res <- connected a b <$> get
      lift $ printf "  connected (%d, %d): %s\n" a b (show res)

main :: IO ()
main = do
  evalStateT (unionFindTest "QuickFind")  $ mkQuickFind 10
  evalStateT (unionFindTest "QuickUnion") $ mkUnionFind 10
