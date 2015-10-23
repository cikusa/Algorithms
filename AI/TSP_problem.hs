module Main where

import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Data.Function
import Data.Maybe

import Control.Monad
import Control.Monad.Random
import Control.Monad.ST.Strict

import System.Random.Shuffle

type Position = (Double, Double)

posDistance :: Position -> Position -> Double
posDistance (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2

data City = City
  { cityNum :: Int
  , cityPos :: Position }

instance Eq City where
  (==) = (==) `on` cityNum

cityDistance :: City -> City -> Double
cityDistance = posDistance `on` cityPos

type CityMap = Vector City
type Genome  = Vector Int

tourDistance :: Genome -> CityMap -> Double
tourDistance g m = snd $ V.foldl' go (startCity, 0) g
  where
    go (lastCity, acc) index =
      if lastCity == startCity
        then (currCity, acc)
        else (currCity, acc + cityDistance lastCity currCity)
      where currCity = getCity index
    getCity i = m V.! (g V.! i)
    startCity = getCity 0

type Rate = Double

data GASettings = GASettings
  { crossoverRate  :: Rate
  , mutationRate   :: Rate
  , maxPoplulation :: Int }
  deriving (Show)

rouletteWheelSelection :: (MonadRandom m) => [(Genome, Double)] -> Double -> m Genome
rouletteWheelSelection gs totalScore = do
  slice <- getRandomR (0, totalScore)
  return $ go slice 0 gs
  where
    go slice acc ((genome, score):gss)
      | slice < acc + score = genome
      | otherwise = go slice (acc + score) gss
    go _ _ [] = undefined

-- Partially-mapped crossover algorithm
crossover :: (MonadRandom m) => GASettings -> Genome -> Genome -> m (Genome, Genome)
crossover s f m = do
  rf <- getRandomR (0, 1)
  if rf > rate || f == m
    then return (f, m)
    else do
      let glen = V.length f
      crossPoint1 <- getRandomR (0, glen - 2)
      crossPoint2 <- getRandomR (crossPoint1 + 1, glen - 1)
      return $ runST $ do
        mvf <- V.thaw f
        mvm <- V.thaw m
        let { mapGnome i
          | i > crossPoint2 = return ()
          | otherwise = do
            let v1 = f V.! i; v2 = m V.! i
            MV.swap mvf i (fromJust $ V.findIndex (==v2) f)
            MV.swap mvm i (fromJust $ V.findIndex (==v1) m)
            mapGnome $ i + 1 }
        mapGnome crossPoint1
        (,) <$> V.unsafeFreeze mvf <*> V.unsafeFreeze mvm
  where rate = crossoverRate s

mutate :: (MonadRandom m) => GASettings -> Genome -> m Genome
mutate s g = do
  rf <- getRandomR (0, 1)
  if rf > rate
    then return g
    else do
      let glen = V.length g
      mutPoint1 <- getRandomR (0, glen - 2)
      mutPoint2 <- getRandomR (mutPoint1, glen - 1)
      return $ runST $ do
        mv <- V.thaw g
        MV.swap mv mutPoint1 mutPoint2
        V.unsafeFreeze mv
  where rate = mutationRate s

cityMap :: CityMap
cityMap = V.fromList
  [ City 0 (0,  0)
  , City 1 (10, 0)
  , City 2 (5, 0)
  , City 3 (2, 0)
  , City 4 (7, 0)
  , City 5 (108, 0)
  , City 6 (10, 0)
  , City 7 (12, 0) ]

mapPairM :: (Monad m) => (a -> m b) -> (a, a) -> m (b, b)
mapPairM f (a, b) = (,) <$> f a <*> f b

concatTuple :: [(a, a)] -> [a]
concatTuple [] = []
concatTuple ((a, b):xs) = a : b : concatTuple xs

maxCombined :: (Genome, Double) -> (Genome, Double) -> (Genome, Double)
maxCombined g1@(_, s1) g2@(_, s2)
  | s1 > s2 = g1
  | otherwise = g2

defaultGASettings :: GASettings
defaultGASettings = GASettings
  { crossoverRate  = 0.75
  , mutationRate   = 0.1
  , maxPoplulation = 280 }

epoch :: GASettings -> [Genome] -> IO (Either Genome [Genome])
epoch s gs = do
  print shortestDis
  if shortestDis < 20
    then return $ Left $ fst $ foldr1 maxCombined combined
    else do
      offsprings <- concatTuple <$> replicateM (maxPoplulation s `div` 2) makeBaby
      return $ Right offsprings
  where
    tourDistances = fmap (flip tourDistance cityMap) gs
    longestDis  = foldr1 max tourDistances
    shortestDis = foldr1 min tourDistances

    scores = map (longestDis-) tourDistances
    totalScore = sum scores
    combined = zip gs scores

    makeBaby = do
      father <- rouletteWheelSelection combined totalScore
      mother <- rouletteWheelSelection combined totalScore
      crossover s father mother >>= mapPairM (mutate s)

main :: IO ()
main = do
  genomes <- getRandomLists $ maxPoplulation defaultGASettings
  res <- loop genomes
  print res
  where
    getRandomLists :: (MonadRandom m) => Int -> m [Genome]
    getRandomLists 0 = return []
    getRandomLists i = do
      rl <- shuffleM [0..V.length cityMap - 1]
      (V.fromList rl :) <$> (getRandomLists $ i - 1)
    loop gs = do
      res <- epoch defaultGASettings gs
      case res of
        Right offsprings -> loop offsprings
        Left  solution   -> return solution
