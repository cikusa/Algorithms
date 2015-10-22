module Main where

import Data.Bits
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe
import Data.Foldable
import Data.Word

import Control.Monad
import Control.Monad.Random

data MazeBlock
  = MazeObstacle
  | MazeEmpty
  | MazeEntrance
  | MazeExit
  deriving (Show, Eq)

data MazeMap = MazeMap
  { mazeData   :: Vector MazeBlock
  , mazeWidth  :: Int
  , mazeHeight :: Int
  , mazeEntrance :: (Int, Int)
  , mazeExit :: (Int, Int) }

mazeAt ::Int -> Int -> MazeMap -> MazeBlock
mazeAt x y maze = vec V.! (wid * y + x)
  where
    vec = mazeData  maze
    wid = mazeWidth maze

blockFromNum :: (Num n, Eq n) => n -> MazeBlock
blockFromNum n
  | n == 1 = MazeObstacle
  | n == 8 = MazeEntrance
  | n == 5 = MazeExit
  | otherwise = MazeEmpty

mazeFromList :: (Num n, Eq n) => Int -> Int -> [n] -> Maybe MazeMap
mazeFromList height width m =
  if length m /= width * height
    then Nothing
    else Just MazeMap
      { mazeData   = vec
      , mazeWidth  = width
      , mazeHeight = height
      , mazeEntrance = findPos MazeEntrance
      , mazeExit     = findPos MazeExit }
  where
    vec = V.fromList $ fmap blockFromNum m
    toPos i = (i `rem` width, i `div` width)
    findPos b = toPos $ fromJust $ V.findIndex (==b) vec

mazeMap :: MazeMap
mazeMap = fromJust $ mazeFromList 10 15 defMazeMapData

defMazeMapData :: [Int]
defMazeMapData =
  [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
  ,1,0,1,0,0,0,0,0,1,1,1,0,0,0,1
  ,8,0,0,0,0,0,0,0,1,1,1,0,0,0,1
  ,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1
  ,1,0,0,0,1,1,1,0,0,0,0,0,1,0,1
  ,1,1,0,0,1,1,1,0,0,0,0,0,1,0,1
  ,1,0,0,0,0,1,0,0,0,0,1,1,1,0,1
  ,1,0,1,1,0,0,0,1,0,0,0,0,0,0,5
  ,1,0,1,1,0,0,0,1,0,0,0,0,0,0,1
  ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]

data Direction
  = North
  | South
  | East
  | West
  deriving (Show, Eq)

finalPosition :: [Direction] -> MazeMap -> (Int, Int)
finalPosition dirs m =
    let (sx, sy) = mazeEntrance m
    in step sx sy dirs
  where
    step x y [] = (x, y)
    step x y (d:ds) =
      case mazeAt nx ny m of
        MazeObstacle -> step x y ds
        MazeExit -> (nx, ny)
        _ -> step nx ny ds
      where
        (nx, ny) = move x y d
    move x y dir =
      case dir of
        North -> (x, y - 1)
        South -> (x, y + 1)
        East  -> (x - 1, y)
        West  -> (x + 1, y)

assessScore :: [Direction] -> MazeMap -> Double
assessScore dir m = toScore $ disToExit $ finalPosition dir m
  where
    (ex, ey) = mazeExit m
    disToExit (finX, finY) = (abs $ finX - ex, abs $ finY - ey)
    toScore   (difX, difY) = 1 / fromIntegral (difX + difY + 1)

decodeGenome :: (FiniteBits a) => a -> [Direction]
decodeGenome g = fmap go [0, 2..finiteBitSize g - 1]
  where
    go i =
      case (bit1, bit2) of
        (False, False) -> North
        (False, True)  -> South
        (True,  False) -> East
        (True,  True)  -> West
      where
        bit1 = testBit g i
        bit2 = testBit g $ i + 1

printMaze :: MazeMap -> IO ()
printMaze m = mapM_ go [0..V.length vec - 1]
  where
    go i = do
      let block = vec V.! i
      putStr $ case block of
        MazeEmpty    -> "  "
        MazeObstacle -> "██"
        MazeEntrance -> "()"
        MazeExit     -> "[]"
      when ((i + 1) `rem` w == 0) $ putChar '\n'
    -- maximum row index
    w = mazeWidth m
    vec = mazeData m

type Rate = Double

-- GA for Genetic Alogrithm
data GASettings = GASettings
  { crossoverRate  :: Rate
  , mutationRate   :: Rate
  , maxPoplulation :: Int }
  deriving (Show)

defaultGASettings :: GASettings
defaultGASettings = GASettings
  { crossoverRate  = 0.7
  , mutationRate   = 0.001
  , maxPoplulation = 280 }

rouletteWheelSelection :: (FiniteBits a, MonadRandom m) => [(a, Double)] -> Double -> m a
rouletteWheelSelection gs totalScore = do
  slice <- getRandomR (0, totalScore)
  return $ go slice 0 gs
  where
    go slice acc ((genome, score):gss)
      | slice < acc + score = genome
      | otherwise = go slice (acc + score) gss
    go _ _ _ = undefined

crossover :: (Bounded a, FiniteBits a, MonadRandom m) => GASettings -> a -> a -> m (a, a)
crossover s f m = do
  rf <- getRandomR (0, 1)
  if rf > rate || f == m
    then return (f, m)
    else do
      splitPos <- getRandomR (0, finiteBitSize f - 1)
      let leftMask  = shiftL maxBound splitPos
          rightMask = shiftR maxBound splitPos
      return (f .&. leftMask .|. m .&. rightMask
             ,m .&. leftMask .|. f .&. rightMask)
  where rate = crossoverRate s

mutate :: (FiniteBits a, MonadRandom m) => GASettings -> a -> m a
mutate s g = foldM mutateBit g [0..finiteBitSize g - 1]
  where
    mutateBit a p = do
      rf <- getRandomR (0, 1)
      return $ if rf < rate then setBit a p else a
    rate = mutationRate s

mapPairM :: (Monad m) => (a -> m b) -> (a, a) -> m (b, b)
mapPairM f (a, b) = do
  m <- f a
  n <- f b
  return (m, n)

concatTuple :: [(a, a)] -> [a]
concatTuple [] = []
concatTuple ((a, b):xs) = a : b : concatTuple xs

epoch :: (Bounded a, FiniteBits a) => GASettings -> [a] -> IO (Either a [a])
epoch s gs = do
  print totalScore
  case find ((==1) . snd) combined of
    Just (genome, _) -> return $ Left genome
    Nothing -> do
      offsprings <- concatTuple <$> replicateM (maxPoplulation s `div` 2) makeBaby
      return $ Right offsprings
  where
    scores = fmap (flip assessScore mazeMap . decodeGenome) gs
    totalScore = sum scores
    combined = zip gs scores
    makeBaby = do
      father <- rouletteWheelSelection combined totalScore
      mother <- rouletteWheelSelection combined totalScore
      crossover s father mother >>= mapPairM (mutate s)

main :: IO ()
main = do
  genomes <- take (maxPoplulation defaultGASettings) <$> getRandoms
  res <- loop (genomes :: [Word64])
  print $ decodeGenome res
  where
    loop gs = do
      res <- epoch defaultGASettings gs
      case res of
        Right offsprings -> loop offsprings
        Left  solution   -> return solution
