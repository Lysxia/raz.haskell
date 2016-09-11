{-# language RecordWildCards #-}

import Control.Monad (when)
import Control.Monad.Random
import Criterion.Measurement
import Data.Foldable (forM_)
import Text.Printf

import Data.Raz.Core

data BenchParams = BenchParams
  { noHead :: Bool
  , rndSeed :: Int
  , tag :: String
  , testRaz :: Bool
  , testFt :: Bool
  , start :: Int
  , inserts :: Int
  , groups :: Int
  , reps :: Int
  , multInserts :: Bool
  }

defBenchParams :: BenchParams
defBenchParams = BenchParams
  { noHead = False
  , rndSeed = 0
  , tag = "None"
  , testRaz = True
  , testFt = False
  , start = 0
  , inserts = 10000
  , groups = 10
  , reps = 1
  , multInserts = False
  }

time :: IO a -> IO (Double, a)
time run = do
  start <- getTime
  a <- run
  stop <- getTime
  return (stop - start, a)

rndInsertRaz :: MonadRandom m => Int -> Int -> Raz Int -> m (Raz Int)
rndInsertRaz _ 0 r = return r
rndInsertRaz sz n r = do
  p <- getRandomR (0, sz)
  r <- (insert L n . focus p . unfocus) r
  rndInsertRaz (sz+1) (n-1) r

printRow :: Int -> Int -> String -> String -> Int -> Int -> Int -> Double -> IO ()
printRow = printf "%d,%d,%s,%s,%d,%d,%d,%.4f\n"

main = do
  initializeTime
  r <- insert L 0 (singleton 0)
  let BenchParams{..} = defBenchParams
  setStdGen (mkStdGen rndSeed)
  r <- if testRaz && start > 0
    then do
      (t, r) <- time $ rndInsertRaz 0 start r
      printRow 0 rndSeed tag "RAZ" 0 0 start t
      return r
    else return r
  forM_ [1 .. reps] $ \i -> do
    let
      ins | multInserts = inserts * i
          | otherwise = inserts
      seqRaz size 0 r = return ()
      seqRaz size repeats r = do
        (t, r) <- time $ rndInsertRaz size ins r
        printRow 0 rndSeed tag "RAZ" i size ins t
        seqRaz (size + ins) (repeats - 1) r
    when testRaz $ seqRaz start groups r
