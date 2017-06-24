module Main where

import Control.Monad
import Data.Bifunctor
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import Graphics.Rendering.Chart.Easy hiding (Vector)
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)
import System.Random.Mersenne


clamp low high x | x < low = low
                 | x > high = high
                 | otherwise = x

sample :: MTGen -> Int -> Double -> [(Double,Double)] -> IO (Vector Double)
sample g n wiggleAmnt dist = V.replicateM n pickOne >>= V.mapM wiggle
  where
    pickOne :: IO Double
    pickOne = getChoice dist
              . (totalWeights *)
              <$> random g

    totalWeights = sum . map snd $ dist
                              
    getChoice [] _ = 0
    getChoice [(x, _ )] _ = x
    getChoice ((val,weight):rest) target =
      if target <= weight
      then val
      else getChoice rest (target-weight)

    wiggle x = do
      delta <- (subtract wiggleAmnt) . (2 * wiggleAmnt *) <$> random g
      return $ clamp 0 (1-(1e-10)) $ x + delta
      

runRound :: MTGen -> Int -> Int -> Double -> [(Double,Double)] -> IO [(Double,Double)]
runRound g turns nPlayers wiggle distribution = do
  players <- sample g nPlayers wiggle distribution
  scores <- MV.replicate nPlayers (0,0)
  -- scores <- MV.replicate nPlayers 0

  let runTrials remaining coffeePot =
        if remaining <= 0 then return () else do
          player <- floor . (fromIntegral nPlayers *)
                    <$> (random g :: IO Double)
          let cupSize = players V.! player
              coffeePot' = coffeePot - cupSize
          if coffeePot' <= 0
            then do
            MV.modify scores (second (+ 1)) player
            runTrials (remaining - 1) 1
            else do
            MV.modify scores (bimap (+ cupSize) (+ 1)) player
            -- MV.modify scores (+ cupSize) player
            runTrials (remaining - 1) coffeePot'

  runTrials turns 1

  finalScores <- V.map (uncurry (/)) <$> V.freeze scores
  -- finalScores <- V.freeze scores

  return $ V.toList $ V.zip players finalScores



plotResults results = 
  toFile def "results.svg" $ do
    setColors [opaque blue]
    plot $ points "run 1" (results)


main = do
  g <- newMTGen Nothing

  let loop :: Int -> [(Double,Double)] -> IO [[(Double,Double)]]
      loop n lastRoundResults =
        if n <= 0
        then return []
        else do
          results <- runRound g (10^8) 100 0.005 lastRoundResults
          (results :) <$> loop (n-1) results
      -- initialDist = [(x,1) | x <- [0.05,0.10..0.95]]
      -- initialDist = [(x,(x-0.5)^2+0.01) | x <- [0.01,0.02..0.98]]
      initialDist = [(0.7,1),(0.8,30),(0.9,30),(0.21,39)]

  results <- loop 1 initialDist

  toFile def "collusion-distribution-results.svg" $
    forM_ (zip results [1..]) $
    \(result,i) -> (plot $ points ("run "++show i) result)

  {-
  toFile def "results-zoom.svg" $
    forM_ (zip results [1..]) $
    \(result,i) -> (plot $ points ("run "++show i)
                           (filter ((>0.4).fst) result))
  -}
                   



  
          
