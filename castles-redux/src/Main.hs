{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad.Reader
import Control.Monad.ST
import Data.Function
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Vector.Algorithms.Intro
import System.Random.Mersenne
import Text.PrettyPrint.HughesPJClass hiding ((<>))


--- random-mersenne is fast but the api blows... ---
type RandomIO = ReaderT MTGen IO

randomR :: (Int,Int) -> RandomIO Int
randomR (min,max) = do
  gen <- ask
  lift $ (+min) . floor . (*fromIntegral (max-min+1)) <$> (random gen :: IO Double)

randomRs :: (Int,Int) -> RandomIO [Int]
randomRs (min,max) = do
  gen <- ask
  lift $
    map ((+min) . floor . (*fromIntegral (max-min+1))) <$>
    (randoms gen :: IO [Double])

liftedRandom = lift . random =<< ask
liftedRandoms = lift . randoms =<< ask

--- Setting up the castle game ---
-- A strategy is an array of 10 values, normalized so that the sum is 1.
-- The value at index 0 is the weight given to the castle worth 10, etc.
newtype Strategy = Strategy (U.Vector Double)

instance Pretty Strategy where
  pPrint (Strategy s) =
    vcat $
    text "Strategy:" :
    ( map (\(i,v) -> nest 2 $
                     int i <>
                     text ": " <>
                     int v ) .
      zip [10,9..] .
      map (round . (100*)) $
      U.toList s )

normalize :: U.Vector Double -> U.Vector Double
normalize v = U.map (/ U.sum v) v

-- normalizing constructor
_Strategy :: U.Vector Double -> Strategy
_Strategy = Strategy . normalize

beats :: Strategy -> Strategy -> Bool
beats (Strategy x) (Strategy y) =
  case compare outcome 0 of
    GT -> True
    LT -> False
    EQ -> False
  where
    outcome = U.sum $
              U.izipWith
              (\i xi yi -> case compare xi yi of
                  GT -> 10 - i
                  LT -> (-10) + i
                  EQ -> 0 )
              x y


--- Genetic Algorithm ---

score :: V.Vector Strategy -> Strategy -> Int
score population strategy = V.length . V.filter (strategy `beats`) $ population

randomStrategy :: RandomIO Strategy
randomStrategy =
  _Strategy . U.fromList . take 10 <$> liftedRandoms

mutate :: Strategy -> RandomIO Strategy
mutate (Strategy s) = do
  ix <- randomR (0,10)
  newWeight <- liftedRandom
  return . _Strategy $ s U.// [(ix,newWeight)]

crossover :: Strategy -> Strategy -> RandomIO (Strategy,Strategy)
crossover (Strategy x) (Strategy y) = do
  ix <- randomR (0,10)
  let (x1,x2) = U.splitAt ix x
      (y1,y2) = U.splitAt ix y
      newX = _Strategy $ x1 <> y2
      newY = _Strategy $ y1 <> x2
  return (newX,newY)

runGA :: V.Vector Strategy -- Fixed population to include in fitness eval
      -> V.Vector Strategy -- Starting population
      -> Int -- Elitist carryover between generations
      -> Int -- Selection size
      -> Int -- Tournament-size parameter for selection
      -> Double -- Percent crossover vs mutation
      -> Int -- Generations to run
      -> RandomIO Strategy -- Result
runGA fixedPop startingPop carryover selectionSize tournamentSize pctCrossover maxGens =
  go 0 startingPop 
  where
    popSize = V.length startingPop
    crossoverCount = (round $ fromIntegral (popSize-carryover) * pctCrossover) `div` 2
    mutationSize = selectionSize - 2*crossoverCount
    
    go gen pop = do
      let scoredPop = V.map (\s -> (s,score pop s + score fixedPop s)) pop
          sortedPop = V.map fst $ runST $ do
            thawed <- V.thaw scoredPop
            sortBy (flip (compare `on` snd)) thawed
            V.unsafeFreeze thawed          
      if gen >= maxGens
        then return $ V.head sortedPop
        else do
        let carriedOver = V.take carryover sortedPop
        selected <- runSelection sortedPop
        mutated <- runMutation selected
        crossedOver <- runCrossover selected
        go (gen+1) (carriedOver <> mutated <> crossedOver)

    runSelection pop =
      V.replicateM selectionSize $ do
        tournamentEntries <- take tournamentSize <$> randomRs (0,selectionSize-1)
        let winner = minimum tournamentEntries
        return $ pop V.! winner

    runCrossover selection =
      fmap (uncurry (<>) . V.unzip) .
      V.replicateM crossoverCount $ do
        parentAIx <- randomR (0,selectionSize-1)
        parentBIx <- randomR (0,selectionSize-1)
        let parentA = selection V.! parentAIx
            parentB = selection V.! parentBIx
        crossover parentA parentB
        
    runMutation selection =
      V.replicateM mutationSize $ do
        ix <- randomR (0,selectionSize-1)
        let parent = selection V.! ix
        mutate parent
    
main :: IO ()
main = do
  g <- newMTGen Nothing
  flip runReaderT g $ do
    initialPop <- V.replicateM 300 randomStrategy
    best <- runGA mempty initialPop 5 120 20 0.8 1000
    lift . putStrLn . prettyShow $ best
