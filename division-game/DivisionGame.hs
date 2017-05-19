{-
Apr 7, 2017 riddler classic
https://fivethirtyeight.com/features/can-you-outsmart-our-elementary-school-math-problems/

We compute the optimal and random EVs by enumerating the entire game tree and computing the EV at each node by either averaging or choosing the maximum of all leafs.
-}
{-# LANGUAGE GADTs #-}
data Slot = S1 | S2 | S3 | S4
  deriving Eq

data Start
data Player
data Deck
 
data GameMove a where
  Start :: [GameMove Deck] -> GameMove Start
  DeckMove :: Int -> [GameMove Player] -> GameMove Deck
  PlayerMove :: Slot -> [GameMove Deck] -> GameMove Player
  End :: GameMove Deck

type Board = (Int,Int,Int,Int)

-- Enumerate the full game tree.
gameTree :: GameMove Start
gameTree = Start $ goDeck allSlots allDigits
  where
    allSlots = [S1,S2,S3,S4]
    allDigits = [0..9]
    goDeck slots remainingDigits =
      map
      (\digit ->
        DeckMove digit $
        goPlayer slots (filter (/= digit) remainingDigits))
      remainingDigits
    goPlayer [lastSlot] _ = [PlayerMove lastSlot [End]]
    goPlayer remainingSlots digits =
      map
      (\slot ->
        PlayerMove slot $
        goDeck (filter (/= slot) remainingSlots) digits)
      remainingSlots

-- Setter function for the game board.
setValue :: Slot -> Int -> Board -> Board
setValue S1 v (_,x,y,z) = (v,x,y,z)
setValue S2 v (w,_,y,z) = (w,v,y,z)
setValue S3 v (w,x,_,z) = (w,x,v,z)
setValue S4 v (w,x,y,_) = (w,x,y,v)

average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)

-- Given a way to find the expected value of a player's chosen move
-- from a list of all possible alternatives, compute the expected
-- value of a move.
scoreMove :: ([Double] -> Double) -> Board -> GameMove a -> Double
scoreMove strategy board (Start nextMoves) =
  average $ map (scoreMove strategy board) nextMoves
scoreMove strategy board (DeckMove card nextMoves) =
  strategy . flip map nextMoves $
    (\move@(PlayerMove slot _) ->
      scoreMove strategy (setValue slot card board) move)
scoreMove strategy board (PlayerMove _ nextMoves) =
  average $ map (scoreMove strategy board) nextMoves
scoreMove _ (w,x,y,z) End = fromIntegral $ (w*10+x)*(y*10+z)

blankBoard :: Board
blankBoard = (undefined,undefined,undefined,undefined)


randomEV = scoreMove average blankBoard gameTree
-- randomEV = 2339.3

optimalEV = scoreMove minimum blankBoard gameTree
-- optimalEV = 1056.8

skillAdvantage = randomEV / optimalEV
-- skillAdvantage = 2.2135
