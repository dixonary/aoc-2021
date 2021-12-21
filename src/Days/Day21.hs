module Days.Day21 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Control.Monad (replicateM)
import Data.Biapplicative
import Control.Applicative
import Data.STRef
import Control.Monad.ST
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,)
  <$> ("Player 1 starting position: " *> decimal <* skipSpace)
  <*> ("Player 2 starting position: " *> decimal)

------------ TYPES ------------
type Input = (Int,Int)

data Turn = One | Two deriving (Eq, Ord)
type TurnState = (Turn, Int, [Int], [Int], [Int], Int, Int)

detDie :: [Int]
detDie = cycle [1..100]

diracDist :: [(Int, Integer)]
diracDist = [(3,1), (4, 3), (5,6), (6,7), (7,6), (8,3), (9,1)]

places :: [Int]
places = cycle [1..10]

turn :: TurnState -> TurnState
turn (t, n, a:b:c:d, p1, p2, s1, s2) = case t of
  One -> let p1'@(p:_) = drop (a+b+c) p1 in (Two, n+3, d, p1', p2, s1+p, s2)
  Two -> let p2'@(p:_) = drop (a+b+c) p2 in (One, n+3, d, p1, p2', s1, s2+p)

------------ PART A ------------
partA :: Input -> Int
partA (i1, i2) =
  (\(_,n,_,_,_,s1,s2) -> n * min s1 s2)
  $ head
  $ dropWhile (\(_,_,_,_,_,s1,s2) -> s1 < 1000 && s2 < 1000)
  $ iterate turn
    ( One, 0, detDie
    , drop (i1-1) places
    , drop (i2-1) places
    , 0, 0)

------------ PART B ------------
partB :: Input -> Integer
partB (a,b) = uncurry max $ victories (One, a, b, 21, 21)

victories :: (Turn, Int, Int, Int, Int) -> (Integer, Integer)
victories (t, p1, p2, s1, s2) = runST $ do
  m <- newSTRef (Map.empty :: Map (Turn, Int, Int, Int, Int) (Integer, Integer))
  let 
    v (t, p1, p2, s1, s2)
      | t == Two && s1 <= 0 = pure (1,0)
      | t == One && s2 <= 0 = pure (0,1)
      | otherwise = do
          mm <- readSTRef m
          case Map.lookup (t, p1, p2, s1, s2) mm of
            Just x -> pure x
            Nothing -> do
              (w1,w2) <- fmap sum2 $ sequence $ do
                (roll, count) <- diracDist
                let
                  addRoll p = (p + roll - 1) `mod` 10 + 1
                  m' = case t of
                    One -> let p' = addRoll p1 in (Two, p', p2, s1 - p', s2)
                    Two -> let p' = addRoll p2 in (One, p1, p', s1, s2 - p')
                pure $ both (*count) <$> v m'
              modifySTRef m (Map.insert (t, p1, p2, s1, s2) (w1,w2))
              pure (w1, w2)
  v (t, p1, p2, s1, s2)