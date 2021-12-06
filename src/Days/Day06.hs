module Days.Day06 (runDay) where

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
import Data.Semigroup

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` char ','

------------ TYPES ------------
type Input = [Int]


------------ SHARED ------------
-- counts :: [Int] -> [Int]
-- counts = map sum . iterate step . freq

newtype Grow = Grow { grow :: Map Int Int -> Map Int Int }
instance Semigroup Grow where Grow x <> Grow y = Grow $ x . y

step :: Grow
step = Grow (Map.fromListWith (+) . concatMap step' . Map.assocs)
  where 
    step' (0,n) = [(6,n), (8,n)]
    step' (t,n) = [(t-1,n)]

afterSteps :: [Int] -> Int -> Int
afterSteps fs count = sum $ (grow $ count `stimes` step) $ freq fs

------------ PART A ------------
partA :: Input -> Int
partA fs = fs `afterSteps` 80

------------ PART B ------------
partB :: Input -> Int
partB fs = fs `afterSteps` 256