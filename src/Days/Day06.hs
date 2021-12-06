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
counts :: [Int] -> [Int]
counts = map sum . iterate step . freq

step :: Map Int Int -> Map Int Int
step = Map.fromListWith (+) . concatMap step' . Map.assocs
  where 
    step' (0,n) = [(6,n), (8,n)]
    step' (t,n) = [(t-1,n)]


------------ PART A ------------
partA :: Input -> Int
partA fs = counts fs !! 80


------------ PART B ------------
partB :: Input -> Int
partB fs = counts fs !! 256