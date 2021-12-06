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
import Data.Function

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
step :: Map Int Int -> Map Int Int
step m = 
  let zeroes = Map.findWithDefault 0 0 m
  in m & Map.delete 0
       & Map.mapKeys (subtract 1)
       & Map.insert 8 zeroes
       & Map.insertWith (+) 6 zeroes 

afterSteps :: [Int] -> Int -> Int
afterSteps fs n = sum $ step `fpow` n $ freq fs


------------ PART A ------------
partA :: Input -> Int
partA fs = fs `afterSteps` 80


------------ PART B ------------
partB :: Input -> Int
partB fs = fs `afterSteps` 256