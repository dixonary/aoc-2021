module Days.Day07 (runDay) where

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
import Control.Arrow
import Data.Ord (comparing)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` char ','

------------ TYPES ------------
type Input = [Int]

------------ PART A ------------
partA :: Input -> Int
partA xs = minimum $ map totDist [minimum xs .. maximum xs]
  where totDist n = sum $ map (abs . subtract n) xs

------------ PART B ------------
partB :: Input -> Int
partB xs = minimum $ map totDist [minimum xs .. maximum xs]
  where totDist n = sum $ map (\x -> let t = abs $ x - n in (t*t+t)`div`2) xs
