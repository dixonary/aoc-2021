module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text hiding (take)
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser [Int]
inputParser = decimal `sepBy` skipSpace

------------ PART A ------------
partA :: [Int] -> Int
partA xs = length $ filter id $ zipWith (<) xs (tail xs)

------------ PART B ------------
partB :: [Int] -> Int
partB = partA . map (sum . take 3) . tails 