{-# LANGUAGE ParallelListComp #-}

module Days.Day05 (runDay) where

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
import Util.Parsers as P

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Bool
import Control.Applicative
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `around` (char ',') `around` string " -> " `sepBy` skipSpace

------------ TYPES ------------
type Input = [Line]
type Line = ((Int,Int),(Int,Int))

------------ PART A ------------
partA :: Input -> Int
partA = partB . filter isOrthog

isOrthog :: Line -> Bool
isOrthog ((x1,y1),(x2,y2)) = (x1 == x2) || (y1 == y2)

------------ PART B ------------
partB :: Input -> Int
partB = length . Map.filter (>=2) . freq . foldMap line
  where 
    line l@((x1,y1),(x2,y2)) 
      = (if isOrthog l then liftA2 (,) else zip) (x1...x2) (y1...y2)