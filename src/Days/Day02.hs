module Days.Day02 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Functor
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
inputParser = line `sepBy` skipSpace
  where 
    line = ((,) <$> dir <*> (skipSpace *> decimal))
    dir = choice
          [ "forward" $> Forward
          , "up" $> Up
          , "down" $> Down 
          ]
  

------------ TYPES ------------
data Dir = Forward | Up | Down
  deriving (Show)

type Input = [(Dir, Int)]

------------ PART A ------------
partA :: Input -> Int
partA = uncurry (*) . foldl' go (0, 0)
  where 
    go (h, v) (d, x) = case d of
      Up      -> (h, v-x)
      Down    -> (h, v+x)
      Forward -> (h+x, v)

------------ PART B ------------
partB :: Input -> Int
partB = uncurry3 (const (*)) . foldl' go (0, 0, 0)
  where
    go (aim, h, v) (d, x) = case d of
      Up      -> (aim-x, h, v)
      Down    -> (aim+x, h, v)
      Forward -> (aim, h + x, v + (aim * x))
