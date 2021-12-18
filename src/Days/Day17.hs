module Days.Day17 (runDay) where

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
import Util.Parsers as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text hiding (takeWhile, take)
import Data.Void
import Debug.Trace
import Data.Biapplicative (biliftA2)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = "target area: x=" *> signed decimal `around` ".." `around` ", y="

------------ TYPES ------------
type Input = ((Int, Int), (Int, Int))

------------ PART A ------------
partA :: Input -> Int
partA = maximum . getHeights

getHeights :: Input -> [Int]
getHeights i@((_,r), (b,_))
  = mapMaybe (doTrace i 0 . steps)
  $ (,) <$> (0 ... abs r) <*> (negate (abs b) ... abs b)

doTrace :: Input -> Int -> [(Int, Int)] -> Maybe Int
doTrace ((l,r), (b,t)) = doTrace'
  where
    doTrace' highest ((x,y):rest)
      | x >  r || y <  b = Nothing
      | x >= l && y <= t = Just highest
      | otherwise        = doTrace' (max highest y) rest

velStep :: (Ord a, Num a, Num b) => (a, b) -> (a, b)
velStep (vx,vy) = (signum vx * max 0 (abs vx - 1), vy - 1)

steps :: (Int, Int) -> [(Int, Int)]
steps = scanl' (biliftA2 (+) (+)) (0,0) . iterate velStep

------------ PART B ------------
partB :: Input -> Int
partB = length . getHeights