module Days.Day09 (runDay) where

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
import Data.Attoparsec.Text hiding (take)
import Data.Void
import Debug.Trace
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser (pure.read.pure) 0

------------ TYPES ------------
type Input = Map (Int,Int) Int

------------ PART A ------------
partA :: Input -> Int
partA m = sum $ (+1) <$> lowPoints m

lowPoints :: Map (Int,Int) Int -> Map (Int,Int) Int
lowPoints m = Map.filterWithKey isLowPoint m
  where isLowPoint (x,y) k = all (k <) $ mapMaybe (m Map.!?)
          [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

------------ PART B ------------
partB :: Input -> Int
partB m = product $ take 3 $ sortOn negate $ map length getBasins
  where
    m' = Map.filter (/= 9) m
    getBasins = map basin $ Map.keys $ lowPoints m'
    basin (u,v) = converge expand $ Set.singleton (u,v)
    
    expand cs = cs <> Set.fromList 
              [c' 
              | c@(x,y) <- Set.toList cs
              , c' <- [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
              , c' `Map.member` m'
              , m Map.! c < m Map.! c'
              ]