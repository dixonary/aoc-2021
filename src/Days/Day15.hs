module Days.Day15 (runDay) where

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
import Data.Attoparsec.Text
import Data.Void

import Data.Graph.Inductive
import Debug.Trace
import Data.Tuple (swap)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser (pure.read.pure) 0

------------ TYPES ------------
type Input = Cave
type Cave = Map (Int,Int) Int

------------ PART A ------------
partA :: Map (Int, Int) Int -> Int
partA = getShortestPath

getShortestPath :: Cave -> Int
getShortestPath cave = fromJust $ spLength (ix (l,t)) (ix (r,b)) g
  where 
    (l,r,t,b) = mapBoundingBox cave

    coords = Map.keys cave
    ix (x,y) = y*(r+1)+x

    g :: Gr (Int,Int) Int
    g = mkGraph (swap <$> pairWith ix coords) 
          [ (ix u, ix v, w) 
          | u@(ux,uy) <- coords
          , v <- [(ux-1, uy), (ux+1,uy), (ux,uy-1), (ux,uy+1)]
          , Just w <- [cave Map.!? v]
          ]

------------ PART B ------------
partB :: Input -> Int
partB cave = getShortestPath bigCave
  where 
    (_,r,_,b) = mapBoundingBox cave
    
    bigCave = Map.foldMapWithKey allPositions cave

    allPositions (x,y) w = Map.fromList
      [((x+(r+1)*n, y+(b+1)*m), (w+m+n-1)`mod`9+1) | m <- [0..4], n <- [0..4]]