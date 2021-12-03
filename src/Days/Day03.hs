module Days.Day03 (runDay) where

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
import Data.Char
import Data.Void
import Data.Ord (comparing)

import Debug.Trace

import {- # EXTREMELY_CURSED # -} Control.Arrow ((&&&), (>>>))
import Data.Bifunctor
{- ORMOLU_ENABLE -}

import Data.Foldable
import Data.Function ((&))

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 (read.pure <$> digit) `sepBy` skipSpace

------------ TYPES ------------
type Input = [[Int]]


------------ PART A ------------
partA :: Input -> Int
partA = uncurry (*) 
      . (fromBinary &&& fromBinary . map (1-)) 
      . map (head.modes) 
      . transpose

modes :: [Int] -> [Int]
modes = freq >>> \f -> case compare (f Map.! 0) (f Map.! 1) of
  LT -> [1]
  GT -> [0]
  EQ -> [0,1]

fromBinary :: [Int] -> Int
fromBinary = foldl1' (\a x -> a*2+x)

------------ PART B ------------
partB :: Input -> Int
partB xs = oxygenRating * co2Rating
  where
    oxygenRating = getRating
                 $ \case {[m] -> (== m); [0,1] -> (== 1)}
                 
    co2Rating    = getRating
                 $ \case {[m] -> (/= m); [0,1] -> (== 0)}

    getRating crit = fromBinary $ head $ head $ filter ((== 1).length) 
                   $ scanl' getRating' xs [0..length (head xs) - 1]
      where
        getRating' rem i = 
          let mi = map modes (transpose rem) !! i
          in filter (crit mi . (!!i)) rem