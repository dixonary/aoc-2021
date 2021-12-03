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
      . map mode
      . transpose

mode :: [Int] -> Int
mode = freq >>> \f -> fromEnum $ f Map.! 1 >= f Map.! 0

fromBinary :: [Int] -> Int
fromBinary = foldl1' (\a x -> a * 2 + x)

------------ PART B ------------

partB :: Input -> Int
partB xs = getRating (==) * getRating (/=)
  where
    getRating crit = fromBinary . fst . head 
                   $ until isSingleton reduceByMode
                   $ map pair xs
      where
        reduceByMode :: [([Int], [Int])] -> [([Int], [Int])]
        reduceByMode rs = map (second tail) $ filter (keep . head . snd) rs
          where keep = crit (mode $ map (head . snd) rs)