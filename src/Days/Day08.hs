module Days.Day08 (runDay) where

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
import Control.Monad (guard)
import Data.Function
import Data.Set (size)
import Data.Set.Unicode ((∩))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (Set.fromList <$> many1 letter) 
              `sepBy` char ' ' 
              `around` string " | " 
              `sepBy` space

------------ TYPES ------------
type Input = [([Set Char], [Set Char])]

------------ PART A ------------
partA :: Input -> Int
partA = length . filter (\x -> Set.size x `elem` [2,3,4,7]) . concatMap snd

------------ PART B ------------
partB :: Input -> Int
partB = sum . map decode
  where
    decode (is, code) = foldl1' (\a b -> a*10+b)   -- Digits to integer
                      $ (\m -> map (m Map.!) code) -- Apply the mapping
                      $ head $ getCode is          -- Find the mapping

    getCode []     = [Map.empty]
    getCode (i:is) = [
      Map.insert i d assg
      | assg <- getCode is
      , d    <- [0..9] \\ Map.elems assg
      , Set.size (segs d) == length i
      -- The number of shared segments between all pairs of digits must
      -- be preserved by our mapping.
      , flip all (Map.keys assg) $
        \x -> Set.size (i ∩ x) == Set.size (segs d ∩ segs (assg Map.! x))
      ]

-- The "true" assignment of segments for each digit
segs :: Int -> Set Char
segs = Set.fromList . \case
  0 -> "abcefg"
  1 -> "cf"
  2 -> "acdeg"
  3 -> "acdfg"
  4 -> "bcdf"
  5 -> "abdfg"
  6 -> "abdefg"
  7 -> "acf"
  8 -> "abcdefg"
  9 -> "abcdfg"