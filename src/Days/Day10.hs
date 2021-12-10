module Days.Day10 (runDay) where

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
import Data.Attoparsec.Text
import Data.Void
import Data.Char (isSpace)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = map (\x -> tryParse (x, [])) 
              <$> many1 (satisfy (not.isSpace)) `sepBy` space

------------ TYPES ------------
type Input = [PartialParse]
type PartialParse = (String,String)

------------ PART A ------------
partA :: Input -> Int
partA = sum . map (score.head.fst) . filter (not.isIncomplete) 

tryParse :: (String, String) -> (String, String)
tryParse ([], s) = ([], s)
tryParse (x:xs, s)
  | isOpen x = tryParse (xs, x:s)
  | otherwise = case s of
      [] -> (x:xs, [])
      s:ss | s == paired x -> tryParse (xs, ss)
      _ -> (x:xs, s)

isIncomplete :: PartialParse -> Bool
isIncomplete = null.fst

isOpen :: Char -> Bool
isOpen = (`elem` ("{[<("::String))

paired :: Char -> Char
paired = (Map.fromList (zip "}]>)" "{[<(") Map.!)

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
score '(' = 1
score '[' = 2
score '{' = 3
score '<' = 4

------------ PART B ------------
partB :: Input -> Int
partB = median
        . map (foldl1' (\a x -> a*5+x) . map score .snd)
        . filter isIncomplete
  where 
    median xs = sort xs !! (length xs `div` 2)