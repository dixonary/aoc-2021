module Days.Day13 (runDay) where

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
import Data.Void
import Data.Functor
import Control.Applicative
import Util.Parsers (around)
import Data.Function
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,) <$> paper <*> (skipSpace *> fold `sepBy` skipSpace)
  where
    paper = listToPaper <$> (decimal `around` char ',' `sepBy` skipSpace)
    fold = do
      string "fold along "
      (,) <$> ((char 'x' $> X) <|> (char 'y' $> Y)) <*> (char '=' *> decimal)

------------ TYPES ------------
type Input = (Paper, [Fold])

instance {-# OVERLAPS #-} Show Input where
  show (p, fs) = unlines [show p, unlines (map show fs)]

newtype Paper = Paper { getPaper :: Map (Int,Int) Char }

instance Show Paper where
  show (Paper m) = let
    (l,r,t,b) = mapBoundingBox m
    in unlines [[Map.findWithDefault ' ' (x,y) m | x <- [l..r]] | y <- [t..b]]

listToPaper :: [(Int,Int)] -> Paper
listToPaper = Paper . Map.fromList . map (,'#')

data Dir = X | Y deriving Show
type Fold = (Dir, Int)

fold :: Paper -> Fold -> Paper
fold(Paper m) (d,p) = 
  Map.keys m
  & concatMap (\(x,y) -> [(x,y), case d of { X -> (p*2-x,y); Y -> (x,p*2-y) }])
  & filter (\(x,y) -> case d of {X -> x < p; Y -> y < p})
  & map (,'#')
  & Map.fromList
  & Paper

------------ PART A ------------
partA :: Input -> Int
partA (p,f:fs) = length $ getPaper $ fold p f

------------ PART B ------------
partB :: Input -> Paper
partB (p,fs) = foldl' fold p fs