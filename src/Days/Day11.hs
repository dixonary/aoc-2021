module Days.Day11 (runDay) where

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
import Data.Attoparsec.Text hiding (take, takeWhile)
import Data.Void
import Util.Parsers (coordinateParser)
import Data.Function
import Debug.Trace
import Control.Arrow
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser (pure . Normal . read . pure) 0

------------ TYPES ------------
data Octo = Normal Int | Flashed deriving (Eq)
instance Show Octo where 
  show = \case 
    Normal x | x > 9 -> "X"; 
    Normal x         -> show x;
    Flashed -> "F"

type Octos = Map (Int,Int) Octo

type Input = Octos

instance {-# OVERLAPS #-} Show Octos where
  show os = unlines 
          $ map (concatMap (show . snd))
          $ groupBy ((==) `on` (snd.fst)) $ sortOn (snd.fst) 
          $ Map.assocs os

grow :: Octo -> Octo
grow (Normal x) = Normal (x+1)
grow Flashed    = Flashed

isReady :: Octo -> Bool
isReady = \case
  Normal x -> x > 9
  Flashed  -> False
  
reset :: Octo -> Octo
reset = \case 
  Normal x -> Normal x
  Flashed  -> Normal 0   

------------ PART A ------------
partA :: Input -> Int
partA = snd . fpow step 100 . (,0)

step :: (Octos, Int) -> (Octos, Int)
step = first (fmap reset) . flash . first (fmap grow)

flash :: (Octos, Int) -> (Octos, Int)
flash (os, fs) = case Map.keys $ Map.filter isReady os of
  []        -> (os,fs)
  ((x,y):_) -> flash                    -- Go again
    $ (,fs+1)                           -- Increment flash counter
    $ Map.insert (x,y) Flashed          -- Mark (x,y) as Flashed
    $ foldr (Map.adjust grow) os        -- Grow all
    $ (,) <$> [x-1..x+1] <*> [y-1..y+1] -- Adjacents (includes self)

------------ PART B ------------
partB :: Input -> Int
partB = length 
      . takeWhile (\(os,_) -> not $ all (== os Map.! (0,0)) os) 
      . iterate step . (,0)
