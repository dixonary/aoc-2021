module Days.Day14 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Util.Parsers as U
import Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text hiding (sepBy)
import Control.Applicative.Combinators
import Data.Void
import Data.Char (isUpper)
import qualified Data.Text as Text
import Control.Applicative
import Control.Arrow
import Data.Function
import Data.Functor
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  polymer <- freq . (\x -> zip x $ tail x) . Text.unpack <$> takeWhile1 isUpper
  skipSpace
  rules <- liftA2 (,) (liftA2 (,) letter letter) (" -> " *> letter)
           `sepBy` skipSpace
  pure (polymer, Map.fromList rules)


------------ TYPES ------------
type Input = (Polymer, Rules)

type Polymer = Map (Char, Char) Int
type Rules   = Map (Char, Char) Char

step :: Rules -> Polymer -> Polymer
step r p = Map.fromListWith (+) $ Map.assocs p >>= insert
  where
  insert ((a,b),count) = map (,count) 
    $ case r Map.!? (a,b) of { Nothing -> [(a,b)]; Just c -> [(a,c),(c,b)] }

compute :: Int -> Polymer -> Rules -> Int
compute n p r = getCount $ step r `fpow` n $ p
  where
  -- This function takes the max total occurrences of every letter
  -- in first OR second position (to cover the final letter)
  -- then gets the max and min occurrences of that and takes the difference.
  getCount p = uncurry (-)
             $ (maximum &&& minimum)
             $ uncurry (Map.unionWith max)
             $ ((&&&) `on` Map.mapKeysWith (+)) fst snd p

------------ PART A ------------
-- partA :: Input -> 
partA :: Input -> Int
partA = uncurry $ compute 10

------------ PART B ------------
partB :: Input -> Int
partB = uncurry $ compute 40
