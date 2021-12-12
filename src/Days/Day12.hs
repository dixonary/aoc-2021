module Days.Day12 (runDay) where

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

import Data.Char

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text hiding (count)
import Data.Void
import Data.Functor
import Util.Pair
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid
import Data.Tuple (swap)
import Data.Bifunctor (Bifunctor(second))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  let cave = choice
        [ Start <$  string "start" 
        , End   <$  string "end"   
        , Small <$> takeWhile1 isLower
        , Large <$> takeWhile1 isUpper
        ]
  xs <- cave `around` char '-' `sepBy` space
  pure
    $ fmap (delete Start) -- Reject inbound edges to Start
    $ Map.delete End      -- Reject outbound edges from End
    $ Map.fromListWith (<>) $ map (second pure) $ xs <> map swap xs

------------ TYPES ------------
type Input = (Map Cave [Cave]) -- Adjacency List

data Cave = Start | End | Small Text | Large Text
  deriving (Eq, Ord, Show)

isSmall :: Cave -> Bool
isSmall = \case { Small _ -> True; _ -> False }

------------ PART A ------------
partA :: Input -> Int
partA e = length $ getPaths False e [Start]

getPaths :: Bool -> Input -> [Cave] -> [[Cave]]
getPaths b e = \case
  p@(End:_) -> [p]
  p@(x  :_) -> do
    y <- e !@ x
    let (e', b') = upd b e (y:p) 
    getPaths e' b' (y:p)
  where
    upd :: Bool -> Input -> [Cave] -> (Bool, Input)
    upd allowDouble e (y:p) = case allowDouble of
      -- Once we double a small node, remove in-edges to all visited smalls
      -- Then switch to "singles only" mode
      True  | isSmall y && y `elem` p
                        -> (False, foldr (fmap.delete) e $ filter isSmall p)
      -- In singles mode, remove in-edges to any visited node
      False | isSmall y -> (False, delete y <$> e)
      -- In all other cases, we're good
      b                 -> (b, e)

------------ PART B ------------
partB :: Input -> Int
partB e = length $ getPaths True e [Start]