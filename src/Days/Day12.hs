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
  let caves = map fst xs <> map snd xs
  pure (Set.fromList caves, Set.fromList xs)

------------ TYPES ------------
type Input = (Set Cave, Set Tunnel)

data Cave = Start | End | Small Text | Large Text
  deriving (Eq, Ord)
type Tunnel = (Cave,Cave)

instance Show Cave where
  show = \case { Start -> "start";         End   -> "end";
                 Small t -> Text.unpack t; Large t -> Text.unpack t }
instance {-# OVERLAPS #-} Show Tunnel where
  show (x,y) = show x <> "-" <> show y

isLarge, isSmall, isEnd :: Cave -> Bool
isLarge = \case { Large _ -> True; _ -> False }
isSmall = \case { Small _ -> True; _ -> False }
isEnd   = \case { Start -> True; End -> True; _ -> False }

------------ PART A ------------
partA :: Input -> Int
partA g = length $ getPaths g cond [Start]
  where 
    cond = and . Map.mapWithKey (\k v -> isLarge k || v<2) . freq

getPaths :: Input -> ([Cave] -> Bool) -> [Cave] -> [[Cave]]
getPaths (v,e) cond = \case
  p@(End:_) -> [p]
  p@(x  :_) -> getPaths (v,e) cond =<<
    [y:p 
    | y <- Set.toList v
    , (x,y) `elem` e || (y,x) `elem` e
    , cond (y:p)
    ]

------------ PART B ------------
partB :: Input -> Int
partB g@(v,e) = length $ getPaths g cond [Start]
  where 
    cond p = let 
      f = freq p
      vs = Set.toList v
      getWhere c = mapMaybe (f Map.!?) $ filter c $ Set.toList v
      in
      count (==2) (getWhere isSmall) <= 1 &&
      count (> 2) (getWhere isSmall) == 0 &&
      count (> 1) (getWhere isEnd)   == 0