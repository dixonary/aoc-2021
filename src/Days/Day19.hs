{-# LANGUAGE UnicodeSyntax #-}

module Days.Day19 (runDay) where

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
import Data.Attoparsec.Text hiding (take, takeWhile)
import Data.Void
import Debug.Trace
import Data.Text (Text)
import Data.Function
import Data.Foldable (asum)
import Data.Ord (comparing)
import Control.Monad (guard)
import Data.Bifunctor
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = buildMap <$> scanner `sepBy` skipSpace
  where
    scanner = do
      takeTill isEndOfLine *> endOfLine
      fmap Set.fromList
        $ flip sepBy skipSpace
        $ signed decimal `around3` ","

------------ TYPES ------------
type Input = (Territory, [Coord])
type Scanner = Set Coord
type Territory = Set Coord
type Coord = (Int, Int, Int)
type Delta = (Int, Int, Int)
data Axis = X | Y | Z
type Indexed a = (Int, a)

------------ PART A ------------
partA :: Input -> Int
partA (m, sc) = length m

-- Note: we build the map "in the parser" so it can be reused for both parts

buildMap :: [Scanner] -> (Territory, [Coord])
buildMap (s:ss) = buildMap' (s,[(0,0,0)]) $ indexed1 ss
  where
    buildMap' :: (Territory,[Coord]) -> [Indexed Scanner] -> (Territory,[Coord])
    buildMap' (m,sc) [] = (m,sc)
    buildMap' (m,sc) ss = let
      Just (n,m',sc') = asum $ do
        s           <- ss
        (n, coords) <- alignmentsIxed s
        kk@(k:_)    <- takeWhile ((>=12) . length) $ tails $ Set.toList m
        cc@(c:_)    <- takeWhile ((>=12) . length) $ tails $ Set.toList coords
        let cΔ  = c `sub3` k
            kk' = Set.fromList kk
            cc' = filter (`Set.member` kk') $ map (`sub3` cΔ) cc
        pure $ if length (take 12 cc') == 12
          then Just (n, m `Set.union` Set.map (`sub3` cΔ) coords, cΔ)
          else Nothing
      in buildMap'
          (m', sc' : sc)
          (trace ("Adding scanner " <> show n) $ filter ((/= n) . fst) ss)

-- Index a list, starting from 1
indexed1 :: [a] -> [Indexed a]
indexed1 = zip [1..]

-- Get all rotations of all scanners
alignmentsIxed :: Indexed Scanner -> [Indexed Scanner]
alignmentsIxed (ix,s) = map (ix,) $ tSet $ Set.map transform s

-- Get every possible arrangement of coordinates
transform :: Coord -> [Coord]
transform (x,y,z) = pure (x,y,z) >>= rots X >>= rots Y >>= rots Z & nub
  where
    rots :: Axis -> Coord -> [Coord]
    rots a (x,y,z) = do
      let (comb, r1, r2) = case a of
            X -> ((x,,), y,z)
            Y -> ((,y,), x,z)
            Z -> ((,,z), x,y)
      uncurry comb <$> [(r1,r2), (r2,-r1), (-r1, -r2), (-r2,r1)]

tSet :: Ord a => Set [a] -> [Set a]
tSet = fmap Set.fromList
     . transpose
     . Set.toList


triliftA2 :: (a -> b -> c) -> (a,a,a) -> (b,b,b) -> (c,c,c)
triliftA2 f (a,b,c) (x,y,z) = (f a x, f b y, f c z)

add3, sub3 :: Num a => (a,a,a) -> (a,a,a) -> (a,a,a)
sub3 = triliftA2 (-)
add3 = triliftA2 (+)

------------ PART B ------------
partB :: Input -> Int
partB (_,sc) = maximum $ hammingDist3 <$> sc <*> sc