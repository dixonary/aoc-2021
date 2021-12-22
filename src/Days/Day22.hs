module Days.Day22 where

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
import Data.Attoparsec.Text
import Data.Void
import Control.Applicative
import Data.Functor
import Control.Monad (guard, when)
import Data.Biapplicative
import Debug.Trace
import Data.Ord
import Data.Function
import Data.Foldable
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = area `sepBy` skipSpace
  where
    area = do
      s <- ("on" $> True) <|> ("off" $> False)
      " x="
      (l,r) <- signed decimal `around` ".."
      ",y="
      (t,b) <- signed decimal `around` ".."
      ",z="
      (f,a) <- signed decimal `around` ".."
      pure (Area l (r+1) t (b+1) f (a+1), s)

------------ TYPES ------------
type Input = [(Area,Bool)]
instance {-# OVERLAPS #-} Show Input where show = unlines . fmap show

data Area = Area
  { left :: Int , right :: Int
  , top :: Int  , bottom :: Int
  , fore :: Int , aft :: Int
  } 

instance Show Area where
  show (Area l r t b f a) = "<" <> intercalate ", "
    [ show l ++ ".." ++ show (r-1)
    , show t ++ ".." ++ show (b-1)
    , show f ++ ".." ++ show (a-1)
    ] <> ">"


size :: Area -> Int
size Area{..} = (right-left) * (bottom-top) * (aft-fore)

intersects :: Area -> Area -> Bool
(Area l r t b f a) `intersects` (Area l' r' t' b' f' a')
  =  max l l' < min r r'
  && max t t' < min b b'
  && max f f' < min a a'

addArea :: [Area] -> (Area,Bool) -> [Area]
addArea as (aa, add) = let (is, os) = partition (intersects aa) as
  in (if add then (aa:) else id) $ concatMap (`subtractArea` aa) is <> os

-- Precondition: The areas must overlap
subtractArea :: Area -> Area -> [Area]
subtractArea (Area l r t b f a) (Area l' r' t' b' f' a') = let
  -- We compute at most 27 cubes. Some will not be needed
  spread (a,b,n) = (min a b, max a b, n)
  in [
    Area cl cr ct cb cf ca
    | (cl,cr,i) <- spread <$> [(l,l',1), (max l l', min r r',2), (r,r',3)]
    , (ct,cb,j) <- spread <$> [(t,t',1), (max t t', min b b',2), (b,b',3)]
    , (cf,ca,k) <- spread <$> [(f,f',1), (max f f', min a a',2), (a,a',3)]
    , cl < cr && ct < cb && cf < ca           -- remove null areas
    , (i,j,k)  /= (2,2,2)                     -- remove the intersection
    , (cl, cr) /= (l',l), (cl, cr) /= (r, r') -- these areas would all be owned
    , (ct, cb) /= (t',t), (ct, cb) /= (b, b') -- by the subtractor, so we don't
    , (cf, ca) /= (f',f), (cf, ca) /= (a, a') -- add them to our set of areas
  ]

------------ PART A ------------
partA :: Input -> Int
partA = sum . fmap size . foldl' addArea []
      . filter (intersects (Area (-50) 51 (-50) 51 (-50) 51) . fst)

------------ PART B ------------
partB :: Input -> Int
partB = sum . fmap size . foldl' addArea []