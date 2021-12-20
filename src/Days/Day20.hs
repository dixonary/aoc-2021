module Days.Day20 (runDay) where

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
import Util.Parsers as P

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Functor
import Control.Applicative
import qualified Data.Text as Text
import Util.Util
import Data.Bool
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  alg <- Vec.fromList <$> many1 ((char '#' $> True) <|> (char '.' $> False))
  skipSpace
  img <- (,False) <$> coordinateParser (pure . (=='#')) 0
  pure (alg,img)

------------ TYPES ------------
type Input = (Alg, Image)
type Coord = (Int,Int)
type Image = (Map Coord Bool,Bool)
type Alg = Vector Bool

instance {-# OVERLAPS #-} Show Image where
  show (m,_) = "\n" <> let (l,r,t,b) = mapBoundingBox m
    in unlines [[bool '.' '#' $ m Map.! (x,y) | x <- [l..r]] | y <- [t..b]]

------------ PART A ------------
partA :: Input -> Int
partA = computeLights 2

convolve :: Alg -> Image -> Image
convolve vec (m,def) = (Map.fromList $ map kernelize newCoords, def')
  where
    (l,r,t,b) = mapBoundingBox m
    newCoords = (,) <$> [t-1..b+1] <*> [l-1..r+1]
    kernelize (x,y) = let
      area = Map.findWithDefault def `flip` m <$> 
                [(x',y') | y' <- [y-1..y+1], x' <- [x-1..x+1]]
      areaIx = foldl' (\acc n -> acc*2 + fromEnum n) 0 area
      in ((x,y),vec Vec.! areaIx)
    def' = vec Vec.! bool 0 511 def

computeLights :: Int -> (Alg, Image) -> Int
computeLights n (vec,img) = length $ Map.filter id $ fst 
                          $ convolve vec `fpow` n $ img

------------ PART B ------------
partB :: Input -> Int
partB = computeLights 50