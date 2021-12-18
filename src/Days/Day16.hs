{-# LANGUAGE TypeApplications #-}
module Days.Day16 (runDay) where

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
import Util.Parsers as P (parseErr)

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text as Parser
import Data.Void
import Numeric (readHex)
import Text.Printf
import Debug.Trace

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Applicative (Alternative(many, (<|>)))
import Control.Monad (liftM)
import Data.Functor
import Data.Either (fromRight)
import Data.Function (fix, (&))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseErr packetP
            . Text.pack 
            . concatMap (printf "%04b" . fst . head . readHex @Int . pure)
            . Text.unpack
            <$> takeText

packetP :: Parser VersionedPacket
packetP = do
  v <- binary 3
  VersionedPacket v <$> do
    t <- binary 3
    case t of
      4 -> Literal <$> do
        let readLit x = do
              keepReading <- toEnum     <$> binary 1
              valSoFar    <- (x * 16 +) <$> binary 4
              (if keepReading then readLit else pure) valSoFar
        readLit 0
      o -> do
        lt <- binary 1
        case lt of
          0 -> do
            len <- binary 15
            Operator o . parseErr (many packetP) <$> Parser.take len
          1 -> do
            len <- binary 11
            Operator o <$> count len packetP

binary :: Int -> Parser Int
binary n = foldl1' (\r x -> r*2+x) <$> count n (("0" $> 0) <|> ("1" $> 1))

------------ TYPES ------------
type Input = VersionedPacket

data VersionedPacket = VersionedPacket
   { version :: Int 
   , packet  :: Packet 
   }
  deriving Show

data Packet 
  = Literal Int 
  | Operator Int [VersionedPacket] 
  deriving Show

------------ PART A ------------
partA :: Input -> Int
partA = versionSum

versionSum :: VersionedPacket -> Int
versionSum (VersionedPacket v p) = v + case p of
  Literal  _    -> 0
  Operator _ ps -> sum $ map versionSum ps


------------ PART B ------------
partB :: Input -> Int
partB = eval

eval :: VersionedPacket -> Int
eval (VersionedPacket v p) = eval' p
  where
    eval' (Literal n)     = n
    eval' (Operator o ps) = fmap eval ps & case o of
      0 -> sum 
      1 -> product
      2 -> minimum
      3 -> maximum
      5 -> \(x:y:_) -> fromEnum $ x > y
      6 -> \(x:y:_) -> fromEnum $ x < y
      7 -> \(x:y:_) -> fromEnum $ x == y