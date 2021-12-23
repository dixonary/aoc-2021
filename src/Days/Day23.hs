module Days.Day23 where

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
import Data.Attoparsec.Text as Parser hiding (take, D)
import Data.Void
import Control.Monad
import Control.Applicative (Alternative(some))

import Data.Functor
import Debug.Trace
import Util.AStar (astar)
import Data.Hashable
import GHC.Generics (Generic)
{- ORMOLU_ENABLE -}

import Data.Bits
import Data.Char
import Data.STRef (newSTRef, readSTRef, modifySTRef)

import qualified Data.HashMap.Strict as HMap
import Data.HashMap.Strict (HashMap)

import qualified Data.HashSet as HSet
import Data.HashSet (HashSet)

import qualified Data.IntMap as IMap
import Data.IntMap (IntMap)

import Control.Monad.ST (runST)
import Control.Monad.Loops (whileM)
import Data.Bifunctor
import Data.Tuple
import Data.Function

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  takeTill isEndOfLine >> endOfLine
  char '#'
  [a,b] <- Parser.count 2 pod
  char '.'
  (c,d,e) <- pod `around3` char '.'
  char '.'
  [f,g] <- Parser.count 2 pod
  char '#'
  endOfLine
  some $ char '#'
  ((h,i,j,k),(l,m,n,o)) <-
    pod `around4` char '#' `around` (some (char '#') `sepBy` skipSpace)
  return $ mkPos [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o]

  where
    pod = choice $ zipWith (\a b -> char a $> b) ".ABCD" [None .. D]

------------ TYPES ------------
type Input = Pos

data Amphipod = None | A | B | C | D deriving (Eq, Ord, Enum)
instance Show Amphipod where
  show = \case { None -> "."; A -> "A"; B -> "B"; C -> "C"; D -> "D" }
energy :: Amphipod -> Int
energy = \case { A -> 1; B -> 10; C -> 100; D -> 1000 }

data Pos = Pos (Vector Amphipod) (Set Int) deriving (Eq, Ord)
mkPos :: [Amphipod] -> Pos
mkPos l = Pos (Vec.fromList l) Set.empty

instance Show Pos where
  show (Pos pv _) = let
    ps@[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o] = Vec.toList pv
    in unlines
    [ "█████████████"
    , concat ["█",intercalate "." $ map (concatMap show) [[a,b],[c],[d],[e],[f,g]],"█"]
    , concat ["███",intercalate "█" $ show <$> [h,i,j,k], "███"]
    , concat ["  █",intercalate "█" $ show <$> [l,m,n,o], "█  "]
    , "  █████████  "]

instance Hashable Pos where
  hashWithSalt s (Pos ps _) = s `xor` foldl' hashPos 0 ps
    where hashPos acc p = acc `shiftL` 4 + fromEnum p

------------ PART A ------------
partA :: Input -> Maybe Int
partA initialPos = dijkstra initialPos goalPos getAdj
  where
    goalPos = mkPos $ replicate 7 None <> [A,B,C,D,A,B,C,D]

    {-
      Adjacent positions are those reached by moving exactly one non-fixed
      amphipod from one empty position to another, passing only
      through empty positions along the way.

      All movements are either from CELL to CORRIDOR or from CORRIDOR to HOME.

      An amphipod may only move HOME if either
        - both of its HOME squares are empty; or
        - the bottom-most HOME square is the same type of amphipod and is HOME.

      If the move is from CORRIDOR to HOME, we "fix" that position so it may
      never be moved again.
    -}
    getAdj (Pos ps fixed) = do

      -- We move from one non-fixed location
      ix <- filter (`Set.notMember` fixed) [0..Vec.length ps - 1]

      -- Get the creature at the chosen point, and its coordinate
      let sa = ps Vec.! ix              
          (px, py) = toPosition Map.! ix

      -- We can't move an empty space
      guard $ sa /= None

      let headingHome = isCorridor ix

      dest <- if headingHome
          then let (a,b) = homes sa in
              case (ps Vec.! a, ps Vec.! b) of
                (None, None)          -> [b]
                (None, sb) | sa == sb -> [a]
                _                     -> []
          -- If moving into corridor, any corridor cell is a valid target.
          else [0..6]

      let
        (dx,dy) = toPosition Map.! dest

        -- Compuite all cells the movement will pass through.
        corridorLocs = map (,0) (px...dx) 
        rowLocs = if headingHome
          then map (dx,) (1...dy)
          else map (px,) (1...py)  
        locs = mapMaybe (fromPosition Map.!?) 
             $ delete (px,py) $ corridorLocs <> rowLocs

        -- The total weight of the movement
        weight = energy sa * hammingDist (px,py) (dx,dy)

      -- All cells the movement will pass through must be empty.
      guard $ all (\t -> ps Vec.! t == None) locs

      let 
        fixed' = if headingHome then Set.insert dest fixed else fixed
        ps' = flip Vec.imap ps $
          \i p -> if
            | i == ix   -> None
            | i == dest -> sa
            | otherwise -> p

      pure (weight, Pos ps' fixed')


dijkstra :: Pos -> Pos -> (Pos -> [(Int,Pos)]) -> Maybe Int
dijkstra s e getAdj = dijkstra' (Set.singleton (0,s)) Set.empty
  where
    dijkstra' cands ossified = do
      case Set.lookupMin cands of
        Nothing             -> Nothing
        Just (w,p) | e == p -> Just w
        Just (w,p) -> let
          newCands = Set.fromList
                   $ map (first (+w))
                   $ filter ((`Set.notMember` ossified) . snd)
                   $ getAdj $ traceShow p $ traceShow w p
          in dijkstra'
            (Set.union newCands $ Set.filter ((/=p) . snd) cands)
            (Set.insert p ossified)

{-
#############    a 0   b 1   c 2   d 3   e 4   f 5   g 6  
#ab.c.d.e.fg#    h 7   i 8   j 9   k 10  l 11  m 12  n 13  o 14
###h#i#j#k###
  #l#m#n#o#  
  #########
-}

type Loc = Int

isCorridor :: Int -> Bool
isCorridor = (< 7)

homes :: Amphipod -> (Loc,Loc)
homes a = let n = fromEnum a in (6+n, 10+n)

fromPosition :: Map (Int,Int) Int
fromPosition = Map.fromList $ (`zip` [0..])
  [(0,0), (1,0),       (3,0),       (5,0),       (7,0),       (9,0), (10,0)
               , (2,1)      , (4,1)      , (6,1)      , (8,1)
               , (2,2)      , (4,2)      , (6,2)      , (8,2)
  ]

toPosition :: Map Int (Int,Int)
toPosition = Map.fromList $ map swap $ Map.toList fromPosition


------------ PART B ------------
partB :: Input -> Int
partB = error "Not implemented yet!"


