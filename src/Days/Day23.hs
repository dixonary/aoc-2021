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
{- ORMOLU_ENABLE -}

import Data.Bits
import Data.Char
import Data.STRef (newSTRef, readSTRef, modifySTRef)

import qualified Data.HashMap.Strict as HMap
import Data.HashMap.Strict (HashMap)

import qualified Data.IntSet as ISet
import Data.IntSet (IntSet)

import qualified Data.IntMap as IMap
import Data.IntMap (IntMap)

import Data.Bifunctor
import Data.Tuple
import Data.Function
import qualified Data.Text as Text
import Data.Ord

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  let
    burrowP = do
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
      rows <- ((\(a,b,c,d) -> [a,b,c,d]) <$> pod `around4` char '#')
          `sepBy` (some (char '#') `around` skipSpace)
      return $ mkPos $ [a,b,c,d,e,f,g] <> concat rows
    
    pod = choice $ zipWith (\a b -> char a $> b) ".ABCD" [None .. D]

  orig <- takeText
  let linesB = ["  #D#C#B#A#  ","  #D#B#A#C#  "]
      (top,bot) = splitAt 3 . lines . Text.unpack $ orig

  pure $ Input
    (parseErr burrowP orig)
    (parseErr burrowP $ Text.pack $ unlines $ top <> linesB <> bot)

------------ TYPES ------------
data Input = Input Pos Pos
instance Show Input where
  show (Input a b) = unlines [show a, show b]

data Amphipod = None | A | B | C | D deriving (Eq, Ord, Enum)

instance Show Amphipod where
  show = \case { None -> "."; A -> "A"; B -> "B"; C -> "C"; D -> "D" }

energy :: Amphipod -> Int
energy = \case { A -> 1; B -> 10; C -> 100; D -> 1000 }




data Pos = Pos 
  { ps :: Vector Amphipod
  , fixed :: IntSet
  } 
instance Eq Pos  where (==)    = (==) `on` ps
instance Ord Pos where compare = comparing ps

-- Convert a pos to an int, to be stored in an intset.
-- We take the enumeration of each position as a number in base 5.
intify :: Pos -> Int
intify (Pos ps _) = foldl' (\acc x -> acc*5 + x) 0 $ fromEnum <$> Vec.toList ps

mkPos :: [Amphipod] -> Pos
mkPos ll = Pos (Vec.fromList ll) $ ISet.fromList $
  [ i 
  | i <- [7..length ll-1]
  , all (\x -> ll !! x == toEnum (((i-7) `mod` 4)+1)) [i, i+4..length ll-1]
  ]

instance Show Pos where
  show (Pos pv ss)
    = unlines $ showTop <> showRest (drop 11 ps) <> [ "  █████████  "] <> ss'
    where 
      ps@(a:b:c:d:e:f:g:_) = Vec.toList pv
      showTop = [ "█████████████"
                , concat ["█",intercalate "." $ map (concatMap show) 
                          [[a,b],[c],[d],[e],[f,g]],"█"]
                , concat ["███",intercalate "█" $ show <$> take 4 (drop 7 ps),         "███"]
                ]
      showLine [a,b,c,d] = concat ["  █",intercalate "█" $ show <$> [a,b,c,d], "█  "]
      showRest [] = []
      showRest rr = let (ll,rr') = splitAt 4 rr in showLine ll : showRest rr'

      ss' = ["Fixed: {" <> intercalate "," (map show (ISet.toList ss)) <> "}"]



------------ PART A ------------
partA :: Input -> Maybe Int
partA (Input a _) = dijkstra a (goalPos a) getAdj

goalPos :: Pos -> Pos
goalPos (Pos v _) = mkPos 
                  $ replicate 7 None <> take (length v - 7) (cycle [A,B,C,D])

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
getAdj :: Pos -> [(Int, Pos)]
getAdj (Pos ps fixed) = do

  -- We move one non-fixed location
  ix <- filter (`ISet.notMember` fixed) [0..Vec.length ps - 1]

  -- Get the creature at the chosen point, and its coordinate
  let 
    sa = ps Vec.! ix
    (px, py) = toPosition ix

  -- We can't move an empty space
  guard $ sa /= None

  let 
    headingHome = ix < 7
    homes = let a = fromEnum sa 
            in pairWith (ps Vec.!) [a + 6, a + 10 .. length ps - 1]
    validHomes = [t | ((t,a):ts) <- tails homes, a==None, all ((== sa).snd) ts]

  dest <- if headingHome then validHomes else [0..6]
      
  let
    (dx,dy) = toPosition dest

    -- Compute all cells the movement will pass through.
    corridorLocs = map (,0) (px...dx)
    rowLocs = if headingHome
      then map (dx,) (1...dy)
      else map (px,) (1...py)

    locs = mapMaybe fromPosition $ delete (px,py) $ corridorLocs <> rowLocs

    -- The total weight of the movement
    weight = energy sa * hammingDist (px,py) (dx,dy)

  -- All cells the movement will pass through must be empty.
  guard $ all (\t -> ps Vec.! t == None) locs

  let
    fixed' = if headingHome then ISet.insert dest fixed else fixed
    ps' = flip Vec.imap ps $
      \i p -> if
        | i == ix   -> None
        | i == dest -> sa
        | otherwise -> p

  pure (weight, Pos ps' fixed')


dijkstra :: Pos -> Pos -> (Pos -> [(Int,Pos)]) -> Maybe Int
dijkstra s e getAdj = dijkstra' (IMap.singleton 0 (Set.singleton s)) Set.empty
  where
    -- This implementation is optimised towards the case where
    -- each weight may have many candidates (i.e. the graph is dense).
    dijkstra' :: IntMap (Set Pos) -> Set Pos -> Maybe Int
    dijkstra' cands ossified = do
      case IMap.lookupMin cands of
        Nothing                 -> Nothing
        Just (w,p) | e `elem` p -> Just w
        Just (w,p) -> let
          newCands p = IMap.fromList
                     $ map (bimap (+w) Set.singleton)
                     $ filter ((`Set.notMember` ossified) . snd)
                     $ getAdj p
          allNewCands = IMap.unionsWith Set.union $ Set.map newCands p 
          in dijkstra'
              (IMap.unionWith Set.union allNewCands
                $ IMap.map (`Set.difference` p) 
                $ IMap.delete w cands)
              (Set.union ossified $ Set.fromAscList 
                $ Set.toList p)

{-
#############    a 0   b 1   c 2   d 3   e 4   f 5   g 6  
#ab.c.d.e.fg#    h 7   i 8   j 9   k 10  l 11  m 12  n 13  o 14
###h#i#j#k###
  #l#m#n#o#  
  #########
-}

toPosition :: Int -> (Int,Int)
toPosition n
  | n < 7 = ([0,1,3,5,7,9,10]!!n, 0)
  | otherwise = let (y,x) = (n-7) `quotRem` 4 in ([2,4,6,8]!!x,y+1)

fromPosition :: (Int,Int) -> Maybe Int
fromPosition (x,y)
  | y == 0 = elemIndex x [0,1,3,5,7,9,10]
  | y >  0 = (7 + (y-1)*4 +) <$> elemIndex x [2,4,6,8]
  | otherwise = Nothing


------------ PART B ------------
partB :: Input -> Maybe Int
partB (Input _ b) = dijkstra b (goalPos b) getAdj