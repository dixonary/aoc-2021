module Util.Util where

{- ORMOLU_DISABLE -}
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Debug.Trace (trace)
import Data.Bifunctor
{- ORMOLU_ENABLE -}

{-
This module contains a series of miscellaneous utility functions that I have found helpful in the past.
-}

-- Takes a list.
-- Returns a map from elements of that list to the number of times they appeared in the list.
freq :: (Ord a) => [a] -> Map a Int
freq = Map.fromListWith (+) . fmap (,1)

-- Takes a nested list (to be thought of as a 2D structure).
-- Returns a map from "co-ordinates" to the items in the list.
-- For example:
--     Input: [[a,b,c],[d,e]]
--     Output: Map.fromList [((0,0),a), ((0,1),b), ((0,2),c), ((1,0),d), ((1,1),e)]
mapFromNestedLists :: (Ord a) => [[a]] -> Map (Int, Int) a
mapFromNestedLists = Map.fromList . attachCoords 0 0
  where
    attachCoords _ _ [] = []
    attachCoords x _ ([] : ls) = attachCoords (x + 1) 0 ls
    attachCoords x y ((l : ls) : lss) = ((x, y), l) : attachCoords x (y + 1) (ls : lss)

-- Splits a list into chunks of the specified size.
-- The final chunk may be smaller than the chunk size.
-- Chunk size must be positive.
chunksOf :: Int -> [a] -> [[a]]
chunksOf n ls
  | n <= 0 = error "Cannot split into chunks of negative length."
  | null ls = []
  | length ls < n = [ls]
  | otherwise = take n ls : chunksOf n (drop n ls)

-- Splits a list into maximal contiguous chunks that satisfy the given predicate.
-- For example:
--     Input: (> 3) [5,4,3,2,7,6,3,4]
--     Output: [[5,4],[7,6],[4]]
chunksByPredicate :: (a -> Bool) -> [a] -> [[a]]
chunksByPredicate p ls
  | null ls = []
  | otherwise =
    let (prefix, rest) = span p ls
     in if null prefix
          then chunksByPredicate p $ dropWhile (not . p) rest
          else prefix : chunksByPredicate p (dropWhile (not . p) rest)

-- Allows the user to log out some context and then the result of some expression
-- For example, supposing a is 2, and b is 5:
--     Input: traceShowIdWithContext (a, b) $ a + b
--     Output: (2, 5)	7
traceShowIdWithContext :: (Show a, Show b) => a -> b -> b
traceShowIdWithContext context result = trace (show context ++ "\t" ++ show result) result

-- Like !!, but with bounds checking
(!!?) :: [a] -> Int -> Maybe a
list !!? index =
  if
      | index < 0 -> Nothing
      | index >= length list -> Nothing
      | otherwise -> Just $ list !! index

-- Given a map where the keys are co-ordinates, returns the minimum x, maximum x, minimum y, and maximum y; in that order.
mapBoundingBox :: Map (Int, Int) a -> (Int, Int, Int, Int)
mapBoundingBox m =
  (,,,)
    (minimum . fmap fst . Map.keys $ m)
    (maximum . fmap fst . Map.keys $ m)
    (minimum . fmap snd . Map.keys $ m)
    (maximum . fmap snd . Map.keys $ m)


converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

-- Tuples

pairWith :: (a -> b) -> [a] -> [(a,b)]
pairWith f = fmap (\x -> (x,f x))


pair :: a -> (a,a)
pair x = (x,x) 

both :: Bifunctor x => (a -> b) -> x a a -> x b b 
both f = bimap f f

fst3 :: (a -> d) -> (a, b, c) -> (d, b, c)
fst3 f (a, b, c) = (f a, b, c)

snd3 :: (b -> d) -> (a, b, c) -> (a, d, c)
snd3 f (a, b, c) = (a, f b, c)

third3 :: (c -> d) -> (a, b, c) -> (a, b, d)
third3 f (a, b, c) = (a, b, f c)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

hammingDist :: (Int,Int) -> (Int,Int) -> Int 
hammingDist (a,b) (x,y) = abs (x-a) + abs (y-b)

-- Lists

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False


-- Looping

-- | A looping operation, where the predicate returns 'Left' as a seed for the next loop
--   or 'Right' to abort the loop.
--
-- > loop (\x -> if x < 10 then Left $ x * 2 else Right $ show x) 1 == "16"
loop :: (a -> Either a b) -> a -> b
loop act x = case act x of
    Left x -> loop act x
    Right v -> v

range :: (Num a, Enum a, Ord a) => a -> a -> [a]
range x y = if y >= x then [x..y] else [x,x-1..y]

(...) :: (Num a, Enum a, Ord a) => a -> a -> [a]
(...) = range


-- FUNCTIONS
fpow :: (a -> a) -> Int -> a -> a
fpow f n = foldr1 (.) $ replicate n f