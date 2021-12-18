module Days.Day18 where

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

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Control.Applicative
import Control.Applicative.Combinators (between)
import Util.Parsers
import Util.Util
import Control.Monad.State
import qualified Data.Text as Text
import Debug.Trace
import Data.Text (Text)
import Data.Functor
import Data.Function
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = homeworkP `sepBy` skipSpace

homeworkP :: Parser Homework
homeworkP = (Val <$> decimal)
            <|> between "[" "]" (uncurry Pair <$> homeworkP `around` ",")

------------ TYPES ------------
type Input = [Homework]

data Tree a = Val a | Pair (Tree a) (Tree a)
  deriving (Eq, Functor, Foldable, Traversable)

type Homework = Tree Int

instance Show a => Show (Tree a) where
  show (Val x) = show x
  show (Pair x y) = concat ["[", show x, ",", show y ,"]"]
instance {-# OVERLAPS #-} Show a => Show [Tree a] where
  show = unlines . map show

type Depth = Int

data ReduceState = ReduceState
  { propL :: Maybe Int
  , propR :: Maybe Int
  , depth :: Int
  , done  :: Bool
  }
  
defRS :: ReduceState
defRS  = ReduceState Nothing Nothing 0 False

type RS = State ReduceState


------------ PART A ------------
partA :: Input -> Int
partA = magnitude . reduce . foldl1' (\x y -> reduce $ Pair x y)

magnitude :: Homework -> Int
magnitude (Val v) = v
magnitude (Pair x y) = 3 * magnitude x + 2 * magnitude y

reduce :: Homework -> Homework
reduce = converge (st reduceSplit . converge (st reduceExplode))

  where
    st :: (Homework -> RS Homework) -> Homework -> Homework
    st f h = evalState (f h) defRS

    reduceExplode :: Homework -> RS Homework
    reduceExplode (Val v) = pure (Val v)
    reduceExplode (Pair (Val x) (Val y)) = do
      d <- gets depth
      if d >= 4
        then do
          setPropL (Just x)
          setPropR (Just y)
          setDone
          pure $ Val 0
        else pure $ Pair (Val x) (Val y)
    reduceExplode (Pair l r) = do
      l' <- nest $ reduceExplode l
      gets propR >>= \case
        Just n -> do
          setPropR Nothing 
          pure $ Pair l' (adjustR (+ n) r)
        Nothing -> gets done >>= \case
          True -> pure $ Pair l' r
          False -> do
            r' <- nest $ reduceExplode r
            pL <- gets propL
            case pL of 
              Just n -> do
                setPropL Nothing
                pure $ Pair (adjustL (+ n) l') r' 
              Nothing -> pure $ Pair l' r'

    reduceSplit :: Homework -> RS Homework
    reduceSplit (Val v)
      | v >= 10 = do
        setDone
        pure $ Pair (Val (v `div` 2)) (Val (v - (v `div` 2)))
      | otherwise = pure $ Val v

    reduceSplit (Pair l r) = do
      l' <- reduceSplit l
      d <- gets done
      if d 
        then pure $ Pair l' r
        else Pair l' <$> reduceSplit r

    setPropL t = modify $ \x -> x { propL = t    }
    setPropR t = modify $ \x -> x { propR = t    }
    setDone    = modify $ \x -> x { done  = True }

    nest :: RS a -> RS a
    nest s = modify (\x -> x { depth = depth x + 1 }) *> s <*
             modify (\x -> x { depth = depth x - 1 })

    adjustL, adjustR :: (Int -> Int) -> Homework -> Homework
    adjustL f (Val x)    = Val (f x)
    adjustL f (Pair x y) = Pair x (adjustL f y)
    adjustR f (Val x)    = Val (f x)
    adjustR f (Pair x y) = Pair (adjustR f x) y

------------ PART B ------------
partB :: Input -> Int
partB hs = maximum [magnitude $ reduce $ Pair a b | a <- hs, b <- hs]