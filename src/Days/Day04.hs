module Days.Day04 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Either
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Control.Monad
import Control.Monad.Loops
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  ss <- decimal `sepBy` char ','
  skipSpace
  let block = mkBoard <$> (replicateM 5 $ replicateM 5 (decimal <* skipSpace))
  bs <- block `sepBy` skipSpace
  pure (ss, bs)


------------ TYPES ------------
type Input = ([Int], [Board])

newtype Board = Board [[(Int, Bool)]]
mkBoard :: [[Int]] -> Board
mkBoard = Board . map (map (,False))

instance Show Board where
  show (Board b) = 
    "\n" <> (unlines $ map unwords $ fmap (fmap (show.fst)) b) <> "\n"


------------ PART A ------------
partA :: Input -> Int
partA = score . U.loop (markBoards $ find isDone)

{-
  Markboards uses the "loop" function in Utils to repeat an action until
  it finds a Right value. It will find a Right value when the "selectBoard"
  function gives back a Just value. 
  
  Hence (markBoards $ find isDone) will keep marking boards until at least
  one board is done, then give back the first done board and the score 
  that marked it.
-}
markBoards :: ([Board] -> Maybe Board) -> Input -> Either Input ([Int],Board)
markBoards selectBoard (s:ss, bs) 
  = case selectBoard mbs of
      Nothing -> Left (ss, mbs)
      Just mb -> Right (s:ss, mb)
  where 
    mbs = fmap (markBoard s) bs

markBoard :: Int -> Board -> Board
markBoard s (Board b) 
  = Board [ [(x,m') | (x,m) <- r, let m' = m || x == s] | r <- b]

isDone :: Board -> Bool
isDone (Board b) = any (any (and . map snd)) [b, transpose b]

score :: ([Int], Board) -> Int
score (s:_, Board b) = s * (sum $ map fst $ filter (not.snd) $ concat b)


------------ PART B ------------
partB :: Input -> Int
partB = score . U.loop markOneBoard . U.loop (markBoards $ lastBoard)
  where 
    lastBoard bs = case filter (not.isDone) bs of
      [b] -> Just b
      _   -> Nothing
    markOneBoard :: ([Int], Board) -> Either ([Int], Board) ([Int], Board) 
    markOneBoard (s:ss, b) 
      = let b' = markBoard s b
        in if isDone b' then Right (s:ss, b') else Left (ss, b')
