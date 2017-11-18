{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}


-- | Ultimate data representation.


module NLP.MorfNCP.Base
(
-- * Basics
  DAG
, Edge (..)
, Token (..)
, Interp (..)
, fromList

-- * Indices
, recalcIxs
, recalcIxsLen
, recalcIxs1

-- * Merging
, merge
) where


import qualified Data.Set as S
import qualified Data.Char as C
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.MemoCombinators as Memo

import           NLP.Morfeusz (DAG, Edge(..))
import qualified NLP.Morfeusz as Morf


---------------------------------------------------
-- Data Types
---------------------------------------------------


-- | A token with a list of potential interpretations. If the list of
-- interpretations is empty, the token is unknown.
data Token t = Token
  { orth      :: T.Text
  , interps   :: [Interp t]
  } deriving (Show, Functor)


-- | An interpretation of the word.
data Interp t = Interp
  { base   :: T.Text -- ^ Base form
  , msd    :: t      -- ^ Morphosyntactic description
  , choice :: Bool   -- ^ Is it chosen?
  } deriving (Show, Functor)


-- | Create a trivial DAG from a list of elements.
fromList :: [t] -> DAG t
fromList xs =
  [ Edge
    { from = i
    , to = i + 1
    , label = x }
  | (x, i) <- zip xs [0..] ]


------------------------------------------------------
-- Recalculating indices
------------------------------------------------------


-- | Recalculate the `from` and `to` values of the individual labels to reflect
-- the distance from the beginning of the sentence to the given position (with
-- the goal to simplify merging several DAGs).
-- The way distance is calculated depends on the first argument, which
-- computes the length of the token (typical values: the actual length
-- or 1).
recalcIxs :: (Token t -> Int) -> DAG (Token t) -> DAG (Token t)
recalcIxs tokLen dag =
  [ edge
    { from = dist (from edge)
    , to = dist (to edge) }
  | edge <- dag ]
  where
    dist = Memo.integral dist'
    dist' i = maximumDef 0
      [ dist from + tokLen label
      | Edge{..} <- findEdgeTo i dag ]
    maximumDef x xs = case xs of
      [] -> x
      _ -> maximum xs


-- | Run `recalcIxs` with the length of token set to the number of non-space
-- characters inside.
recalcIxsLen :: DAG (Token t) -> DAG (Token t)
recalcIxsLen = recalcIxs $ T.length . T.filter (not . C.isSpace) . orth


-- -- The previous version: simply the length of the orth of a token.
-- recalcIxsLen :: DAG (Token t) -> DAG (Token t)
-- recalcIxsLen = recalcIxs $ T.length . orth


recalcIxs1 :: DAG (Token t) -> DAG (Token t)
recalcIxs1 = recalcIxs $ const 1


-- | Find edge ending on the given position.
findEdgeTo :: Int -> DAG a -> [Edge a]
findEdgeTo i = filter ((==i) . to)


-- oneElem :: (Ord a, Show a, Num a) => [a] -> a
-- oneElem xs = case S.toList (S.fromList xs) of
--   []  -> 0
--   [x] -> x
--   _ -> error $ "oneElem: |" ++ show xs ++ "| <> 1"



------------------------------------------------------
-- Merging
------------------------------------------------------


-- | Merge a list of DAGs.
merge :: (Ord t) => [DAG (Token t)] -> DAG (Token t)
merge dags =
  [ Edge
    { from  = from
    , to    = to
    , label = label }
  | ((from, to), label) <- M.toList dagMap ]
  where
    dagMap = M.fromListWith mergeTok
      [ ((from, to), label)
      | Edge{..} <- concat dags ]
    mergeTok x y = mergeToks [x, y]


-- | Assumption: input list non-empty and all tokens have
-- the same `orth` value.
mergeToks :: (Ord t) => [Token t] -> Token t
mergeToks toks =
  (head toks) {interps = newInterps}
  where
    newInterps =
      [ Interp
        { base = base
        , msd  = msd
        , choice = choice }
      | (msd, (choice, base)) <- M.toList interpsMap ]
    interpsMap = M.fromListWith orBase
      [ (msd, (choice, base))
      | Interp{..} <- concatMap interps toks ]
    -- differences on base values are ignored
    orBase (choice1, base1) (choice2, _base2) = (choice1 || choice2, base1)
