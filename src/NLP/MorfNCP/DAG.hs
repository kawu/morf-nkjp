{-# LANGUAGE DeriveFunctor #-}


-- | Ultimate data representation.


module NLP.MorfNCP.DAG
( DAG
, Edge (..)
, Token (..)
, Interp (..)

, fromList
) where


import qualified Data.Text as T

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
