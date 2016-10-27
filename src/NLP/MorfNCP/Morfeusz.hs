{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


-- | Functionality related to Morfeusz.


module NLP.MorfNCP.Morfeusz
( analyze

-- * Conversion
, fromToken

-- * MSD
, unfoldMSD
) where


-- import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as L

import qualified Data.DAG as DAG

import           NLP.Morfeusz (DAG, Edge(..))
import qualified NLP.Morfeusz as Morf

import qualified NLP.MorfNCP.Base as Base


---------------------------------------------------
-- Morfeusz
---------------------------------------------------


-- | Analyze the given sentence.
analyze :: L.Text -> Morf.DAG Morf.Token
analyze = Morf.analyse False . L.toStrict


-- -- | Convert a Morfeusz DAG to `Base.DAG`.
-- fromDAG :: Morf.DAG t -> Base.DAG t
-- fromDAG = id


-- | Convert a Morfeusz token to `Base.Token`. Unfolding (using `unfoldMSD`) is
-- performed during convertion.
fromToken :: Morf.Token -> Base.Token T.Text
fromToken Morf.Token{..} = Base.Token
  { orth = orth
  , interps = concatMap fromInterp interps }
  where
    fromInterp Morf.Interp{..} =
      [ Base.Interp
        { base = base
        , msd = msdAtom
        , choice = False }
      | msdAtom <- unfoldMSD msd ]


---------------------------------------------------
-- MSD decoding
---------------------------------------------------


-- | Unfold Morfeusz MSD in its raw, textual form.
unfoldMSD :: T.Text -> [T.Text]
unfoldMSD
  = map (T.intercalate ":")
  . sequence
  . map (T.split (=='.'))
  . T.split (==':')


---------------------------------------------------
-- Obsolete?
---------------------------------------------------


-- -- | Convert the analysis result to pedestrian DAG representation.
-- convDAG :: Morf.DAG a -> DAG.DAG () a
-- convDAG =
--   DAG.fromEdgesUnsafe . map convEdge
--   where
--     convEdge Morf.Edge{..} = DAG.Edge
--       { tailNode = DAG.NodeID from
--       , headNode = DAG.NodeID to
--       , edLabel = label }
-- 
-- 
-- -- | Analyze the given sentence.
-- analyze :: L.Text -> DAG.DAG () Morf.Token
-- analyze = convDAG . _analyze
-- 
-- 
