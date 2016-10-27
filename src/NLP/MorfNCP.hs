{-# LANGUAGE RecordWildCards #-}


module NLP.MorfNCP
(
-- * Morfeusz
  analyze

-- * Temp
, temp
) where


import           Control.Applicative ((<$>))
import           Control.Monad (forM_)
-- import           System.Environment (getArgs)
-- import           Data.Text.Lazy (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

-- import           Data.DAG (DAG)
import qualified Data.DAG as DAG

import           NLP.Morfeusz (DAG, Edge(..))
import qualified NLP.Morfeusz as Morf
import qualified Text.NKJP.Morphosyntax as Mx

import qualified NLP.MorfNCP.DAG as Base
import qualified NLP.MorfNCP.NCP as NCP


---------------------------------------------------
-- Morfeusz
---------------------------------------------------


-- | Analyze the given sentence.
_analyze :: L.Text -> Morf.DAG Morf.Token
_analyze = Morf.analyse False . L.toStrict


-- | Convert the analysis result to pedestrian DAG representation.
convDAG :: Morf.DAG a -> DAG.DAG () a
convDAG =
  DAG.fromEdgesUnsafe . map convEdge
  where
    convEdge Morf.Edge{..} = DAG.Edge
      { tailNode = DAG.NodeID from
      , headNode = DAG.NodeID to
      , edLabel = label }


-- | Analyze the given sentence.
analyze :: L.Text -> DAG.DAG () Morf.Token
analyze = convDAG . _analyze


---------------------------------------------------
-- Reconcile
---------------------------------------------------


-- -- | Reconcile the tags chosen in NCP with the output graph generated
-- -- by Morfeusz.
-- reconcile
--   :: [Mx.Seg Text]
--   -- ^ Sentence in NCP
--   -> DAG () (Morf.Token)

---------------------------------------------------
-- Temp
---------------------------------------------------


-- | Temporary function.
temp :: FilePath -> IO ()
temp ncpPath = do
  xs <- NCP.getSentences ncpPath
  forM_ xs $ \sent -> do
    putStrLn ">>>"
    let orth = NCP.showSent sent
    L.putStrLn orth >> putStrLn ""

--    putStrLn "Morfeusz:"
--    mapM_ print $ _analyze orth
--    putStrLn ""

    putStrLn "NCP:"
    let dag = Base.fromList . map (fmap NCP.parseMSD') $ NCP.fromSent sent
    -- let dag = Base.fromList $ NCP.fromSent sent
    mapM_ print dag
    putStrLn ">>>\n"
