{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module NLP.MorfNCP
(
-- * NCP
  Sent
, getSentences
, getLeaves
, showLeaves
, showSent

-- * Morfeusz
, analyze

-- * Temp
, temp
) where


import           Control.Applicative ((<$>))
import           Control.Monad (forM_)
-- import           System.Environment (getArgs)
import           Data.Foldable (foldMap)
import           Data.Text.Lazy (Text)
import qualified Data.Tree as Tree
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import           Data.DAG (DAG)
import qualified Data.DAG as DAG

import qualified Text.NKJP.Named as Ne
import qualified Text.NKJP.Morphosyntax as Mx
import qualified NLP.Morfeusz as Morf


---------------------------------------------------
-- NCP
---------------------------------------------------


-- | A sentence with NEs and segments.
type Sent = Tree.Forest (Either (Ne.NE Text) (Mx.Seg Text))


-- | Transform sentence into a list of segments.
getLeaves :: Sent -> [Mx.Seg Text]
getLeaves =
  concatMap (foldMap getRight)
  where
    getRight (Right x) = [x]
    getRight _         = []


-- | Show a list of segments.
showLeaves :: [Mx.Seg Text] -> Text
showLeaves =
  L.concat . map showLeaf
  where
    showLeaf x = if Mx.nps x
      then Mx.orth x
      else " " `L.append` Mx.orth x


-- | Show the sentence.
showSent :: Sent -> Text
showSent = L.strip . showLeaves . getLeaves


-- | Parse NCP and retrieve the list of sentences.
getSentences :: FilePath -> IO [Sent]
getSentences teiPath = concat <$> Ne.readTrees [] teiPath


---------------------------------------------------
-- Morfeusz
---------------------------------------------------


-- | Analyze the given sentence.
_analyze :: Text -> Morf.DAG Morf.Token
_analyze = Morf.analyse False . L.toStrict


-- | Convert the analysis result to pedestrian DAG representation.
convDAG :: Morf.DAG a -> DAG () a
convDAG =
  DAG.fromEdgesUnsafe . map convEdge
  where
    convEdge Morf.Edge{..} = DAG.Edge
      { tailNode = DAG.NodeID from
      , headNode = DAG.NodeID to
      , edLabel = label }


-- | Analyze the given sentence.
analyze :: Text -> DAG () Morf.Token
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
  xs <- getSentences ncpPath
  forM_ xs $ \sent -> do
    putStrLn ">>>"
    let orth = showSent sent
    L.putStrLn orth >> putStrLn ""

    putStrLn "Morfeusz:"
    mapM_ print $ _analyze orth
    putStrLn ""

    putStrLn "NCP:"
    mapM_ print $ getLeaves sent
    putStrLn ">>>\n"
