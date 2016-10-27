{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


-- | Functionality related to NCP.


module NLP.MorfNCP.NCP
(
-- * Basic
  Sent
, getSentences
, getLeaves
, showLeaves
, showSent

-- * Conversion
, fromSent
) where


import           Control.Applicative ((<$), (*>), (<$>), (<|>), optional)
import qualified Data.Foldable as F
import qualified Data.Tree as Tree
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Attoparsec.Text as P
import           Data.Attoparsec.Text (Parser)

import qualified Text.NKJP.Named as Ne
import qualified Text.NKJP.Morphosyntax as Mx

import qualified NLP.MorfNCP.Base as Base
-- import           NLP.MorfNCP.DAG (DAG)


---------------------------------------------------
-- NCP
---------------------------------------------------


-- | A sentence with NEs and segments.
type Sent = Tree.Forest (Either (Ne.NE L.Text) (Mx.Seg L.Text))


-- | Transform sentence into a list of segments.
getLeaves :: Sent -> [Mx.Seg L.Text]
getLeaves =
  concatMap (F.foldMap getRight)
  where
    getRight (Right x) = [x]
    getRight _         = []


-- | Show a list of segments.
showLeaves :: [Mx.Seg L.Text] -> L.Text
showLeaves =
  L.concat . map showLeaf
  where
    showLeaf x = if Mx.nps x
      then Mx.orth x
      else " " `L.append` Mx.orth x


-- | Show the sentence.
showSent :: Sent -> L.Text
showSent = L.strip . showLeaves . getLeaves


-- | Parse NCP and retrieve the list of sentences.
getSentences :: FilePath -> IO [Sent]
getSentences teiPath = concat <$> Ne.readTrees [] teiPath


---------------------------------------------------
-- NCP
---------------------------------------------------


-- | Convert a NCP sentence to a DAG.
fromSent :: Sent -> [Base.Token T.Text]
fromSent = map (fromSeg . fmap L.toStrict) . getLeaves


-- | Convert a NCP segment to Base.Token.
fromSeg :: Mx.Seg T.Text -> Base.Token T.Text
fromSeg Mx.Seg{..} = Base.Token
  { orth = orth
  , interps = fromLexs (fst choice) lexs }


-- | Convert a list of lexical interpretations to a list of `Base.Interp`s.
fromLexs
  :: T.Text -- ^ ID of the chosen MSD
  -> [Mx.Lex T.Text]
  -> [Base.Interp T.Text]
fromLexs choiceID lexs =
  [ Base.Interp
    { base = base
    , msd  = if T.null msd
             then ctag
             else T.concat [ctag, ":", msd]
    , choice = choiceID == msdID }
  | Mx.Lex{..} <- lexs
  , (msdID, msd) <- msds ]
