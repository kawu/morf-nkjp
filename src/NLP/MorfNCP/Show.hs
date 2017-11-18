{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module NLP.MorfNCP.Show
( showSent
, showData
) where


import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L
import           Data.List (intersperse)
import           Text.Printf (printf)

-- import qualified NLP.MorfNCP.Base as Base
import           NLP.MorfNCP.Base


type Sent = DAG (Token T.Text)


-- | Show the given sentence.
showData :: [Sent] -> L.Text
showData
  = L.toLazyText
  . mconcat
  . intersperse "\n"
  . map buildSent


-- | Show the given sentence.
showSent :: Sent -> L.Text
showSent = L.toLazyText . buildSent


buildSent :: Sent -> L.Builder
buildSent dag = finalize $ do
  Edge{..} <- dag
  Interp{..} <- interps label
  return $ mconcat $ intersperse "\t"
    [ buildNode from
    , buildNode to
    , text (orth label)
    , text base
    , text msd
    , "", ""
    , buildDmb $ if choice then 1 :: Double else 0
    , "", ""
    ]
  where
    text = L.fromText . T.strip
    finalize = (`mappend` "\n") . mconcat . intersperse "\n"
    buildNode i = L.fromString (show i)
    buildDmb = L.fromString . printf "%.4f"
