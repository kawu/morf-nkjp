{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


-- | Manipulating morphosyntactic descriptions.


module NLP.MorfNCP.MSD
( SimpMap
, mainSimpMap
, simpWith
, simpAdv
, simplify
) where


import           Control.Arrow (second)
import           Control.Monad (guard)
import qualified Data.List as List
import qualified Data.Map.Strict as M

import qualified Data.Tagset.Positional as P


-- | Attribute simplification map.
type SimpMap = M.Map P.Attr (M.Map P.AttrVal P.AttrVal)


-- | Simplification table.
mainSimpMap :: SimpMap
mainSimpMap = mkMap
  [ ("gnd", gndMap) ]
  where
    mkMap = M.fromList . map (second M.fromList)
    gndMap =
      [ ("n1", "n")
      , ("n2", "n")
      , ("p1", "m1")
      , ("p2", "n")
      , ("p3", "n") ]


-- | Simplify with the given simplification table.
simpWith :: SimpMap -> P.Tagset -> P.Tag -> P.Tag
simpWith simpMap tagset tag0 =
  List.foldl' simpOne tag0 (M.toList simpMap)
  where
    simpOne tag (attr, valMap) = maybe tag id $ do
      val <- M.lookup attr (P.atts tag)
      newVal <- M.lookup val valMap
      let newAtts = M.insert attr newVal (P.atts tag)
      return $ tag {P.atts = newAtts}


-- | Simplify adverb (if it is an adverb).
simpAdv :: P.Tag -> P.Tag
simpAdv tag = maybe tag id $ do
  guard $ P.pos tag == "adv"
  let newAtts = M.delete "deg" (P.atts tag)
  return $ tag {P.atts = newAtts}


-- | Default simplification config.
simplify :: P.Tagset -> P.Tag -> P.Tag
simplify tagset
  = simpAdv
  . simpWith mainSimpMap tagset
