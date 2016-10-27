{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module NLP.MorfNCP.Dummy
( dummy
) where


import           Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.Foldable as F
import qualified System.FilePath.Posix as Path


import qualified Text.NKJP.Morphosyntax as Mx


---------------------------------------------------
-- Dummy
---------------------------------------------------


-- | Temporary function.
dummy :: FilePath -> IO ()
dummy ncpPath = do
  xs <- Mx.readCorpus [] ncpPath
  F.forM_ xs $ \(path, maybeParas) -> do
    F.forM_ maybeParas $ \paras -> do
      F.forM_ paras $ \para -> do
        F.forM_ (Mx.sentences para) $ \sent -> do
          F.forM_ (zip [1..] $ Mx.segments sent) $ \(ix, seg) -> do
            let (ctag, msd) = case getMSD (fst $ Mx.choice seg) (Mx.lexs seg) of
                  Nothing -> ("_", "_")
                  Just (x, mayMsd) -> (x, case mayMsd of
                                            Nothing -> "_"
                                            Just y  -> y )
            L.putStrLn $ L.intercalate "\t"
              [ L.pack (show ix)
              , Mx.orth seg
              , if Mx.nps seg then "nsp" else "_"
              , ctag, msd
              , L.pack $ pathCore path
              , Mx.paraID para
              , Mx.sentID sent
              , Mx.segID seg ]
          putStrLn ""


-- | Convert a list of lexical interpretations to a list of `Base.Interp`s.
getMSD
  :: L.Text -- ^ ID of the chosen MSD
  -> [Mx.Lex L.Text]
  -> Maybe (L.Text, Maybe L.Text)
getMSD choiceID lexs = listToMaybe
  [ if L.null msd
    then (ctag, Nothing)
    else (ctag, Just msd)
  | Mx.Lex{..} <- lexs
  , (msdID, msd) <- msds
  , choiceID == msdID ]


-- | Get the core of the NCP path.
pathCore :: FilePath -> FilePath
pathCore = reverse . drop 1 . reverse . (!!1) . Path.splitPath
