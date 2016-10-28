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
          -- information that there is no space *after* the given word;
          -- in NCP there's info that there is nsp *before* the given word.
          let npss = (++[False]) . tail . map Mx.nps . Mx.segments $ sent
          F.forM_ (zip3 [1..] npss $ Mx.segments sent) $ \(ix, nps, seg) -> do
            let interp = getChoice (fst $ Mx.choice seg) (Mx.lexs seg)
                may = maybe "_" id
                ctagStr = may $ ctag <$> interp
                baseStr = may $ base <$> interp
                msdStr  = may $ mmsd =<< interp
            L.putStrLn $ L.intercalate "\t"
              [ L.pack (show ix)
              , Mx.orth seg
              , if nps then "nsp" else "_"
              , baseStr
              , ctagStr
              , msdStr
              , L.pack $ pathCore path
              , Mx.paraID para
              , Mx.sentID sent
              , Mx.segID seg ]
          putStrLn ""


-- | Morphosyntactic interpretation.
data Interp = Interp
  { base :: L.Text
  , ctag :: L.Text
  , mmsd :: Maybe L.Text }


-- | Convert a list of lexical interpretations to a list of `Base.Interp`s.
getChoice
  :: L.Text -- ^ ID of the chosen MSD
  -> [Mx.Lex L.Text]
  -> Maybe Interp
getChoice choiceID lexs = listToMaybe
  [ Interp
    { base = base
    , ctag = ctag
    , mmsd = if L.null msd
             then Nothing
             else Just msd }
  | Mx.Lex{..} <- lexs
  , (msdID, msd) <- msds
  , choiceID == msdID ]


-- | Get the core of the NCP path.
pathCore :: FilePath -> FilePath
pathCore = reverse . drop 1 . reverse . (!!1) . Path.splitPath
