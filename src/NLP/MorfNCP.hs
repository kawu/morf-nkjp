{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module NLP.MorfNCP
( temp
) where


import           Control.Monad (forM_)
-- import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import qualified Data.Tagset.Positional as P

import qualified NLP.MorfNCP.Base as Base
import qualified NLP.MorfNCP.Show as Show
import qualified NLP.MorfNCP.NCP as NCP
-- import qualified NLP.MorfNCP.MSD as MSD
import qualified NLP.MorfNCP.Morfeusz as Morf


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
temp :: FilePath -> FilePath -> IO ()
temp tagsetPath ncpPath = do

  tagset <- P.parseTagset tagsetPath <$> readFile tagsetPath
  let parseTag x = if x == "ign"
        then Nothing
        else Just $ P.parseTag tagset x
      showTag may = case may of
        Nothing -> "ign"
        Just x  -> P.showTag tagset x

  xs <- NCP.getSentences ncpPath
  forM_ xs $ \sent -> do
    -- putStrLn ">>>"
    let orth = NCP.showSent sent
    -- L.putStrLn orth >> putStrLn ""

    -- mapM_ print $ Morf.analyze orth
    -- let dag = map (fmap Morf.fromToken) $ Morf.analyze orth
    -- let dag = map (fmap $ fmap MSD.parseMSD' . Morf.fromToken) $ Morf.analyze orth
    let dagMorf
          = Base.recalcIxsLen
          . map (fmap $ fmap parseTag . Morf.fromToken)
          $ Morf.analyze orth
    -- putStrLn "Morfeusz:"
    -- mapM_ print dagMorf >> putStrLn ""

    let dagNCP
          = Base.recalcIxsLen
          . Base.fromList
          . map (fmap parseTag)
          $ NCP.fromSent sent
    -- let dag = Base.fromList . map (fmap MSD.parseMSD') $ NCP.fromSent sent
    -- let dag = Base.fromList $ NCP.fromSent sent
    -- putStrLn "NCP:"
    -- mapM_ print dagNCP  >> putStrLn ""

    let dag
          = Base.recalcIxs1
          . map (fmap $ fmap showTag)
          $ Base.merge [dagMorf, dagNCP]
    L.putStrLn $ Show.showSent dag
    -- putStrLn "Result:"
    -- mapM_ print dag
    -- putStrLn ">>>\n"
