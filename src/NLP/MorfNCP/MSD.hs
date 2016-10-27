{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module NLP.MorfNCP.MSD
(
-- * MSD
  MSD (..)

-- * Attributes
, Nmb (..)
, Cas (..)
, Gnd (..)
, Per (..)
, Deg (..)
, Asp (..)
, Ngt (..)
, Acm (..)
, Acn (..)
, Ppr (..)
, Agg (..)
, Vlc (..)
, Dot (..)

-- * Parsing
, parseMSD
, parseMSD'
, msdP

-- * Conversion
) where


import           Control.Applicative ((<$), (*>), (<$>), (<|>), optional)
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Attoparsec.Text as P
import           Data.Attoparsec.Text (Parser)


-- -- | A datatype used to distinguish values which can be present only
-- -- within the context of NCP.
-- data Ncp a
--   = Ncp a
--   | NotN
--
--
-- -- | Like `Ncp` but for Morfeusz.
-- data Mrf a
--   = Mrf a
--   | NotM


-- | A morphosyntactic tag consistent with NCP tagset.
data MSD
  = Adja
  | Adjp
  | Adjc
  | Conj
  | Comp
  | Interp
  | Pred
  | Xxx
  | Adv
    { degNcp :: Maybe Deg
    -- ^ Attribute specific to NCP
    }
  | Imps
    { asp :: Asp }
  | Inf
    { asp :: Asp }
  | Pant
    { asp :: Asp }
  | Pcon
    { asp :: Asp }
  | Qub
    { vlcOpt :: Maybe Vlc }
  | Prep
    { cas :: Cas
    , vlcOpt :: Maybe Vlc }
  | Siebie
    { cas :: Cas }
  | Subst
    { nmb :: Nmb
    , cas :: Cas
    , gnd :: Gnd }
  | Depr
    { nmb :: Nmb
    , cas :: Cas
    , gnd :: Gnd }
  | Ger
    { nmb :: Nmb
    , cas :: Cas
    , gnd :: Gnd
    , asp :: Asp
    , ngt :: Ngt }
  | Ppron12
    { nmb :: Nmb
    , cas :: Cas
    , gnd :: Gnd
    , per :: Per
    , acnOpt :: Maybe Acn }
  | Ppron3
    { nmb :: Nmb
    , cas :: Cas
    , gnd :: Gnd
    , per :: Per
    , acnOpt :: Maybe Acn
    , pprOpt :: Maybe Ppr }
  | Num
    { nmb :: Nmb
    , cas :: Cas
    , gnd :: Gnd
    , acm :: Acm }
  | Numcol
    { nmb :: Nmb
    , cas :: Cas
    , gnd :: Gnd
    , acm :: Acm }
  | Adj
    { nmb :: Nmb
    , cas :: Cas
    , gnd :: Gnd
    , deg :: Deg }
  | Pact
    { nmb :: Nmb
    , cas :: Cas
    , gnd :: Gnd
    , asp :: Asp
    , ngt :: Ngt }
  | Ppas
    { nmb :: Nmb
    , cas :: Cas
    , gnd :: Gnd
    , asp :: Asp
    , ngt :: Ngt }
  | Winien
    { nmb :: Nmb
    , gnd :: Gnd
    , asp :: Asp }
  | Praet
    { nmb :: Nmb
    , gnd :: Gnd
    , asp :: Asp
    , aggOpt :: Maybe Agg }
  | Bedzie
    { nmb :: Nmb
    , per :: Per
    , asp :: Asp }
  | Fin
    { nmb :: Nmb
    , per :: Per
    , asp :: Asp }
  | Impt
    { nmb :: Nmb
    , per :: Per
    , asp :: Asp }
  | Aglt
    { nmb :: Nmb
    , per :: Per
    , asp :: Asp
    , vlc :: Vlc }
  | Brev
    { dot :: Dot }
  | Burk
  | Interj
  -- below, classes specific to Morfeusz
  | Dig_Morf
  | Romandig_Morf
  | Emo_Morf
  deriving (Show, Eq, Ord)


---------------------------------------------------
-- Attributes
---------------------------------------------------


data Nmb = Sg | Pl
  deriving (Show, Eq, Ord)

data Cas = Nom | Gen | Dat | Acc | Inst | Loc | Voc
  deriving (Show, Eq, Ord)

-- | Gender; certain values specific to Morfeusz or NCP.
data Gnd
  = M1
  | M2
  | M3
  | F
  | N_Ncp
  | N1_Morf
  | N2_Morf
  | P1_Morf
  | P2_Morf
  | P3_Morf
  | Space_Morf
  deriving (Show, Eq, Ord)

data Per = Pri | Sec | Ter
  deriving (Show, Eq, Ord)
data Deg = Pos | Com | Sup
  deriving (Show, Eq, Ord)
data Asp = Imperf | Perf
  deriving (Show, Eq, Ord)
data Ngt = Aff | Neg
  deriving (Show, Eq, Ord)
data Acm = Congr | Rec
  deriving (Show, Eq, Ord)
data Acn = Akc | Nakc
  deriving (Show, Eq, Ord)
data Ppr = Npraep | Praep
  deriving (Show, Eq, Ord)
data Agg = Agl | Nagl
  deriving (Show, Eq, Ord)
data Vlc = Nwok | Wok
  deriving (Show, Eq, Ord)
data Dot = Pun | Npun
  deriving (Show, Eq, Ord)


---------------------------------------------------
-- Parsing
---------------------------------------------------


-- | Parse MSD.
parseMSD :: T.Text -> MSD
parseMSD x = case P.parseOnly (msdP <* P.endOfInput) x of
  Left err -> error $ unlines
    [ "parseMSD: failed while parsing " ++ T.unpack x
    , "error: " ++ err ]
  Right msd -> msd


-- | Parse MSD while taking into account the potential "ign" tags
-- (which lead to `Nothing`).
parseMSD' :: T.Text -> Maybe MSD
parseMSD' "ign" = Nothing
parseMSD' x = Just (parseMSD x)


-- | MSD parser.
msdP :: Parser MSD
msdP = F.asum
  [ mk Adja "adja"
  , mk Adjp "adjp"
  , mk Adjp "adjc"
  , mk Conj "conj"
  , mk Comp "comp"
  , mk Interp "interp"
  , mk Pred "pred"
  , mk Xxx "xxx"
  , mk Adv "adv" <?> degP
  , mk Imps "imps" <&> aspP
  , mk Inf "inf" <&> aspP
  , mk Pant "pant" <&> aspP
  , mk Pcon "pcon" <&> aspP
  , mk Qub "qub" <?> vlcP
  , mk Prep "prep" <&> casP <?> vlcP
  , mk Siebie "siebie" <&> casP
  , mk Subst "subst" <&> nmbP <&> casP <&> gndP
  , mk Depr "depr" <&> nmbP <&> casP <&> gndP
  , mk Ger "ger" <&> nmbP <&> casP <&> gndP <&> aspP <&> ngtP
  , mk Ppron12 "ppron12" <&> nmbP <&> casP <&> gndP <&> perP <?> acnP
  , mk Ppron3 "ppron3" <&> nmbP <&> casP <&> gndP <&> perP <?> acnP <?> pprP
  , mk Num "num" <&> nmbP <&> casP <&> gndP <&> acmP
  , mk Numcol "numcol" <&> nmbP <&> casP <&> gndP <&> acmP
  , mk Adj "adj" <&> nmbP <&> casP <&> gndP <&> degP
  , mk Pact "pact" <&> nmbP <&> casP <&> gndP <&> aspP <&> ngtP
  , mk Ppas "ppas" <&> nmbP <&> casP <&> gndP <&> aspP <&> ngtP
  , mk Winien "winien" <&> nmbP <&> gndP <&> aspP
  , mk Praet "praet" <&> nmbP <&> gndP <&> aspP <?> aggP
  , mk Bedzie "bedzie" <&> nmbP <&> perP <&> aspP
  , mk Fin "fin" <&> nmbP <&> perP <&> aspP
  , mk Impt "impt" <&> nmbP <&> perP <&> aspP
  , mk Aglt "aglt" <&> nmbP <&> perP <&> aspP <&> vlcP
  , mk Brev "brev" <&> dotP
  , mk Burk "burk"
  , mk Interj "interj"
  -- below, specific to Morfeusz
  , mk Dig_Morf "dig"
  , mk Romandig_Morf "romandig"
  , mk Emo_Morf "emo" ]
  where
    m <&> n = m <*> (colon *> n)
    m <?> n = m <*> optional (colon *> n)
    colon = P.char ':'


nmbP = mk Sg "sg" <|> mk Pl "pl"
casP = mk Nom "nom" <|> mk Gen "gen" <|> mk Dat "dat" <|> mk Acc "acc"
  <|> mk Inst "inst" <|> mk Loc "loc" <|> mk Voc "voc"

gndP
   =  mk M1 "m1"
  <|> mk M2 "m2"
  <|> mk M3 "m3"
  <|> mk F "f"
  <|> mk N1_Morf "n1"
  <|> mk N2_Morf "n2"
  <|> mk N_Ncp "n"
  <|> mk P1_Morf "p1"
  <|> mk P2_Morf "p2"
  <|> mk P3_Morf "p3"
  <|> mk Space_Morf "_"


perP = mk Pri "pri" <|> mk Sec "sec" <|> mk Ter "ter"
degP = mk Pos "pos" <|> mk Com "com" <|> mk Sup "sup"
aspP = mk Imperf "imperf" <|> mk Perf "perf"
ngtP = mk Aff "aff" <|> mk Neg "neg"
acnP = mk Akc "akc" <|> mk Nakc "nakc"
acmP = mk Congr "congr" <|> mk Rec "rec"
pprP = mk Npraep "npraep" <|> mk Praep "praep"
aggP = mk Agl "agl" <|> mk Nagl "nagl"
vlcP = mk Nwok "nwok" <|> mk Wok "wok"
dotP = mk Pun "pun" <|> mk Npun "npun"


mk :: a -> T.Text -> Parser a
mk cst str = cst <$ P.string str


---------------------------------------------------
-- Conversion
---------------------------------------------------


-- | Simplify MSD.  What is important at the moment is that
-- simplified MSDs are both Morfeusz and NCP compliant.
simplifyMSD :: MSD -> MSD
simplifyMSD msd = case msd of
  Adv{..} -> Adv Nothing
  Numcol{..} -> updGnd $ Num
    { nmb = nmb
    , cas = cas
    , gnd = gnd
    , acm = acm }
  Dig_Morf -> Interp
  Romandig_Morf -> Interp
  Emo_Morf -> Interp
  _ -> if hasGnd msd
       then updGnd msd
       else msd
  where
    updGnd msd = msd {gnd = simplifyGnd (gnd msd)}


-- | Simplify gender.
simplifyGnd :: Gnd -> Gnd
simplifyGnd x = case x of
  N_Ncp -> N_Ncp
  N1_Morf -> N_Ncp
  N2_Morf -> N_Ncp
  P1_Morf -> M1
  P2_Morf -> N_Ncp
  P3_Morf -> N_Ncp
  _ -> x


-- | Does it have a gender?
hasGnd :: MSD -> Bool
hasGnd msd = case msd of
  Subst{} -> True
  Depr{} -> True
  Ger{} -> True
  Ppron12{} -> True
  Ppron3{} -> True
  Num{} -> True
  Numcol{} -> True
  Adj{} -> True
  Pact{} -> True
  Ppas{} -> True
  Winien{} -> True
  Praet{} -> True
  Praet{} -> True
  _ -> False


-- -- | Convert a NCP tag to one or more Morfeusz tags.
-- morfMSD :: MSD -> [MSD]
-- morfMSD msd
--   | Adv{..} = [Adv Nothing]
--   | Numcol{} = updGnd msd
--   | hasGnd msd = updGnd msd
--   | otherwise = [msd]
--   where
--     updGnd msd =
--       [ msd {gnd = newGnd}
--       | newGnd <- morfGnd (gnd msd) ]
-- 
-- 
-- -- | Make the gender value Morfeusz compliant.
-- morfGnd :: Gnd -> [Gnd]
-- morfGnd gnd = case gnd of
--   N_Ncp -> [N1_Morf, N2_Morf, P2_Morf, P3_Morf]
--   M1 -> [M1, P1_Morf]
--   x -> [x]
