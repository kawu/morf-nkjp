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
-- , fromSeg

-- * Tag
-- ** MSD
, MSD (..)
-- ** Attributes
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
-- ** Parsing
, parseMSD
, parseMSD'
, msdP
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

import qualified NLP.MorfNCP.DAG as DAG
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
fromSent :: Sent -> [DAG.Token T.Text]
fromSent = map (fromSeg . fmap L.toStrict) . getLeaves


-- | Convert a NCP segment to DAG.Token.
fromSeg :: Mx.Seg T.Text -> DAG.Token T.Text
fromSeg Mx.Seg{..} = DAG.Token
  { orth = orth
  , interps = fromLexs (fst choice) lexs }


-- | Convert a list of lexical interpretations to a list of `DAG.Interp`s.
fromLexs
  :: T.Text -- ^ ID of the chosen MSD
  -> [Mx.Lex T.Text]
  -> [DAG.Interp T.Text]
fromLexs choiceID lexs =
  [ DAG.Interp
    { base = base
    , msd  = if T.null msd
             then ctag
             else T.concat [ctag, ":", msd]
    , choice = choiceID == msdID }
  | Mx.Lex{..} <- lexs
  , (msdID, msd) <- msds ]


---------------------------------------------------
-- MSD Tag
---------------------------------------------------


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
    { degOpt :: Maybe Deg }
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
  deriving (Show, Eq, Ord)


---------------------------------------------------
-- Attributes
---------------------------------------------------


data Nmb = Sg | Pl
  deriving (Show, Eq, Ord)
data Cas = Nom | Gen | Dat | Acc | Inst | Loc | Voc
  deriving (Show, Eq, Ord)
data Gnd = M1 | M2 | M3 | F | N -- # N1 N2 N3 P1 P2 P3
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
  , mk Interj "interj" ]
  where
    m <&> n = m <*> (colon *> n)
    m <?> n = m <*> optional (colon *> n)
    colon = P.char ':'


nmbP = mk Sg "sg" <|> mk Pl "pl"
casP = mk Nom "nom" <|> mk Gen "gen" <|> mk Dat "dat" <|> mk Acc "acc"
  <|> mk Inst "inst" <|> mk Loc "loc" <|> mk Voc "voc"
gndP = mk M1 "m1" <|> mk M2 "m2" <|> mk M3 "m3" <|> mk F "f" <|> mk N "n"
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
