{-# OPTIONS_GHC -Wall                #-}
{-# LANGUAGE FlexibleContexts        #-}
module RTCParser where

import Text.ParserCombinators.UU.Core           ( (<$>), (<$), (<*>), (*>)
                                                , (<|>), P )
import Text.ParserCombinators.UU.BasicInstances ( Parser, pSym, pRange, Str, LineColPos )
import Text.ParserCombinators.UU.Derived        ( pMany )
import Text.ParserCombinators.UU.Utils          ( pInteger )
import HarmTrace.Base.Parsing                   ( pString, parseDataSafe, parseDataWithErrors )
-- import Data.Maybe                               ( catMaybes ) 
import Data.List                                ( intercalate )
import Data.Text                                ( pack, split, Text, empty)
import qualified Data.Text as T                 ( lines, null, filter )
import Data.ListLike.Text.Text                  ( )
-- import Data.ListLike.Base (ListLike)

-- import Debug.Trace 

parseComp :: FilePath -> IO ()
-- test f = readFile f >>= print . parseDataWithErrors pCompendium 
parseComp f = readFile f >>= print . parseRTC

-- test :: IO ()
-- test = print $ parseDataSafe pRTC 
  -- "2\t \t 3 \t \t\"Williams, Spencer\""-- "		1928	(Unpublished)	R	Brier															0"

data RTC = RTC { id         :: Int
               , midiExist  :: Bool
               , title      :: Text
               , subtitle   :: Text
               , composer   :: Text
               , lyricist   :: Text
               , year       :: RTCYear
               , publisher  :: Text
               , rtctype    :: RTCType
               , source     :: Text 
               , status     :: Text
               , folio      :: Text
               , folioDet   :: Text
               , folders    :: [RTCFolder]
               , auxFolders :: Text
               , len        :: Int
               } deriving (Eq, Ord)
               
               
               
data RTCYear = Year   Int
             | Approx Int
             | Pre    Int
             | Range  Int Int
             | Modern
             | YNone
             | OtherYear String deriving (Show, Eq, Ord)
               
data RTCType = TypeR | TypeY | TypeS | TypeI | TNone
             | TypeUnknown String deriving (Show, Eq, Ord)

data RTCFolder = Cowles | Crausaz | Edwards | Intartaglia | MacDonald
               | PittPayne | Reublin | Trachtman | Watanabe | Wilson 
               | OtherFolder String deriving (Show, Eq, Ord)
 
instance Show RTC where
  show (RTC i md tit subtit comp lyr yr pub tp src stat fol folDet fold aux l)
    = intercalate ", " [ show i, show md, show tit, show subtit, show comp
                       , show lyr, show yr, show pub, show tp, show src
                       , show stat, show fol, show folDet, show fold, show aux
                       , show l]
  
  showList rtc s = s ++ (intercalate "\n" . map show $ rtc)
                
-- pCompendium :: Parser [RTC] 
-- pCompendium = pListSep pComma pRTC

-- data Test = Test Int Bool String String String deriving Show

parseRTC :: String -> [RTC]
parseRTC = map doLine . T.lines . pack

doLine :: Text -> RTC
doLine t = case split (=='\t') . T.filter (/= '\"') $ t of
  (i : md : tit : subtit : comp : lyr : yr : pub 
     : tp : src : stat : fol    : folDet :rest ) 
      -> RTC (parseField pInteger (-1) i)    -- id         :: Int
             (T.null md)                     -- midiExist  :: Bool
             tit                             -- title      :: Text
             subtit                          -- subtitle   :: Text
             comp                            -- composer   :: Text
             lyr                             -- lyricist   :: Text
             (parseField pRTCYear YNone yr ) -- year       :: RTCYear
             pub                             -- publisher  :: Text
             (parseField pRTCType TNone tp)  -- rtctype    :: RTCType
             src                             -- source     :: Text 
             stat                            -- status     :: Text
             fol                             -- folio      :: Text
             folDet                          -- folioDet   :: Text
             []                              -- folders    :: [RTCFolder]
             empty                           -- auxFolders :: Text
             0                               -- len        :: Int
  _    -> error ("unexpected number of fields: " ++ show t)

-- Given a parser, an empty string option, and a text, returns the parsed result
-- or the empty string option
parseField :: P (Str Char Text LineColPos) b -> b -> Text -> b
parseField p empt t 
  | T.null t  = empt
  | otherwise = parseDataSafe p t

        
pHasContent :: Parser Bool
pHasContent = null <$> pRTCString -- perhaps check for x

pRTCYear :: Parser RTCYear
pRTCYear =   Year      <$>  pInteger
         <|> Pre       <$> (pString "ca "  *> pInteger)
         <|> Pre       <$> (pSym  '~'      *> pInteger)
         <|> Approx    <$> (pString "pre " *> pInteger)
         <|> Range     <$>  pInteger <*> (pString " - " *> pInteger)
         <|> Modern    <$  pString "[modern]"
         -- <|> OtherYear <$> pRTCString

pRTCType :: Parser RTCType
pRTCType =   TypeR <$ pSym 'R'
         <|> TypeY <$ pSym 'Y'
         <|> TypeS <$ pSym 'S'
         <|> TypeI <$ pSym 'I'
         -- <|> TypeUnknown <$> pRTCString
         
-- pRTCFolder :: Parser [RTCFolder]
-- pRTCFolder =   catMaybes . zipWith selectFold flds <$> pExact 10 pMaybeFolder where

  -- selectFold :: RTCFolder -> Maybe Char -> Maybe RTCFolder
  -- selectFold f mc = maybe Nothing (const $ Just f) mc

  -- flds = [ Cowles, Crausaz, Edwards, Intartaglia, MacDonald
         -- , PittPayne, Reublin, Trachtman, Watanabe, Wilson  ]

  -- pMaybeFolder :: Parser (Maybe Char)
  -- pMaybeFolder = pMaybe (pSym 'x')               


pRTCString :: Parser String
pRTCString =   pMany pValidChar
           -- <|> ((:) <$> (pSym ' ') <*> (pure []))
           
-- Valid characters are:
-- !\"#$%&'()*+
-- -./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~
pValidChar :: Parser Char
pValidChar = pRange (' ', '~') 
           
pComma :: Parser Char
pComma = pSym '\t'
