{-# OPTIONS_GHC -Wall                #-}
{-# LANGUAGE FlexibleContexts        #-}
module RTCParser (parseRTCFile, parseRTC, RTC (..), RTCFolder (..)) where

import Text.ParserCombinators.UU.Core           ( (<$>), (<$), (<*>), (*>), (<*)
                                                , (<|>), P )
import Text.ParserCombinators.UU.BasicInstances ( Parser, pSym, Str, LineColPos )
import Text.ParserCombinators.UU.Utils          ( pInteger, lexeme, pUpper )
import HarmTrace.Base.Parsing                   ( pString, parseDataSafe, parseDataWithErrors )
import Data.Maybe                               ( catMaybes ) 
import Data.List                                ( intercalate )
import Data.Text                                ( pack, split, Text )
import qualified Data.Text as T                 ( lines, null, filter )
import Data.ListLike.Text.Text                  ( )

parseRTCFile :: FilePath -> IO ()
parseRTCFile f = readFile f >>= print . filter midiExist . parseRTC

--------------------------------------------------------------------------------
-- Some datatypes for representing the RagTimeCompendium
--------------------------------------------------------------------------------

-- | Represents a RagTime Compendium entry
data RTC = RTC { id         :: RTCID
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

-- | Represents a ragtime compendium identifyer
data RTCID   = RTCID Int | New | NoID deriving (Show, Eq, Ord)

-- | Stores a copyright date
-- Ideally, the year of copyright as stated on the sheet music copyright page 
-- - but some of this music was not copyrighted so the only practicable date 
-- for such items is the year of publication or even composition. In this 
-- listing minor errors in date (one year here or there) has not been considered
-- serious - it has been seen as being more important to list the item. 

-- ~ means about (e.g. about 1910); c/c means composed/copyright date 
-- (e.g. 1910c/c1924).  “Pre xxxx” generally means there is a piano roll or 
-- recording in this year (and so far as I am aware many of these titles were 
-- never issued as printed scores); in rare cases it may be the year of death 
-- of the composer.

-- In some cases publishers are known to have issued an orchestration 
-- (by an arranger) first in order to create awareness and demand, and then 
-- later they issued the instrumental (or song).  This would mean that the
 -- base composition (the instrumental or song) was from an earlier date than 
 -- its first publication (which is the date usually cited).  Where there is 
 -- evidence of an earlier published orchestration it has been noted, e.g. 
 -- i 1914 (o 1912). 
data RTCYear = Year   Int
             | Approx Int
             | Pre    Int
             | Range  Int Int
             | Decade Int
             | Modern
             | IOYear Int Int
             | YNone
             | OtherYear Text deriving (Show, Eq, Ord)

-- | Type - unfortunately somewhat a grey area, but there are hundreds of 
-- corrections in this version.	
-- B = Blues
-- F = Fox Trot
-- I = Intermezzo or Two Step
-- M = March
-- N = Novelty
-- O = Opera or  classical
-- P = Polkas or Parlor Piano pieces
-- R = Ragtime composed before about 1945
-- S = Ragtime Style Song
-- W = Waltz
-- Y = Contemporary Ragtime
data RTCType = RTCType Char | TNone deriving (Show, Eq, Ord)

data RTCFolder = -- Most important folders?
                 Cowles | Crausaz | Edwards | Intartaglia | MacDonald
               | PittPayne | Reublin | Trachtman | Watanabe | Wilson 
                 -- Other folders
               | Treemonisha | TrachtmanRolls | Summers | Smythe | Schwartz
               | Roache | Ranalli | Pianocorder | PianocorderCheck
               | Perry | OldWeb | ODell | MiscSZ | MiscMR | Mathew
               | MiscFL | MiscAE | MiscUnks | AddedUniques | AddedHimpsl 
               | AddedOther | Mezjuev | Keller | Hiawatha | ElectriClef
               | Decker | CookieJar | BokerTov  deriving (Show, Eq, Ord)
 
instance Show RTC where
  show (RTC i md tit subtit comp lyr yr pub tp src stat fol folDet fold aux l)
    = intercalate ", " . filter (/= "\"\"") $
                       [ show i, show md, show tit, show subtit, show comp
                       , show lyr, show yr, show pub, show tp, show src
                       , show stat, show fol, show folDet, show fold, show aux
                       , show l]
  
  showList rtc s = s ++ (intercalate "\n" . map show $ rtc)


--------------------------------------------------------------------------------
-- Parsing the Compendium
--------------------------------------------------------------------------------

parseRTC :: String -> [RTC]
parseRTC = map doLine . T.lines . pack

-- parses one ragtime entry
doLine :: Text -> RTC
doLine t = case split (=='\t') . T.filter (/= '\"') $ t of
  (i : md : tit : subtit : comp : lyr : yr : pub 
     : tp : src : stat : fol    : folDet :rest ) 
      -> let (fldrs, (aux : [ln])) = splitAt 10 rest
         in RTC (parseField pRTCID NoID i)          -- id         :: Int
             (not . T.null $ md)                    -- midiExist  :: Bool
             tit                                    -- title      :: Text
             subtit                                 -- subtitle   :: Text
             comp                                   -- composer   :: Text
             lyr                                    -- lyricist   :: Text
             (parseFieldFail pRTCYear OtherYear yr) -- year       :: RTCYear
             pub                                    -- publisher  :: Text
             (parseField pRTCType TNone tp)         -- rtctype    :: RTCType
             src                                    -- source     :: Text 
             stat                                   -- status     :: Text
             fol                                    -- folio      :: Text
             folDet                                 -- folioDet   :: Text
             (parseFolders fldrs)                   -- folders    :: [RTCFolder]
             aux                                    -- auxFolders :: Text
             (parseField pInteger (-1) ln)          -- len        :: Int
  _    -> error ("unexpected number of fields: " ++ show t)

-- folders are marked with [x]'s, we match them agains all folder headers
parseFolders :: [Text] -> [RTCFolder] 
parseFolders t = catMaybes $ zipWith f t [ Cowles, Crausaz, Edwards, Intartaglia
                                         , MacDonald, PittPayne, Reublin
                                         , Trachtman, Watanabe, Wilson ]
                                         
  where f :: Text -> RTCFolder -> Maybe RTCFolder
        f c r | (not . T.null $ c) = Just r
              | otherwise          = Nothing
              
-- parses a field using the p and applies f if p fails
parseFieldFail :: P (Str Char Text LineColPos) b -> (Text -> b) -> Text -> b
parseFieldFail p f t = case parseDataWithErrors p t of
                        (r, []) -> r
                        (_, _ ) -> f t
  
-- Given a parser, an empty string option, and a text, returns the parsed result
-- or the empty string option
parseField :: P (Str Char Text LineColPos) b -> b -> Text -> b
parseField p empt t 
  | T.null t  = empt
  | otherwise = parseDataSafe p t

-- | Parses a year
pRTCYear :: Parser RTCYear
pRTCYear =   Year        <$>  pInteger
         <|> Decade      <$> pInteger <* pString "'s"
         <|> Approx      <$> (pString "ca "  *> pInteger)
         <|> Approx      <$> (pSym  '~'      *> pInteger)
         <|> Pre         <$> (pString "pre " *> pInteger)
         <|> Range       <$>  pInteger <*> (pString "-" *> pInteger)
         <|> IOYear      <$> (pString "i " *> pInteger) 
                         <*> (pString ", o " *> pInteger)
         <|> flip IOYear <$> (pString "o " *> pInteger) 
                         <*> (pString ", i " *> pInteger)
         <|> Modern      <$  pString "[modern]"

pRTCID :: Parser RTCID
pRTCID =   RTCID <$> pInteger
       <|> New   <$  pString "new"
     
pRTCType :: Parser RTCType 
pRTCType = lexeme (RTCType <$> pUpper)

