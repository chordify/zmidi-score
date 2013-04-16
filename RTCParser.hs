{-# OPTIONS_GHC -Wall                #-}
{-# LANGUAGE FlexibleContexts        #-}
module RTCParser where

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


parseComp :: FilePath -> IO ()
parseComp f = readFile f >>= print . parseRTC


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
               
data RTCID   = RTCID Int | New | NoID deriving (Show, Eq, Ord)
               
data RTCYear = Year   Int
             | Approx Int
             | Pre    Int
             | Range  Int Int
             | Decade Int
             | Modern
             | IOYear Int Int
             | YNone
             | OtherYear Text deriving (Show, Eq, Ord)

             
data RTCType = RTCType Char | TNone deriving (Show, Eq, Ord)

data RTCFolder = Cowles | Crausaz | Edwards | Intartaglia | MacDonald
               | PittPayne | Reublin | Trachtman | Watanabe | Wilson 
                 deriving (Show, Eq, Ord)
 
instance Show RTC where
  show (RTC i md tit subtit comp lyr yr pub tp src stat fol folDet fold aux l)
    = intercalate ", " [ show i, show md, show tit, show subtit, show comp
                       , show lyr, show yr, show pub, show tp, show src
                       , show stat, show fol, show folDet, show fold, show aux
                       , show l]
  
  showList rtc s = s ++ (intercalate "\n" . map show $ rtc)
                

parseRTC :: String -> [RTC]
parseRTC = map doLine . T.lines . pack

doLine :: Text -> RTC
doLine t = case split (=='\t') . T.filter (/= '\"') $ t of
  (i : md : tit : subtit : comp : lyr : yr : pub 
     : tp : src : stat : fol    : folDet :rest ) 
      -> let (fldrs, (aux : [ln])) = splitAt 10 rest
         in RTC (parseField pRTCID NoID i)          -- id         :: Int
             (T.null md)                            -- midiExist  :: Bool
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

