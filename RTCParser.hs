{-# OPTIONS_GHC -Wall                #-}
{-# LANGUAGE FlexibleContexts        #-}
module RTCParser where

import Text.ParserCombinators.UU.Core           ( (<$>), (<*>), (<*) )
import Text.ParserCombinators.UU.BasicInstances ( Parser, pSym )
import Text.ParserCombinators.UU.Derived        ( pListSep )
import Text.ParserCombinators.UU.Utils          ( pQuotedString, pInteger )


data RTC = RTC { id         :: Int
               , midiExist  :: Bool
               , title      :: String
               , subtitle   :: String
               , composer   :: String
               , lyricist   :: String
               , year       :: RTCYear
               , publisher  :: String
               , rtctype    :: RTCType
               , source     :: String 
               , status     :: String
               , folio      :: String
               , folioDet   :: String
               , folders    :: [RTCFolder]
               , length     :: Int
               } deriving (Show, Eq, Ord)
               
               
               
data RTCYear = Year   Int
             | Approx Int
             | Modern
             | OtherYear String deriving (Show, Eq, Ord)
               
data RTCType = TypeR | TypeY | TypeS | TypeI deriving (Show, Eq, Ord)

data RTCFolder = Cowles | Crausaz | Edwards | Intartaglia | MacDonald
               | PittPayne | Reublin | Trachtman | Watanabe | Wilson 
               | OtherFolder String deriving (Show, Eq, Ord)
 
                
                
pCompendium :: Parser [RTC] 
pCompendium = pListSep pComma pRTC

pRTC :: Parser RTC
pRTC = RTC <$> pInteger      <* pComma -- id         :: Int
           <*> pHasContent   <* pComma -- midiExist  :: Bool
           <*> pQuotedString <* pComma -- title      :: String
           <*> pQuotedString <* pComma -- subtitle   :: String
           <*> pQuotedString <* pComma -- composer   :: String
           <*> pQuotedString <* pComma -- lyricist   :: String
           <*> pRTCYear      <* pComma -- year       :: RTCYear
           <*> pQuotedString <* pComma -- publisher  :: String
           <*> pRTCType      <* pComma -- rtctype    :: RTCType
           <*> pQuotedString <* pComma -- source     :: String 
           <*> pQuotedString <* pComma -- status     :: String
           <*> pQuotedString <* pComma -- folio      :: String
           <*> pQuotedString <* pComma -- folioDet   :: String
           <*> pRTCFolder    <* pComma -- folders    :: [RTCFolder]
           <*> pInteger      <* pComma -- length     :: Int

pHasContent :: Parser Bool
pHasContent = undefined

pRTCYear :: Parser RTCYear
pRTCYear = undefined

pRTCType :: Parser RTCType
pRTCType = undefined

pRTCFolder :: Parser [RTCFolder]
pRTCFolder = undefined

pComma :: Parser Char
pComma = pSym ','