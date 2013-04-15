{-# OPTIONS_GHC -Wall                #-}
{-# LANGUAGE FlexibleContexts        #-}
module RTCParser where

import Text.ParserCombinators.UU.Core           ( (<$>), (<$), (<*>), (<*), (*>), (<|>))
import Text.ParserCombinators.UU.BasicInstances ( Parser, pSym )
import Text.ParserCombinators.UU.Derived        ( pListSep, pMany )
import Text.ParserCombinators.UU.Utils          ( pInteger, pAscii )
import HarmTrace.Base.Parsing                   ( pString )


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
             | Pre    Int
             | Range  Int Int
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
           <*> pRTCString    <* pComma -- title      :: String
           <*> pRTCString    <* pComma -- subtitle   :: String
           <*> pRTCString    <* pComma -- composer   :: String
           <*> pRTCString    <* pComma -- lyricist   :: String
           <*> pRTCYear      <* pComma -- year       :: RTCYear
           <*> pRTCString    <* pComma -- publisher  :: String
           <*> pRTCType      <* pComma -- rtctype    :: RTCType
           <*> pRTCString    <* pComma -- source     :: String 
           <*> pRTCString    <* pComma -- status     :: String
           <*> pRTCString    <* pComma -- folio      :: String
           <*> pRTCString    <* pComma -- folioDet   :: String
           <*> pRTCFolder    <* pComma -- folders    :: [RTCFolder]
           <*> pInteger      <* pComma -- length     :: Int

pHasContent :: Parser Bool
pHasContent = null <$> pRTCString -- perhaps check for x

pRTCYear :: Parser RTCYear
pRTCYear =   Year      <$>  pInteger
         <|> Pre       <$> (pString "ca "  *> pInteger)
         <|> Pre       <$> (pSym  '~'      *> pInteger)
         <|> Approx    <$> (pString "pre " *> pInteger)
         <|> Range     <$>  pInteger <*> (pString " - " *> pInteger)
         <|> Modern    <$  pString "[modern]"
         <|> OtherYear <$> pRTCString

pRTCType :: Parser RTCType
pRTCType = undefined

pRTCFolder :: Parser [RTCFolder]
pRTCFolder = undefined

pRTCString :: Parser String
pRTCString = pMany pAscii

pComma :: Parser Char
pComma = pSym ','