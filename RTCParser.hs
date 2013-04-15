{-# OPTIONS_GHC -Wall                #-}
{-# LANGUAGE FlexibleContexts        #-}
module RTCParser where

import Text.ParserCombinators.UU.BasicInstances


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
pCompendium = undefined

pRTC :: Parser RTC
pRTC = undefined