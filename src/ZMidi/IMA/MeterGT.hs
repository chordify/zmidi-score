{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
 {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric              #-}
module ZMidi.IMA.MeterGT where

import Prelude              hiding ( readFile )

import ZMidi.Score                 ( TimeSig (..) )
import ZMidi.IMA.SelectProfBins    ( Rot (..) )

import Data.Vector                 ( Vector )
import Data.Csv                    ( FromField (..), FromNamedRecord (..)
                                   , decodeByName, (.:), Header)
import Data.List                   ( intercalate )
import Data.ByteString.Lazy        ( readFile )
import Control.Applicative         ( (<$>), (<*>), pure )
import Control.Monad               ( mzero )

data MeterGT = MeterGT { gtFile  :: FilePath
                       , gtMeter :: TimeSig
                       , gtRot   :: Rot
                       , gtNotes :: String
                       } 
                       
instance Show MeterGT where
  show (MeterGT f ts r s) = intercalate [',', ' '] [f, show ts, show . rot $ r, s]
  showList l s = s ++ (intercalate ['\n'] . map show $ l)

instance FromNamedRecord MeterGT where
  parseNamedRecord m = MeterGT <$> m .: "File" 
                               <*> m .: "Corrected"
                               <*> m .: "Rotation"
                               <*> m .: "Notes"
                       
instance FromField TimeSig where
  parseField s = case s of 
                   "'4/4'" -> pure $ TimeSig 4 4 0 0
                   "'2/4'" -> pure $ TimeSig 2 4 0 0
                   "'2/2'" -> pure $ TimeSig 2 2 0 0
                   "'3/4'" -> pure $ TimeSig 3 4 0 0
                   "'6/8'" -> pure $ TimeSig 6 8 0 0
                   _      -> mzero
                       
                                              
readGT :: FilePath -> IO (Either String (Header, Vector MeterGT))
readGT f = readFile f >>= return . decodeByName 

