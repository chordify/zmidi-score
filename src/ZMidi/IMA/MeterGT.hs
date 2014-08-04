{-# OPTIONS_GHC -Wall          #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor     #-}
-- todo move to ZMidi.IO
module ZMidi.IMA.MeterGT where

import Prelude              hiding ( readFile )

import ZMidi.Score                 ( TimeSig (..) )
import ZMidi.IMA.SelectProfBins    ( Rot (..) )

import Data.Vector                 ( Vector, toList )
import Data.Csv                    ( FromField (..), FromNamedRecord (..)
                                   , decodeByName, (.:), Header)
import Data.List                   ( intercalate )
import Data.ByteString.Lazy        ( readFile )
import Control.Applicative         ( (<$>), (<*>), pure )
import Control.Monad               ( mzero )
import Control.Arrow               ( second )

data MeterGT a = MeterGT { gtFile  :: FilePath
                         , gtMeter :: a
                         , gtRot   :: Rot
                         , gtNotes :: String
                         } deriving Functor
                       
instance Show a => Show (MeterGT a) where
  show (MeterGT f ts r s) = intercalate [',', ' '] [f, show ts, show . rot $ r, s]
  showList l s = s ++ (intercalate ['\n'] . map show $ l)

instance FromField a => FromNamedRecord (MeterGT a) where
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
                       
mergeMetersOfSong :: [MeterGT TimeSig] -> [MeterGT [TimeSig]]
mergeMetersOfSong = foldr step [] where

  step :: MeterGT TimeSig -> [MeterGT [TimeSig]] -> [MeterGT [TimeSig]]
  step m []    = [fmap return m] 
  step m (h:t) | gtFile m == gtFile h = fmap (gtMeter m :) h : t
               | otherwise            = fmap return m   :  h : t
                       
readGT :: FilePath -> IO (Either String (Header, [MeterGT TimeSig]))
readGT f = readFile f >>= return . fmap (second toList) . decodeByName 

