{-# OPTIONS_GHC -Wall          #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor     #-}
-- todo move to ZMidi.IO
module ZMidi.IMA.MeterGT ( MeterGT (..)
                         , readGT
                         , maybeReadGT
                         , setGT
                         ) where

import Prelude              hiding ( readFile )

import ZMidi.Score                 ( TimeSig (..) )
import ZMidi.IO.Common             ( warning, putErrStrLn )
import ZMidi.IMA.SelectProfBins    ( Rot (..) )
import ZMidi.IMA.NSWProf           ( NSWPStore (..) )

import Data.Vector                 ( Vector, toList )
import Data.Csv                    ( FromField (..), FromNamedRecord (..)
                                   , decodeByName, (.:), Header)
import Data.List                   ( intercalate, find )
import Data.ByteString.Lazy        ( readFile )
import Control.Applicative         ( (<$>), (<*>), pure )
import Control.Monad               ( mzero )
import Control.Arrow               ( second, first )

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
                       
setGT :: [MeterGT [TimeSig]] -> NSWPStore -> NSWPStore
setGT g n = let fp = nswpsFile n
            in case find (\x -> gtFile x == fp) g of
                 Just x -> update x n
                 _ -> error ("setGT: NSWPStore for " ++ fp)

{-
mergeWithNSWPStore :: [MeterGT [TimeSig]] -> [NSWPStore] -> [NSWPStore]
mergeWithNSWPStore gt = zipWith setGT gt where

  setGT :: MeterGT [TimeSig] -> NSWPStore -> NSWPStore
  setGT g n | gtFile g == nswpsFile n = update g n
            | otherwise = error "mergeWithNSWPStore: filenames do not match" 
-}

update :: MeterGT [TimeSig] -> NSWPStore -> NSWPStore
update g n = n { nswps = zipWith (\t x -> first (const t) x) (gtMeter g) (nswps n) }
                       
readGT :: FilePath -> IO [MeterGT [TimeSig]]
readGT f = do i <- readFile f 
              case decodeByName i of
                Left w  -> warning f w >> return []
                Right x -> return . mergeMetersOfSong . toList . snd $ x
                                   
maybeReadGT :: Maybe FilePath -> IO (Maybe [MeterGT [TimeSig]])
maybeReadGT mfp = case mfp of 
  Just fp -> readGT fp >>= return . Just
  _  -> putErrStrLn "warning: No external ground-truth provided" >> return Nothing