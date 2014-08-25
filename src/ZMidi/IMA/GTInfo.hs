{-# OPTIONS_GHC -Wall          #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor     #-}
-- todo move to ZMidi.IO
module ZMidi.IMA.GTInfo ( GTInfo (..)
                         , readGT
                         , maybeReadGT
                         , setGT
                         ) where

import Prelude              hiding ( readFile )

import ZMidi.Score                 ( TimeSig (..) )
import ZMidi.IO.Common             ( warning, putErrStrLn )
import ZMidi.IMA.SelectProfBins    ( Rot (..) )
import ZMidi.IMA.NSWProf           ( NSWPStore (..) )

import Data.Vector                 ( toList )
import Data.Csv                    ( FromField (..), FromNamedRecord (..)
                                   , decodeByName, (.:))
import Data.List                   ( intercalate, find )
import Data.ByteString.Lazy        ( readFile )
import Control.Applicative         ( (<$>), (<*>), pure )
import Control.Monad               ( mzero )
import Control.Arrow               ( first )

--TODO we should pair the TimeSig and the Rot, the rotation can be different per segment!

data GTInfo a = GTInfo { gtFile  :: FilePath
                         , gtMeter :: a
                         , gtRot   :: Rot
                         , gtNotes :: String
                         } deriving Functor


instance Show a => Show (GTInfo a) where
  show (GTInfo f ts r s) = intercalate [',', ' '] [f, show ts, show . rot $ r, s]
  showList l s = s ++ (intercalate ['\n'] . map show $ l)

instance FromField a => FromNamedRecord (GTInfo a) where
  parseNamedRecord m = GTInfo <$> m .: "File" 
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
                       
mergeMetersOfSong :: [GTInfo TimeSig] -> [GTInfo [TimeSig]]
mergeMetersOfSong = foldr step [] where

  step :: GTInfo TimeSig -> [GTInfo [TimeSig]] -> [GTInfo [TimeSig]]
  step m []    = [fmap return m] 
  step m (h:t) | gtFile m == gtFile h = fmap (gtMeter m :) h : t
               | otherwise            = fmap return m   :  h : t
                   
-- Takes a Meter Ground-Truth and updates the meter annotation in a NSWPStore                   
setGT :: [GTInfo [TimeSig]] -> NSWPStore -> NSWPStore
setGT g n = let fp = nswpsFile n
            in case find (\x -> gtFile x == fp) g of
                 Just x -> update x n
                 _ -> error ("setGT: NSWPStore for " ++ fp)

{-
mergeWithNSWPStore :: [GTInfo [TimeSig]] -> [NSWPStore] -> [NSWPStore]
mergeWithNSWPStore gt = zipWith setGT gt where

  setGT :: GTInfo [TimeSig] -> NSWPStore -> NSWPStore
  setGT g n | gtFile g == nswpsFile n = update g n
            | otherwise = error "mergeWithNSWPStore: filenames do not match" 
-}

update :: GTInfo [TimeSig] -> NSWPStore -> NSWPStore
update g n = n { nswps = zipWith (\t x -> first (const t) x) (gtMeter g) (nswps n) }
                       
readGT :: FilePath -> IO [GTInfo [TimeSig]]
readGT f = do i <- readFile f 
              case decodeByName i of
                Left w  -> warning f w >> return []
                Right x -> return . mergeMetersOfSong . toList . snd $ x
                                   
maybeReadGT :: Maybe FilePath -> IO (Maybe [GTInfo [TimeSig]])
maybeReadGT mfp = case mfp of 
  Just fp -> do putStrLn ("read Ground-Truth: " ++ fp)
                readGT fp >>= return . Just
  _  -> putErrStrLn "warning: No external ground-truth provided" >> return Nothing
