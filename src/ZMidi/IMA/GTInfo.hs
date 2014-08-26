{-# OPTIONS_GHC -Wall          #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
-- todo move to ZMidi.IO
module ZMidi.IMA.GTInfo ( GTInfo (..)
                        , GTMR (..)
                        , readGT
                        , maybeReadGT
                        ) where

import Prelude              hiding ( readFile )

import ZMidi.Score                 ( TimeSig (..) )
import ZMidi.IO.Common             ( warning, putErrStrLn )
import ZMidi.IMA.Rotations         ( Rot (..) )

import Data.Vector                 ( toList )
import Data.Csv                    ( FromField (..), FromNamedRecord (..)
                                   , decodeByName, (.:))
import Data.List                   ( intercalate )
import Data.ByteString.Lazy        ( readFile )
import Data.Binary                 ( Binary )
import Control.Applicative         ( (<$>), (<*>), pure )
import Control.Monad               ( mzero )
import Control.DeepSeq             ( NFData (..) )
import GHC.Generics                ( Generic )


data GTInfo = GTInfo { gtFile   :: FilePath
                     , gtMeters :: [GTMR]
                     , gtNotes  :: String
                     } 

data GTMR = GTMR { gtTimeSig :: TimeSig
                 , gtRot     :: Rot
                 } deriving Generic
             
instance Binary GTMR
instance NFData GTMR
             
instance Show GTMR where
  show (GTMR m r) = '(' : show m ++ ',' : show r ++ [')']
             
instance Show GTInfo where
  show (GTInfo f ms s) = intercalate ['m',' '] [f, show ms, s]
  showList l s = s ++ (intercalate ['\n'] . map show $ l)

instance FromNamedRecord GTInfo where
  parseNamedRecord m = toGTInfo <$> m .: "File" 
                                <*> m .: "Corrected"
                                <*> m .: "Rotation"
                                <*> m .: "Notes" where
                                
     toGTInfo :: FilePath -> TimeSig -> Rot -> String -> GTInfo
     toGTInfo fp ts r n = GTInfo fp [GTMR ts r] n
                       
instance FromField TimeSig where
  parseField s = case s of 
                   "'4/4'" -> pure $ TimeSig 4 4 0 0
                   "'2/4'" -> pure $ TimeSig 2 4 0 0
                   "'2/2'" -> pure $ TimeSig 2 2 0 0
                   "'3/4'" -> pure $ TimeSig 3 4 0 0
                   "'6/8'" -> pure $ TimeSig 6 8 0 0
                   _      -> mzero
                       
-- mergeMetersOfSong :: [GTInfo TimeSig] -> [GTInfo [TimeSig]]
mergeMetersOfSong :: [GTInfo] -> [GTInfo]
mergeMetersOfSong = foldr merge [] where

  merge :: GTInfo -> [GTInfo] -> [GTInfo]
  merge m [] = [m]
  merge a@(GTInfo fa ma na) (b@(GTInfo fb mb nb) : t) 
    | fa == fb  = GTInfo fa (ma ++ mb) (na ++ nb) : t
    | otherwise = a : b : t


readGT :: FilePath -> IO [GTInfo]
readGT f = do i <- readFile f 
              case decodeByName i of
                Left w  -> warning f w >> return []
                Right x -> return . mergeMetersOfSong . toList . snd $ x
                                   
maybeReadGT :: Maybe FilePath -> IO (Maybe [GTInfo])
maybeReadGT mfp = case mfp of 
  Just fp -> do putStrLn ("read Ground-Truth: " ++ fp)
                readGT fp >>= return . Just
  _  -> putErrStrLn "warning: No external ground-truth provided" >> return Nothing
