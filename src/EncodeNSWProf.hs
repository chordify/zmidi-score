{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}

module Main where
import qualified Data.ByteString.Lazy as BL
import Data.Csv              hiding ()             
import qualified Data.Vector as V
import GHC.Generics
import ZMidi.Score.Datatypes hiding  ( numerator, denominator )
import ZMidi.Score.Quantise          ( QMidiScore (..), toQBins, QBins, ShortestNote (..))
import ZMidi.IO.Common               ( readQMidiScoreSafe, mapDir, warning)
import Ragtime.NSWProf
import Ragtime.MidiIMA               ( doIMA, toNSWProfWithTS, fourBarFilter)
import Ragtime.TimeSigSeg            ( TimedSeg (..))
import Ragtime.SelectQBins           ( selectQBins, filterByQBinStrength, printMeterStats, filterToList )
import IMA.InnerMetricalAnalysis     ( SWeight (..))
import System.Environment            ( getArgs )
import Data.List                     ( intercalate )
import Data.Maybe                    ( fromJust )
import Data.Ratio                    ( numerator, denominator)
import Data.Map.Strict               ( Map, elems )
import qualified Data.Map.Strict as M( lookup )

data NSWProfCSV = NSWProfCSV TimeSig [NSWeight]

instance ToField    NSWeight where toField (NSWeight w) = toField w
instance ToField    TimeSig  where 
  toField (TimeSig n d _ _) = toField (show n ++ "/" ++ show d)
-- instance ToField (Beat, BeatRat) where
  -- toField = toField . printKey
  
instance ToRecord NSWProfCSV where
  toRecord (NSWProfCSV ts p) = record (toField ts : map toField p)

-- instance ToRecord [(Beat, BeatRat)] where
  -- toRecord = record . map (toField . printKey)
  
printKey :: (Beat, BeatRat) -> String
printKey (Beat b, BeatRat br) = show b ++ "." 
                             ++ show (numerator br) ++ "." 
                             ++ show (denominator br)
  
  
toCSV :: Map TimeSig [(Beat, BeatRat)] -> QMidiScore 
      -> Either String [NSWProfCSV]
toCSV s qm = doIMA qm >>= fourBarFilter tb >>= return . map toProf where

  tb = ticksPerBeat . qMidiScore $ qm
  qb = toQBins FourtyEighth

  toProf :: TimedSeg TimeSig [Timed (Maybe ScoreEvent, SWeight)] -> NSWProfCSV
  toProf (TimedSeg (Timed _ ts) td) = 
    NSWProfCSV ts . filterToList s ts              -- only keep the strong pos
                  . normSWProf     -- normalise the profile 
                  . toNSWProfWithTS ts tb $ td     -- calculate the profile
    
processMidi :: Map TimeSig [(Beat, BeatRat)] -> FilePath -> FilePath -> IO ()
processMidi s out infp = do qm <- readQMidiScoreSafe FourtyEighth infp
                            case qm >>= timeSigCheck >>= toCSV s of
                              Left  err -> warning infp err
                              Right csv -> BL.appendFile out . encode $ csv 

-- writeHeader :: Map TimeSig [(Beat, BeatRat)] -> FilePath -> IO()
-- writeHeader m out = 
  -- BL.writeFile out . encode . fromJust . M.lookup (TimeSig 4 4 0 0) $ m
  
writeHeader :: Map TimeSig [(Beat, BeatRat)] -> FilePath -> IO()
writeHeader m out = 
  writeFile out . (++ "\n") . intercalate "," $ "meter" : 
                  (map printKey . fromJust . M.lookup (TimeSig 4 4 0 0) $ m)

  
-- testing
main :: IO ()
main = 
  do let out = "train.csv"
     arg <- getArgs 
     m   <- readNSWProf "ragtimeMeterProfilesTrain_2014-03-25.bin"  
            >>= return . selectQBins 12
     case arg of
       ["-f", fp] -> writeHeader m out >> processMidi m out fp
       ["-d", fp] -> writeHeader m out >> mapDir (processMidi m out) fp >> return ()
       ["-m", fp] -> readNSWProf fp >>= printMeterStats . filterByQBinStrength
       _ -> error "usage: -f <filename> -d <directory>"

  
       
-- copied from RagPatIMA Checks for a valid time signature
timeSigCheck :: QMidiScore -> Either String QMidiScore
timeSigCheck ms | hasTimeSigs (qMidiScore ms) = Right ms
                | otherwise = Left "Has no valid time signature" 

            
            