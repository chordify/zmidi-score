{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import GHC.Generics
import ZMidi.Score.Datatypes
import ZMidi.Score.Quantise (QMidiScore (..), toQBins, QBins, ShortestNote (..))
import ZMidi.IO.Common       ( readQMidiScoreSafe, mapDir, warning)
import Ragtime.NSWProf
import Ragtime.MidiIMA (doIMA, toNSWProfWithTS, fourBarFilter)
import Ragtime.TimeSigSeg (TimedSeg (..))
import IMA.InnerMetricalAnalysis (SWeight (..))
import System.Environment    ( getArgs )

data NSWProfCSV = NSWProfCSV TimeSig (V.Vector NSWeight)

instance ToField    NSWeight where toField (NSWeight w) = toField w
instance ToField    TimeSig  where 
  toField (TimeSig n d _ _) = toField (show n ++ "/" ++ show d)

instance ToRecord   NSWProfCSV where
  toRecord (NSWProfCSV ts p) = record (toField ts : map toField (V.toList p))

toCSV :: QMidiScore -> Either String [NSWProfCSV]
toCSV qm = doIMA qm >>= fourBarFilter tb >>= return . map toProf where

  tb = ticksPerBeat . qMidiScore $ qm
  qb = toQBins FourtyEighth

  toProf :: TimedSeg TimeSig [Timed (Maybe ScoreEvent, SWeight)] -> NSWProfCSV
  toProf (TimedSeg (Timed _ ts) td) = 
    NSWProfCSV ts . vectorize qb ts . normSWProf . toNSWProfWithTS ts tb $ td
    
processMidi :: FilePath -> IO ()
processMidi fp = do qm <- readQMidiScoreSafe FourtyEighth fp 
                    case qm >>= toCSV of
                      Left  err -> warning fp err
                      Right csv -> BL.appendFile "prof.csv" . encode $ csv
    
-- testing
main :: IO ()
main = 
  do arg <- getArgs 
     case arg of
       ["-f", fp] -> processMidi fp
       ["-d", fp] -> mapDir processMidi fp >> return ()
       _ -> error "usage: -f <filename> -d <directory>"
