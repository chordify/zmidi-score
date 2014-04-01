{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}
module Main where
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import GHC.Generics
import ZMidi.Score.Datatypes hiding (numerator, denominator)
import ZMidi.Score.Quantise (QMidiScore (..), toQBins, QBins, ShortestNote (..))
import ZMidi.IO.Common       ( readQMidiScoreSafe, mapDir, warning)
import Ragtime.NSWProf
import Ragtime.MidiIMA (doIMA, toNSWProfWithTS, fourBarFilter)
import Ragtime.TimeSigSeg (TimedSeg (..))
import Ragtime.SelectQBins (selectQBins)
import IMA.InnerMetricalAnalysis (SWeight (..))
import System.Environment    ( getArgs )
import Data.List             (intercalate)
import Data.Ratio            (numerator, denominator)

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
                    case qm >>= timeSigCheck >>= toCSV of
                      Left  err -> warning fp err
                      Right csv -> BL.appendFile "train.csv" . encode $ csv 

genHeader :: Int -> String
genHeader n = intercalate "," $ ("meter" : map toName [0..n]) where

  ts  = TimeSig 12 4 0 0 -- an ugly hack, but getBeatInBar only looks at the numerator
  tpb = fromIntegral (toQBins FourtyEighth) :: TPB
                        
  toName :: Int -> String
  toName i = case getBeatInBar ts tpb (Time i) of
              (1, b, BeatRat br) ->    show (beat b) ++ "." 
                                    ++ show (numerator br) ++ "." 
                                    ++ show (denominator br)
              _  -> error ("index out of bounds: " ++ show i)                      
    
-- testing
main :: IO ()
main = 
  do arg <- getArgs 
     case arg of
       ["-f", fp] -> processMidi fp
       ["-d", fp] -> mapDir processMidi fp >> return ()
       ["-m", fp] -> readNSWProf fp >>= print . selectQBins 12
       _ -> error "usage: -f <filename> -d <directory>"

-- copied from RagPatIMA Checks for a valid time signature
timeSigCheck :: QMidiScore -> Either String QMidiScore
timeSigCheck ms | hasTimeSigs (qMidiScore ms) = Right ms
                | otherwise = Left "Has no valid time signature" 
         
         


            
            