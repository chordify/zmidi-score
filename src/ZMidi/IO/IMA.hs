{-# OPTIONS_GHC -Wall                   #-}
module ZMidi.IO.IMA ( printIMA
                    , convertToIMA
                    ) where

import ZMidi.Score
import ZMidi.IO.Common             ( readMidiScoreSafe, ioWithWarning )
import ZMidi.IMA.Internal
import ZMidi.IMA.Analyse

import Ragtime.TimeSigSeg          ( TimedSeg (..) )

import IMA.InnerMetricalAnalysis hiding           ( Time(..) )

import Text.Printf                 ( printf )
import Data.Ratio                  ( numerator, denominator )
import Data.Binary                 ( Binary (..), encodeFile )

--------------------------------------------------------------------------------
-- Exporting SWMeterSegs (create separate IO
--------------------------------------------------------------------------------
         
convertToIMA :: FilePath -> FilePath -> IO ()
convertToIMA i o = undefined
                                  
convertQMid :: FilePath -> QMidiScore -> IO ()
convertQMid f qm = undefined
                                  
writeIMA :: FilePath -> [SWMeterSeg] -> IO ()
writeIMA f d = encodeFile f d >> putStrLn ("written: " ++ f)

--------------------------------------------------------------------------------
-- Printing the Inner Metrical Analysis
--------------------------------------------------------------------------------

printIMA :: QMidiScore -> IO ()
printIMA qm = mapM_ (starMeter tb) . either error id . doIMA $ qm 
  where tb = ticksPerBeat . qMidiScore $ qm
               
          
-- Prints an Inner metrical analysis. The printed weights are normalised by
-- the maximum weight found in the song.
starMeter :: TPB -> SWMeterSeg -> IO ()
starMeter tb (TimedSeg (Timed t ts) s) = 
  do putStrLn . printf ("%6d: ======================= " ++ show ts 
                         ++ " =======================" ) $ t
     mapM_ (toLine t ts) s where
                    
  -- prints one line e.g. "  2112:   2.2 -  1 /  2 1D : 397392 ***************"
  toLine :: Time -> TimeSig -> Timed (Maybe ScoreEvent, SWeight) -> IO ()
  toLine os x (Timed g (se,w)) = 
    let (br, bib, BeatRat r) = getBeatInBar x tb g
    in putStrLn (printf ("%6d: %3d.%1d - %2d / %2d" ++ showMSE se ++ ": %6d " ++ toStar w) 
                (g+os) br bib (numerator r) (denominator r) w)
                
  m = fromIntegral . maximum . map (snd . getEvent) $ s :: Double
  
  toStar :: SWeight -> String
  toStar x = stars (fromIntegral x / m)
                
  showMSE :: Maybe ScoreEvent -> String
  showMSE = maybe "    " (show . pitch) 