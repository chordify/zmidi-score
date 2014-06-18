{-# OPTIONS_GHC -Wall                   #-}
module ZMidi.IO.IMA ( printIMA
                    , convertToIMA
                    , readIMAScoreGeneric
                    ) where

import ZMidi.Score
import ZMidi.IO.Common             ( readQMidiScoreSafe, warning )
import ZMidi.IMA.Internal
import ZMidi.IMA.Analyse

import Ragtime.TimeSigSeg          ( TimedSeg (..) )

import IMA.InnerMetricalAnalysis hiding           ( Time(..) )

import Text.Printf                 ( printf )
import System.FilePath             ( takeExtension )
-- import Control.Applicative         ( Applicative (..),(<$>) )
import Data.Char                   ( toLower )
import Data.Ratio                  ( numerator, denominator )
import Data.Binary                 ( encodeFile, decodeFile )

--------------------------------------------------------------------------------
-- Exporting SWMeterSegs (create separate IO
--------------------------------------------------------------------------------
         
convertToIMA :: FilePath -> FilePath -> IO ()
convertToIMA o i =   readQMidiScoreSafe FourtyEighth i >>= writeIMA
                 
  where writeIMA :: Either String QMidiScore -> IO ()
        writeIMA qm = do either (warning i) (encodeFile o) $ toIMAStore i qm
                         putStrLn ("written: " ++ o)
        
readIMAScoreGeneric :: FilePath -> IO (Either String IMAStore)
readIMAScoreGeneric f = 
  case take 4 . map toLower . takeExtension $ f of
    ".mid" -> readQMidiScoreSafe FourtyEighth f >>= return . toIMAStore f
    ".ima" -> decodeFile f >>= return . Right
    e      -> error ("Error: " ++ e ++ " is not an accepted file type")  


--------------------------------------------------------------------------------
-- Printing the Inner Metrical Analysis
--------------------------------------------------------------------------------

printIMA :: IMAStore -> IO ()
printIMA is = mapM_ (starMeter (imaTPB is)) . swMeterSeg $ is
          
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