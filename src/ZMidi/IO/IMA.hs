{-# OPTIONS_GHC -Wall                   #-}
module ZMidi.IO.IMA ( exportIMAStore
                    , readIMAScoreGeneric
                    , exportCSVProfs
                    , writeCSVHeader
                    , matchIO
                    -- | * Printing
                    , printMatchLine
                    , printMatchAgr
                    , printIMA
                    , analyseProfile
                    ) where

import ZMidi.Score
import ZMidi.IO.Common             -- ( readQMidiScoreSafe, warning )
import ZMidi.IMA.Internal
import ZMidi.IMA.Analyse
import ZMidi.IMA.NSWProf           ( normSWProfByBar )
import ZMidi.IMA.SelectProfBins    ( Rot (..), filterBin )
import ZMidi.IMA.RNSWMatch         ( PMatch, pickMeters, match
                                   , avgResult, evalMeter, printPickMeter )
import ZMidi.IMA.TimeSigSeg        ( TimedSeg (..) )

import ReadPDF                     ( readPDFs )
import ZMidi.IMA.RNSWProf          ( toRNSWProf, toCSV, genHeader )

import Data.Map.Strict             ( Map )


import IMA.InnerMetricalAnalysis hiding           ( Time(..) )

import Text.Printf                 ( printf )
import System.FilePath             ( takeExtension, takeFileName, (</>), (<.>) )
import Data.Char                   ( toLower )
import Data.Ratio                  ( numerator, denominator, (%) )
import Data.Binary                 ( encodeFile, decodeFile )
import Data.List                   ( intercalate ) 
import qualified Data.ByteString.Lazy as BS ( appendFile )

--------------------------------------------------------------------------------
-- Reading and writing 
--------------------------------------------------------------------------------
         
exportIMAStore :: FilePath -> FilePath -> IO ()
exportIMAStore dir i =   readQMidiScoreSafe i >>= writeIMA
                 
  where writeIMA :: Either String QMidiScore -> IO ()
        writeIMA qm = do let out = dir </> takeFileName i <.> "ima"
                         either (warning i) (encodeFile out) $ toIMAStore i qm
                         putStrLn ("written: " ++ out)
        
readIMAScoreGeneric :: FilePath -> IO (Either String IMAStore)
readIMAScoreGeneric f = 
  case take 4 . map toLower . takeExtension $ f of
    ".mid" -> readQMidiScoreSafe f >>= return . toIMAStore f
    ".ima" -> decodeFile f >>= return . Right
    e      -> error ("Error: " ++ e ++ " is not an accepted file type")  
    
--------------------------------------------------------------------------------
-- Exporting CSV profiles
--------------------------------------------------------------------------------

-- | Processes a MidiFile, calculates the RNSWProf and writes it to a file
-- as CSV 
exportCSVProfs :: Map TimeSig [(Beat, BeatRat)] -> FilePath -> FilePath -> IO ()
exportCSVProfs s o i =   readIMAScoreGeneric i 
                     >>= either (warning i) (BS.appendFile o . toCSV s)
    
writeCSVHeader :: Map TimeSig [(Beat, BeatRat)] -> FilePath -> IO ()
writeCSVHeader m out = writeFile out . genHeader $ m
    
--------------------------------------------------------------------------------
-- Matching
--------------------------------------------------------------------------------
                     
-- TODO promote this pattern
matchIO :: Int -> Map TimeSig [(Beat, BeatRat)] -> IMAStore
        -> IO [TimedSeg TimeSig PMatch]
matchIO v m ima = do ps <- readPDFs ("fit"++show v++".json" )
                     return . pickMeters $ match m ps ima

printMatchLine ::  [TimedSeg TimeSig PMatch] -> IO [TimedSeg TimeSig PMatch]
printMatchLine m = do putStrLn . intercalate "\n" . map printPickMeter $ m 
                      return m

printMatchAgr ::  [TimedSeg TimeSig PMatch] -> IO ()
printMatchAgr = print . avgResult . evalMeter


        
--------------------------------------------------------------------------------
-- Printing the Inner Metrical Analysis
--------------------------------------------------------------------------------

-- printing the inner metrical analysis and the midi score data
printIMA :: IMAStore -> IO ()
printIMA is = mapM_ (starMeter (imaTPB is)) . swMeterSeg $ is where
          
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
    
-- Analyses a MidiFile verbosely by printing the spectral weight profiles
analyseProfile :: Int -> Map TimeSig [(Beat, BeatRat)] -> IMAStore -> IO ()
analyseProfile r s im = 
  do let pp  = swMeterSeg im
         qb  = imaQBins im 
         tb  = imaTPB im 
         
         showSel :: SWMeterSeg -> String
         showSel x = let ts = getEvent . boundary $ x
                     in show . filterBin qb (Rot (r % 12)) s ts
                             . normSWProfByBar  
                             . toSWProfWithTS ts tb . seg $ x
         
         prnt = intercalate "\n"
         
     putStrLn . prnt $ "original profiles" : 
                map (show . normSWProfByBar . seg . toSWProf tb) pp
     putStrLn ("rotation: " ++ show r)
     putStrLn . prnt $ "matched profiles" : map showSel pp
     putStrLn . prnt $ map (show . toRNSWProf qb tb (Rot (r % 12)) s) pp
