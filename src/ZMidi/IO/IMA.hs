{-# OPTIONS_GHC -Wall                   #-}
module ZMidi.IO.IMA ( readNSWPStoreGeneric
                    , exportNSWPStore
                    , exportCSVProfs
                    , writeCSVHeader
                    , Print (..)
                    , pHeader
                    , readMatchPutLn
                    , trainRotPrior
                    , selectMaxWeightBins
                    -- | * Printing
                    , printMatchLine
                    , printMatchAgr
                    , printIMA
                    , analyseProfile
                    -- * JSON import and export
                    , writeJSON
                    , readJSON
                    ) where

import ZMidi.Score
import ZMidi.IO.Common             -- ( readQMidiScoreSafe, warning )
import ZMidi.IMA.Internal
import ZMidi.IMA.Analyse
import ZMidi.IMA.NSWProf           ( NSWPStore (..), NSWProf, setGT, showGTMRProf )
import ZMidi.IMA.GTInfo            ( GTInfo (..), GTMR (..) )
import ZMidi.IMA.SelectProfBins    ( QBinSelection, sumNSWProf, filterBin )
import ZMidi.IMA.Rotations         ( Rot (..),  Rotations, RPrior (..)
                                   , stdRotations, threePerNum )
import ZMidi.IMA.RNSWMatch         ( PMatch, pickMeters, dontPickMeters
                                   , matchNSWPStore, pickMaxRotation
                                   , avgResult, evalMeter, printGtPMatch )
import ZMidi.IMA.TimeSigSeg        ( TimedSeg (..) )

import ReadPDF                     ( IMAPDF )
import ZMidi.IMA.RNSWProf          ( toCSV, genHeader )

import Data.Map.Strict             ( Map, fromList, toList, foldrWithKey )
-- import qualified Data.Map.Strict as M ( map )

import IMA.InnerMetricalAnalysis hiding           ( Time(..) )

import Text.Printf                 ( printf )
import System.FilePath             ( takeExtension, takeFileName, (</>), (<.>) )
import Data.Char                   ( toLower )
import Data.Ratio                  ( numerator, denominator )
import Data.Binary                 ( encodeFile, decodeFile )
import Data.List                   ( intercalate ) 
import qualified Data.ByteString.Lazy as BL ( appendFile, readFile, writeFile )
import Control.DeepSeq
import Data.Aeson                  ( ToJSON (..), FromJSON (..), decode, encode )

--------------------------------------------------------------------------------
-- Reading and writing 
--------------------------------------------------------------------------------
                         
exportNSWPStore :: FilePath -> FilePath -> IO ()
exportNSWPStore dir i = readNSWPStoreGeneric i >>= either (warning i) write
                 
  where write :: NSWPStore -> IO ()
        write n = do let out = dir </> takeFileName i <.> "prof"
                     encodeFile out n
                     putStrLn ("written: " ++ out)
    
readNSWPStoreGeneric :: FilePath -> IO (Either String NSWPStore)
readNSWPStoreGeneric f =  
  case map toLower . takeExtension $ f of
    ".mid"  -> readQMidiScoreSafe f >>= return . (toNSWPStore f)
    ".prof" -> decodeFile f >>= return . Right 
    ".midi" -> readQMidiScoreSafe f >>= return . (toNSWPStore f)
    e       -> error ("Error: " ++ e ++ " is not an accepted file type")
--------------------------------------------------------------------------------
-- Exporting CSV profiles
--------------------------------------------------------------------------------

-- | Processes a MidiFile, calculates the RNSWProf and writes it to a file
-- as CSV 
exportCSVProfs :: QBinSelection -> FilePath -> FilePath -> IO ()
exportCSVProfs s o i =   readNSWPStoreGeneric i 
                     >>= either (warning i) (BL.appendFile o) . (>>= toCSV s)
    
writeCSVHeader :: QBinSelection -> FilePath -> IO ()
writeCSVHeader m out = writeFile out . genHeader $ m

--------------------------------------------------------------------------------
-- Reading / Writing JSON
--------------------------------------------------------------------------------
     
writeJSON :: ToJSON a => FilePath -> Map TimeSig a -> IO ()
writeJSON fp = BL.writeFile fp . encode . toList 

readJSON :: FromJSON a => FilePath -> IO (Map TimeSig a)
readJSON fp = do mr <- BL.readFile fp >>= return . decode
                 case mr of 
                   Just r  -> do putStrLn ("read TimeSig Map: " ++ fp)
                                 return . fromList $ r 
                   Nothing -> error "readRotations: cannot parse rotations JSON"  


--------------------------------------------------------------------------------
-- Matching
--------------------------------------------------------------------------------  

data Print = PRot | PFile | None

readMatchPutLn :: Print -> QBinSelection -> [IMAPDF] -> Rotations 
               -> Maybe [GTInfo] -> FilePath -> IO (Maybe ([(GTMR, PMatch)]))
readMatchPutLn prnt s ps r mGT fp = 
  do let f = case prnt of
               PRot  -> printMatchLine . dontPickMeters 
               PFile -> printMatchLine . pickMeters 
               None  -> \x -> return $!! pickMeters $!! x 
         -- update a ground-truth, if any
         g = maybe id setGT mGT
         
     -- read the MIDI or profile data
     ima <- readNSWPStoreGeneric fp :: IO (Either String NSWPStore)
     case ima of
       Left  w -> warning fp w >> return Nothing 
       Right x -> (f . matchNSWPStore r s ps . g $ x) >>= return . Just

printMatchLine ::  [(GTMR, PMatch)] -> IO [(GTMR, PMatch)]
printMatchLine m = do putStrLn . intercalate "\n" . map printGtPMatch $ m 
                      return m

printMatchAgr ::  [(GTMR, PMatch)] -> IO ()
printMatchAgr = print . avgResult . evalMeter

trainRotPrior :: QBinSelection -> [IMAPDF] -> FilePath 
              -> Map TimeSig (Map Rot RPrior) 
              -> IO (Map TimeSig (Map Rot RPrior))  
trainRotPrior s ps fp m = 
  do let r   = stdRotations (QBins 12) threePerNum
         f x = pickMaxRotation (matchNSWPStore r s ps x) m
     ima <- readNSWPStoreGeneric fp :: IO (Either String NSWPStore)
     either (\x -> warning fp x >> return m) return (ima >>= f)

pHeader :: IO ()
pHeader = putStrLn "Annotated\tEstimated\tMatch\tLog-Prob\tAnnotated\tEstimated\tMatch\tRot-Prior\tFile"
     
--------------------------------------------------------------------------------
-- QBin Selection
--------------------------------------------------------------------------------

selectMaxWeightBins :: FilePath -> Map TimeSig NSWProf -> IO (Map TimeSig NSWProf)
selectMaxWeightBins fp m = do d <- readNSWPStoreGeneric fp 
                              case d >>= sumNSWProf m of
                                Left  w -> warning fp w >> return m
                                Right p -> return p 

        
--------------------------------------------------------------------------------
-- Printing the Inner Metrical Analysis
--------------------------------------------------------------------------------

-- printing the inner metrical analysis and the midi score data
printIMA :: QMidiScore -> IO ()
printIMA m = mapM_ (starMeter (ticksPerBeat . qMidiScore $ m)) 
           . either error id . doIMApreprocess $ m where
          
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
                  
    d = fromIntegral . maximum . map (snd . getEvent) $ s :: Double
    
    toStar :: SWeight -> String
    toStar x = stars (fromIntegral x / d)
                  
    showMSE :: Maybe ScoreEvent -> String
    showMSE = maybe "    " (show . pitch) 
    
-- NOTE: for the plotting of profiles it would be good to revive the 
-- time-signature and rotation CLI arguments. 
    
-- Analyses a MidiFile verbosely by printing the spectral weight profiles
analyseProfile :: Maybe [GTInfo] -> QBinSelection -> NSWPStore -> IO ()
analyseProfile mGT s n =
  putStrLn . intercalate "\n" $ (nswpsFile n : (concatMap prnt . nswps . g $ n))
  
  where -- g = maybe (const (error "no groundtruth")) setGT mGT :: NSWPStore -> NSWPStore
        g = maybe id setGT mGT
        
        prnt :: (GTMR, Map TimeSig NSWProf) -> [String]
        prnt x@(GTMR _ts r, m) = showGTMRProf x : "qbin selections:" 
                               : foldrWithKey select [] m
        
          where select :: TimeSig -> NSWProf -> [String] -> [String]
                select t p y = ("TimeSig:" ++ show t) 
                             : (show $ filterBin r s t p) : y
                                
       
