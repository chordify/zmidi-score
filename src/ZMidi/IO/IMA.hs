{-# OPTIONS_GHC -Wall                   #-}
module ZMidi.IO.IMA ( exportIMAStore
                    , exportNSWPStore
                    , readIMAScoreGeneric
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
                    ) where

import ZMidi.Score
import ZMidi.IO.Common             -- ( readQMidiScoreSafe, warning )
import ZMidi.IMA.Internal
import ZMidi.IMA.Analyse
import ZMidi.IMA.NSWProf           ( normSWProfByBar, NSWPStore (..), NSWProf )
import ZMidi.IMA.MeterGT           ( MeterGT (..), setGT )
import ZMidi.IMA.SelectProfBins    ( Rot (..), sumNSWProf
                                   , QBinSelection, Rotations, threePerNum
                                   , stdRotations, RPrior )
import ZMidi.IMA.RNSWMatch         ( PMatch, pickMeters, dontPickMeters
                                   , matchNSWPStore, pickMaxRotation
                                   , avgResult, evalMeter, printGtPMatch )
import ZMidi.IMA.TimeSigSeg        ( TimedSeg (..) )

import ReadPDF                     ( IMAPDF )
import ZMidi.IMA.RNSWProf          ( toCSV, genHeader )

import Data.Map.Strict             ( Map )
-- import qualified Data.Map.Strict as M ( map )

import IMA.InnerMetricalAnalysis hiding           ( Time(..) )

import Text.Printf                 ( printf )
import System.FilePath             ( takeExtension, takeFileName, (</>), (<.>) )
import Data.Char                   ( toLower )
import Data.Ratio                  ( numerator, denominator )
import Data.Binary                 ( encodeFile, decodeFile )
import Data.List                   ( intercalate ) 
import qualified Data.ByteString.Lazy as BS ( appendFile )
import Control.DeepSeq

--------------------------------------------------------------------------------
-- Reading and writing 
--------------------------------------------------------------------------------
         
exportIMAStore :: FilePath -> FilePath -> IO ()
exportIMAStore dir i =   readQMidiScoreSafe i >>= writeIMA
                 
  where writeIMA :: Either String QMidiScore -> IO ()
        writeIMA qm = do let out = dir </> takeFileName i <.> "ima"
                         either (warning i) (encodeFile out) $ toIMAStore i qm
                         putStrLn ("written: " ++ out)
                         
exportNSWPStore :: FilePath -> FilePath -> IO ()
exportNSWPStore dir i = readNSWPStoreGeneric i >>= either (warning i) write
                 
  where write :: NSWPStore -> IO ()
        write n = do let out = dir </> takeFileName i <.> "prof"
                     encodeFile out n
                     putStrLn ("written: " ++ out)
        
readIMAScoreGeneric :: FilePath -> IO (Either String IMAStore)
readIMAScoreGeneric f = 
  case take 4 . map toLower . takeExtension $ f of
    ".mid" -> readQMidiScoreSafe f >>= return . toIMAStore f
    ".ima" -> do r <- decodeFile f 
                 r `seq` return (Right r)
    e      -> error ("Error: " ++ e ++ " is not an accepted file type")  
    
readNSWPStoreGeneric :: FilePath -> IO (Either String NSWPStore)
readNSWPStoreGeneric f =  
  case take 5 . map toLower . takeExtension $ f of
    ".prof" -> decodeFile f >>= return . Right 
    _       -> readIMAScoreGeneric f >>= return . fmap toNSWPStore 
--------------------------------------------------------------------------------
-- Exporting CSV profiles
--------------------------------------------------------------------------------

-- | Processes a MidiFile, calculates the RNSWProf and writes it to a file
-- as CSV 
exportCSVProfs :: Map TimeSig [(Beat, BeatRat)] -> FilePath -> FilePath -> IO ()
exportCSVProfs s o i =   readNSWPStoreGeneric i 
                     >>= either (warning i) (BS.appendFile o) . (>>= toCSV s)
    
writeCSVHeader :: Map TimeSig [(Beat, BeatRat)] -> FilePath -> IO ()
writeCSVHeader m out = writeFile out . genHeader $ m
    
--------------------------------------------------------------------------------
-- Matching
--------------------------------------------------------------------------------  
                     
-- TODO promote this pattern
-- TODO remove v variable -> denotes the number of selected bins, and should
-- be controlled by the QBinSelection m

data Print = PRot | PFile | None

readMatchPutLn :: Print -> QBinSelection -> [IMAPDF] -> Rotations 
               -> Maybe [MeterGT [TimeSig]] -> FilePath 
               -> IO (Maybe ([(TimeSig, PMatch)]))
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

printMatchLine ::  [(TimeSig, PMatch)] -> IO [(TimeSig, PMatch)]
printMatchLine m = do putStrLn . intercalate "\n" . map printGtPMatch $ m 
                      return m

printMatchAgr ::  [(TimeSig, PMatch)] -> IO ()
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
pHeader = putStrLn "Annotated\tEstimated\tMatch\tLog-Prob\tRotation\tRot-Prior\tFile"
     
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
-- TODO rewrite printing functions...
analyseProfile :: Int -> Map TimeSig [(Beat, BeatRat)] -> IMAStore -> IO ()
analyseProfile r _s im = 
  do let pp  = swMeterSeg im
         -- qb  = imaQBins im 
         tb  = imaTPB im 
         
         -- showSel :: SWMeterSeg -> String
         -- showSel x = let ts = getEvent . boundary $ x
                     -- in show . filterBin qb (Rot (r % 12)) s ts
                             -- . normSWProfByBar  
                             -- . toSWProfWithTS ts tb . seg $ x
         
         prnt = intercalate "\n"
         
     putStrLn . prnt $ "original profiles" : 
                map (show . normSWProfByBar . seg . toSWProf tb) pp
     putStrLn ("rotation: " ++ show r)
     -- putStrLn . prnt $ "matched profiles" : map showSel pp
     -- putStrLn . prnt $ map (show . toRNSWProfWithTs qb tb (Rot (r % 12)) s) pp
