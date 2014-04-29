module Main where
import qualified Data.ByteString.Lazy as BL
import Data.Csv              hiding ()             
import ZMidi.Score.Datatypes hiding  ( numerator, denominator )
import ZMidi.Score.Quantise          ( QMidiScore (..), ShortestNote (..), toQBins, QBins)
import ZMidi.IO.Common               ( readQMidiScoreSafe, mapDir_
                                     , warning, ioWithWarning)
import Ragtime.NSWProf
import Ragtime.MidiIMA               ( doIMA, toNSWProfWithTS, fourBarFilter
                                     , emptySegFilter, SWMeterSeg, NSWDist (..)
                                     , PMatch (..), pickMeters, printPickMeter
                                     , toSWProf )
import Ragtime.TimeSigSeg            ( TimedSeg (..))
import Ragtime.SelectQBins           ( selectQBins, printMeterStats, filterToList
                                     , Rot (..), filterBin )
import System.Environment            ( getArgs )
import Data.List                     ( intercalate )
import Data.Maybe                    ( fromJust )
import Data.Ratio                    ( numerator, denominator)
import Data.Map.Strict               ( Map )
import qualified Data.Map.Strict as M( lookup )
import Text.Printf                   ( printf )

import ReadPDF
-- import Debug.Trace

-- | Reduced Normalised Spectral Weight Profile. It contains a list with the 
-- most a prominent weights of a 'NSWProf', ordered by their weight.
data RNSWProf = RNSWProf TimeSig [NSWeight] deriving (Eq)

instance Show RNSWProf where
  show (RNSWProf ts l) = show ts ++ ": " ++ concatMap (printf "%.2f ") l

instance ToField    NSWeight where toField (NSWeight w) = toField w
instance ToField    TimeSig  where 
  toField (TimeSig n d _ _) = toField (show n ++ "/" ++ show d)
  toField  NoTimeSig        = toField "n/a"
  
instance ToRecord RNSWProf where
  toRecord (RNSWProf ts p) = record (toField ts : map toField p)

printKey :: (Beat, BeatRat) -> String
printKey (Beat b, BeatRat br) = show b ++ "." 
                             ++ show (numerator br) ++ "." 
                             ++ show (denominator br)

toDoubles :: Int -> RNSWProf -> [Double]
toDoubles i (RNSWProf _ts ws) = map nsweight . take i $ ws

-- type SWMeterSeg = TimedSeg TimeSig [Timed (Maybe ScoreEvent, SWeight)]
  
match :: QBins -> TPB -> Int -> Rot -> Map TimeSig [(Beat, BeatRat)] -> [ToPDF] -> [SWMeterSeg] 
      -> [TimedSeg TimeSig [PMatch]]
match qb tb vars mr s pdfs dat = map update dat where

  update :: SWMeterSeg ->  TimedSeg TimeSig [PMatch]
  update x = fmap (const [getProb x r p | r <- [mr, pred mr .. 0], p <- pdfs]) x

  getProb :: SWMeterSeg -> Rot -> ToPDF -> PMatch
  getProb sg r pdf = let ts = pdfTimeSig pdf
                         d  = toDoubles vars $ toRNSWProf qb tb r (const ts) s sg
                         p  = NSWDist $ log (pdfPrior pdf) + log (multiNormal pdf d)
                     in PMatch ts p r
                     -- in traceShow m m where m  = PMatch ts p r
                    
-- TODO promote this pattern
matchIO :: Int -> Rot ->  Map TimeSig [(Beat, BeatRat)] -> FilePath -> IO ()
matchIO v r m fp = do ps <- readPDFs "fit12.json" 
                      ioWithWarning (readQMidiScoreSafe FourtyEighth) 
                                    (doMatch ps) printMatch fp

  where doMatch :: [ToPDF] -> QMidiScore-> Either String [TimedSeg TimeSig PMatch]
        doMatch ps qm = do segs <- preprocess qm 
                           let tb = ticksPerBeat . qMidiScore $ qm
                               qb = toQBins . qShortestNote $ qm
                           pickMeters . match qb tb v r m ps $ segs
        
        printMatch ::  [TimedSeg TimeSig PMatch] -> IO ()
        printMatch = putStrLn . intercalate "\n" 
                              . map (\x -> fp ++ "\t" ++ printPickMeter x)

                    
-- | Top-level function that converts a 'QMidiFile' into CSV-writeable profiles
toCSV :: Map TimeSig [(Beat, BeatRat)] -> QMidiScore -> Either String [RNSWProf]
toCSV s qm = do segs <- preprocess qm 
                let tb = ticksPerBeat . qMidiScore $ qm
                    qb = toQBins . qShortestNote $ qm
                return . map (toRNSWProf qb tb 0 id s) $ segs -- no rotation

-- | Pre-processes a 'QMidiFile' and returns the IMA weights and score data
-- for segments that represent one time signature 
preprocess :: QMidiScore -> Either String [SWMeterSeg]
preprocess qm =   timeSigCheck qm
              >>= doIMA
              >>= fourBarFilter tb
              >>= emptySegFilter 
                where tb = ticksPerBeat . qMidiScore $ qm

-- | Converts one segment into a RNSWProf, preserving only the profile bins
-- with heavy weights. The 'TimeSig' argument determines the time signature
-- of the 'RNSWProf'.
toRNSWProf :: QBins -> TPB -> Rot -> (TimeSig -> TimeSig) -> Map TimeSig [(Beat, BeatRat)] 
           -> SWMeterSeg -> RNSWProf
toRNSWProf q tb r f s (TimedSeg (Timed _ ts) d) = 
  RNSWProf (f ts) . filterToList q r s (f ts) 
                  . normSWProfByBar 
                  . toNSWProfWithTS (f ts) tb $ d 

-- | Processes a MidiFile, calculates the RNSWProf and writes it to a file
-- as CSV 
processMidi :: Map TimeSig [(Beat, BeatRat)] -> FilePath -> FilePath -> IO ()
processMidi s out infp = do qm <- readQMidiScoreSafe FourtyEighth infp
                            case qm >>= toCSV s of
                              Left  err -> warning infp err
                              Right csv -> BL.appendFile out . encode $ csv 
  
writeHeader :: Map TimeSig [(Beat, BeatRat)] -> FilePath -> IO()
writeHeader m out = 
  writeFile out . (++ "\n") . intercalate "," $ "meter" : 
                  (map printKey . fromJust . M.lookup (TimeSig 4 4 0 0) $ m)
  
analyseMidi :: Rot -> Map TimeSig [(Beat, BeatRat)] -> FilePath -> IO ()
analyseMidi r s fp = ioWithWarning (readQMidiScoreSafe FourtyEighth)
                                   analyse putStrLn fp where
     
  analyse :: QMidiScore -> Either String String
  analyse qm = do pp <- preprocess qm 
                  let qb  = toQBins . qShortestNote $ qm
                      tb = ticksPerBeat . qMidiScore $ qm 
                      
                      showSel :: SWMeterSeg -> String
                      showSel x = show . filterBin qb r s (getEvent . boundary $ x) 
                                . normSWProfByBar . seg . toSWProf tb $ x
                      
                      prf = intercalate "\n" $ "original profiles" : 
                            map (show . normSWProfByBar . seg . toSWProf tb) pp
                      rst = "rotation: " ++ show (rot r)              
                      sel = intercalate "\n" $ "matched profiles" : map showSel pp
                      rns = intercalate "\n" $ map (show . toRNSWProf qb tb r id s) pp

                  return . intercalate "\n" $ [rst, prf, sel, rns]
                  
-- testing
main :: IO ()
main = 
  do -- parameters
     let out   = "train.barnorm.sqr.smth.log.csv"
         profs = "ragtimeMeterProfilesTrain_2014-03-25.bin" 
         vars  = 12
         rt    = 47
     arg <- getArgs 
     m   <- readNSWProf profs >>= return . selectQBins vars
     case arg of
       ["-f", fp, "-r", r] -> analyseMidi (Rot $ read r) m fp
       ["-f", fp] -> analyseMidi rt m fp
       ["-d", fp] -> writeHeader m out >> mapDir_ (processMidi m out) fp 
       ["-s"    ] -> readNSWProf profs >>= printMeterStats 
       ["-m", fp] -> matchIO vars rt m fp
       ["-a", fp] -> mapDir_ (matchIO vars rt m) fp
       
       _ -> error "usage: -f <filename> -d <directory>"

-- copied from RagPatIMA Checks for a valid time signature
timeSigCheck :: QMidiScore -> Either String QMidiScore
timeSigCheck ms | hasTimeSigs (qMidiScore ms) = Right ms
                | otherwise = Left "Has no valid time signature" 

            
            