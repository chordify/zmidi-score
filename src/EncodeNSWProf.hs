module Main where
import qualified Data.ByteString.Lazy as BL
import Data.Csv              hiding ()             
import ZMidi.Score.Datatypes hiding  ( numerator, denominator )
import ZMidi.Score.Quantise          ( QMidiScore (..), ShortestNote (..))
import ZMidi.IO.Common               ( readQMidiScoreSafe, mapDir, mapDir_, warning)
import Ragtime.NSWProf
import Ragtime.MidiIMA               ( doIMA, toNSWProfWithTS, fourBarFilter
                                     , emptySegFilter, SWMeterSeg, NSWDist (..)
                                     , PMatch (..), pickMeters, printPickMeter)
import Ragtime.TimeSigSeg            ( TimedSeg (..))
import Ragtime.SelectQBins           ( selectQBins, filterByQBinStrength
                                     , printMeterStats, filterToList )
import System.Environment            ( getArgs )
import Data.List                     ( intercalate )
import Data.Maybe                    ( fromJust )
import Data.Ratio                    ( numerator, denominator)
import Data.Map.Strict               ( Map )
import qualified Data.Map.Strict as M( lookup )

import ReadPDF
-- import Debug.Trace

-- | Reduced Normalised Spectral Weight Profile. It contains a list with the 
-- most a prominent weights of a 'NSWProf', ordered by their weight.
data RNSWProf = RNSWProf TimeSig [NSWeight]

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
  
match :: TPB -> Int -> Map TimeSig [(Beat, BeatRat)] -> [ToPDF] -> [SWMeterSeg] 
      -> [TimedSeg TimeSig [PMatch]]
-- match tb vars s pdfs dat = [ getProb d p | d <- dat, p <- pdfs ] where
match tb vars s pdfs dat = map update dat where

  update :: SWMeterSeg ->  TimedSeg TimeSig [PMatch]
  update s = fmap (const (map (getProb s) pdfs)) s

  getProb :: SWMeterSeg -> ToPDF -> PMatch
  getProb sg pdf = let ts = pdfTimeSig pdf
                       d  = toDoubles vars $ toRNSWProf tb (const ts) s sg
                       p  = NSWDist $ log (pdfPrior pdf) + log (multiNormal pdf d)
                   in PMatch ts p 0
                   -- in traceShow d (ts, log (pdfPrior pdf) + log (multiNormal pdf d))
                 
-- matchIO :: Int ->  Map TimeSig [(Beat, BeatRat)] -> FilePath -> IO [TimedSeg TimeSig [PMatch]]
-- matchIO v m fp = do qm <- readQMidiScoreSafe FourtyEighth fp >>= either (warning fp) return
                    -- ps <- readPDFs "fit.json" 
                    -- let dat = either error id $ preprocess qm 
                    -- return . match (ticksPerBeat . qMidiScore $ qm) v m ps $ dat

-- TODO promote this pattern
matchIO :: Int ->  Map TimeSig [(Beat, BeatRat)] -> FilePath -> IO ()
matchIO v m fp = do ps <- readPDFs "fit.json" 
                    readQMidiScoreSafe FourtyEighth fp 
                      >>= return . (>>= doMatch ps)
                      >>= either (warning fp) putStrLn

  where doMatch :: [ToPDF] -> QMidiScore-> Either String String
        doMatch ps qm = preprocess qm 
              >>= return . match (ticksPerBeat . qMidiScore $ qm) v m ps 
              >>= pickMeters 
              >>= return . intercalate "\n" 
                         . map (\x -> fp ++ "\t" ++ printPickMeter x)

                    
-- | Top-level function that converts a 'QMidiFile' into CSV-writeable profiles
toCSV :: Map TimeSig [(Beat, BeatRat)] -> QMidiScore -> Either String [RNSWProf]
toCSV s qm = preprocess qm 
         >>= return . map (toRNSWProf (ticksPerBeat . qMidiScore $ qm) id s)

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
toRNSWProf :: TPB -> (TimeSig -> TimeSig) -> Map TimeSig [(Beat, BeatRat)] 
           -> SWMeterSeg -> RNSWProf
toRNSWProf tb f s (TimedSeg (Timed _ ts) d) = 
  RNSWProf (f ts) . filterToList s (f ts) 
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
  
-- testing
main :: IO ()
main = 
  do let out = "train.barnorm.sqr.smth.log.csv"
         vars = 8
     arg <- getArgs 
     m   <- readNSWProf "ragtimeMeterProfilesTrain_2014-03-25.bin"  
            >>= return . selectQBins vars
     case arg of
       ["-f", fp] -> writeHeader m out >> processMidi m out fp
       ["-d", fp] -> writeHeader m out >> mapDir (processMidi m out) fp >> return ()
       ["-s", fp] -> readNSWProf fp >>= printMeterStats . filterByQBinStrength
       ["-m", fp] -> matchIO vars m fp
       ["-a", fp] -> mapDir_ (matchIO vars m) fp
       
       _ -> error "usage: -f <filename> -d <directory>"

       
-- test :: FilePath -> IO ()
-- test fp = do m   <- readNSWProf "..\\ragtimeMeterProfilesTrain_2014-03-25.bin"  
                      -- >>= return . selectQBins 8
             -- qm   <- readQMidiScoreSafe FourtyEighth fp >>= either error return
             -- print . match 8 m qm $ myPDF
             -- pdfs <- readPDFs "..\\fit.json" 
             -- -- print pdfs
             -- -- mapM_ (print . match 8 m qm) pdfs
             -- mapM_ (print . match 8 m qm) pdfs
             
-- myPDF = ToPDF {mu = [7.03235821551444 ,6.96367469532102 ,6.70004713340952 ,6.62435859280952  ,3.22231061445805 ,3.15320092899785,3.08302662754713,3.04189918678881], sigma = [[0.664789550919293,0.692202331350415,0.714080076588,0.731208473039417,1.20731316103041,1.21675534787117,1.2204633132475,1.22513622158397],[0.692202331350415,0.727263884349832,0.745472948102641,0.764069485242172,1.22520528440333,1.23495456459712,1.23820828179735,1.24303669589439],[0.714080076588,0.745472948102641,0.83132257363977,0.852368186638318,1.32634203229495,1.33852414021486,1.34487559039866,1.35005247703635],[0.731208473039417,0.764069485242172,0.852368186638318,0.880000920989508,1.3224141285255,1.33574380227114,1.34236693332217,1.34837514470046],[1.20731316103041,1.22520528440333,1.32634203229495,1.3224141285255,129.463478811362,129.40998977382,129.338415479072,129.295118016827],[1.21675534787117,1.23495456459712,1.33852414021486,1.33574380227114,129.40998977382,129.369775002705,129.301059408905,129.259275733795],[1.2204633132475,1.23820828179735,1.34487559039866,1.34236693332217,129.338415479072,129.301059408905,129.259938550403,129.219780881677],[1.22513622158397,1.24303669589439,1.35005247703635,1.34837514470046,129.295118016827,129.259275733795,129.219780881677,129.182914041182]], inv_sigma = [[180.255261279429,-159.016439232259,-34.323998405441,21.6889553864088,0.489241494265768,1.87158799263829,5.0551814649114,-7.46604706441798],[-159.016439232276,157.399717768508,14.0677494627516,-18.3043605479577,-1.86152347965455,-1.73325547060256,-0.370931450053277,4.00601773492172],[-34.3239984049919,14.0677494623021,201.640548290185,-179.259070776244,-5.43386736530656,8.65281456001335,-40.0914434086323,36.8374683736226],[21.6889553859902,-18.3043605475378,-179.259070776259,172.933092730438,7.48236476510332,-9.34744008311794,36.7140605909342,-34.821642342273],[0.489241494731243,-1.86152348008409,-5.43386736539641,7.48236476517877,81.797617127327,-88.0097117282523,-25.0463044703143,31.2384851124172],[1.87158799195822,-1.73325546997882,8.65281456016593,-9.34744008324486,-88.0097117282695,132.25072657177,-29.0970981524383,-15.1312794134028],[5.05518146500425,-0.370931450140715,-40.0914434083483,36.7140605906524,-25.0463044704667,-29.097098152308,407.851534363076,-353.794172357506],[-7.46604706429646,4.00601773481516,36.8374683732759,-34.8216423419397,31.238485112587,-15.1312794135505,-353.794172357528,337.788226647478]], prior = [0.35632183908046], meter = (4,4)}
-- ws                = [6.159375816950155,5.981357391919658,5.9309427854406644,5.913070724606611,4.741358302803805,4.72421442873184, 0,0]
       
-- copied from RagPatIMA Checks for a valid time signature
timeSigCheck :: QMidiScore -> Either String QMidiScore
timeSigCheck ms | hasTimeSigs (qMidiScore ms) = Right ms
                | otherwise = Left "Has no valid time signature" 

            
            