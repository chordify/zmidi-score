module Main where
import qualified Data.ByteString.Lazy as BL
import Data.Csv              hiding ()             
import ZMidi.Score.Datatypes hiding  ( numerator, denominator )
import ZMidi.Score.Quantise          ( QMidiScore (..), toQBins, ShortestNote (..))
import ZMidi.IO.Common               ( readQMidiScoreSafe, mapDir, warning)
import Ragtime.NSWProf
import Ragtime.MidiIMA               ( doIMA, toNSWProfWithTS, fourBarFilter, emptySegFilter)
import Ragtime.TimeSigSeg            ( TimedSeg (..))
import Ragtime.SelectQBins           ( selectQBins, filterByQBinStrength
                                     , printMeterStats, filterToList, QBinSelection )
import IMA.InnerMetricalAnalysis     ( SWeight (..))
import System.Environment            ( getArgs )
import Data.List                     ( intercalate )
import Data.Maybe                    ( fromJust )
import Data.Ratio                    ( numerator, denominator)
import Data.Map.Strict               ( Map )
import qualified Data.Map.Strict as M( lookup )

import ReadPDF

import Debug.Trace

data NSWProfCSV = NSWProfCSV TimeSig [NSWeight]

instance ToField    NSWeight where toField (NSWeight w) = toField w
instance ToField    TimeSig  where 
  toField (TimeSig n d _ _) = toField (show n ++ "/" ++ show d)
  toField  NoTimeSig        = toField "n/a"
  
instance ToRecord NSWProfCSV where
  toRecord (NSWProfCSV ts p) = record (toField ts : map toField p)

printKey :: (Beat, BeatRat) -> String
printKey (Beat b, BeatRat br) = show b ++ "." 
                             ++ show (numerator br) ++ "." 
                             ++ show (denominator br)

toDoubles :: Int -> NSWProfCSV -> [Double]
toDoubles i (NSWProfCSV _ts ws) = map nsweight . take i $ ws
                             
toCSVwithTS :: TimeSig -> Map TimeSig [(Beat, BeatRat)] -> QMidiScore 
      -> Either String [NSWProfCSV]
toCSVwithTS ts s qm = toCSV' (const ts) s qm
      
toCSV :: Map TimeSig [(Beat, BeatRat)] -> QMidiScore 
      -> Either String [NSWProfCSV]
toCSV s qm = toCSV' id s qm
      
toCSV' :: (TimeSig -> TimeSig) -> Map TimeSig [(Beat, BeatRat)] -> QMidiScore 
      -> Either String [NSWProfCSV]
toCSV' f s qm =   doIMA qm 
           >>= fourBarFilter tb 
           >>= emptySegFilter 
           >>= return . map toProf where

  tb = ticksPerBeat . qMidiScore $ qm
  -- qb = toQBins FourtyEighth

  toProf :: TimedSeg TimeSig [Timed (Maybe ScoreEvent, SWeight)] -> NSWProfCSV
  toProf (TimedSeg (Timed _ ts) td) = 
    NSWProfCSV (f ts) . filterToList s (f ts)          -- only keep the strong pos
                      . normSWProfByBar                -- normalise the profile 
                      . toNSWProfWithTS (f ts) tb $ td -- calculate the profile

processMidi :: Map TimeSig [(Beat, BeatRat)] -> FilePath -> FilePath -> IO ()
processMidi s out infp = do qm <- readQMidiScoreSafe FourtyEighth infp
                            case qm >>= timeSigCheck >>= toCSV s of
                              Left  err -> warning infp err
                              Right csv -> BL.appendFile out . encode $ csv 
  
writeHeader :: Map TimeSig [(Beat, BeatRat)] -> FilePath -> IO()
writeHeader m out = 
  writeFile out . (++ "\n") . intercalate "," $ "meter" : 
                  (map printKey . fromJust . M.lookup (TimeSig 4 4 0 0) $ m)

-- writeHeaderQBinSel :: QBinSelection -> FilePath -> IO ()
-- writeHeaderQBinSel q out =  
  -- writeFile out . (++ "\n") . intercalate "," $ "meter" : 
                  -- (map printKey . fromJust . M.lookup (TimeSig 4 4 0 0) $ q)
  
match :: Int -> Map TimeSig [(Beat, BeatRat)] -> QMidiScore -> ToPDF 
      -> [(TimeSig, Double)]
-- match :: ToPDF -> [Double] -> (TimeSig, Double)
match vars sel qm tpdf = let ts = pdfTimeSig tpdf            -- time signature
                             x  = map (toDoubles vars) . either error id $ toCSVwithTS ts sel qm -- the profile
                             -- pr = pdfPrior $ tpdf            -- the prior
                             -- p  = multiNormal tpdf x         -- probability
                         -- in ( ts, log pr + log p)
                         in map (\y -> traceShow y (ts, log (pdfPrior tpdf) + log (multiNormal tpdf y))) x
  
-- testing
main :: IO ()
main = 
  do let out = "train.barnorm.sqr.log.csv"
     arg <- getArgs 
     m   <- readNSWProf "ragtimeMeterProfilesTrain_2014-03-25.bin"  
            >>= return . selectQBins 8
     case arg of
       ["-f", fp] -> writeHeader m out >> processMidi m out fp
       ["-d", fp] -> writeHeader m out >> mapDir (processMidi m out) fp >> return ()
       ["-s", fp] -> readNSWProf fp >>= printMeterStats . filterByQBinStrength
       ["-test" ] -> readPDFs "fit.json" >>= print
       ["-m", fp] -> do qm   <- readQMidiScoreSafe FourtyEighth fp >>= either error return
                        pdfs <- readPDFs "fit.json" 
                        pdfs `seq` mapM_ (print . match 8 m qm) pdfs
                        
       
       _ -> error "usage: -f <filename> -d <directory>"
       
test :: FilePath -> IO ()
test fp = do m   <- readNSWProf "..\\ragtimeMeterProfilesTrain_2014-03-25.bin"  
                      >>= return . selectQBins 8
             qm   <- readQMidiScoreSafe FourtyEighth fp >>= either error return
             print . match 8 m qm $ myPDF
             pdfs <- readPDFs "..\\fit.json" 
             -- print pdfs
             -- mapM_ (print . match 8 m qm) pdfs
             mapM_ (print . match 8 m qm) pdfs
             
myPDF = ToPDF {mu = [7.03235821551444 ,6.96367469532102 ,6.70004713340952 ,6.62435859280952  ,3.22231061445805 ,3.15320092899785,3.08302662754713,3.04189918678881], sigma = [[0.664789550919293,0.692202331350415,0.714080076588,0.731208473039417,1.20731316103041,1.21675534787117,1.2204633132475,1.22513622158397],[0.692202331350415,0.727263884349832,0.745472948102641,0.764069485242172,1.22520528440333,1.23495456459712,1.23820828179735,1.24303669589439],[0.714080076588,0.745472948102641,0.83132257363977,0.852368186638318,1.32634203229495,1.33852414021486,1.34487559039866,1.35005247703635],[0.731208473039417,0.764069485242172,0.852368186638318,0.880000920989508,1.3224141285255,1.33574380227114,1.34236693332217,1.34837514470046],[1.20731316103041,1.22520528440333,1.32634203229495,1.3224141285255,129.463478811362,129.40998977382,129.338415479072,129.295118016827],[1.21675534787117,1.23495456459712,1.33852414021486,1.33574380227114,129.40998977382,129.369775002705,129.301059408905,129.259275733795],[1.2204633132475,1.23820828179735,1.34487559039866,1.34236693332217,129.338415479072,129.301059408905,129.259938550403,129.219780881677],[1.22513622158397,1.24303669589439,1.35005247703635,1.34837514470046,129.295118016827,129.259275733795,129.219780881677,129.182914041182]], inv_sigma = [[180.255261279429,-159.016439232259,-34.323998405441,21.6889553864088,0.489241494265768,1.87158799263829,5.0551814649114,-7.46604706441798],[-159.016439232276,157.399717768508,14.0677494627516,-18.3043605479577,-1.86152347965455,-1.73325547060256,-0.370931450053277,4.00601773492172],[-34.3239984049919,14.0677494623021,201.640548290185,-179.259070776244,-5.43386736530656,8.65281456001335,-40.0914434086323,36.8374683736226],[21.6889553859902,-18.3043605475378,-179.259070776259,172.933092730438,7.48236476510332,-9.34744008311794,36.7140605909342,-34.821642342273],[0.489241494731243,-1.86152348008409,-5.43386736539641,7.48236476517877,81.797617127327,-88.0097117282523,-25.0463044703143,31.2384851124172],[1.87158799195822,-1.73325546997882,8.65281456016593,-9.34744008324486,-88.0097117282695,132.25072657177,-29.0970981524383,-15.1312794134028],[5.05518146500425,-0.370931450140715,-40.0914434083483,36.7140605906524,-25.0463044704667,-29.097098152308,407.851534363076,-353.794172357506],[-7.46604706429646,4.00601773481516,36.8374683732759,-34.8216423419397,31.238485112587,-15.1312794135505,-353.794172357528,337.788226647478]], prior = [0.35632183908046], meter = (4,4)}
ws                = [6.159375816950155,5.981357391919658,5.9309427854406644,5.913070724606611,4.741358302803805,4.72421442873184, 0,0]
       
-- copied from RagPatIMA Checks for a valid time signature
timeSigCheck :: QMidiScore -> Either String QMidiScore
timeSigCheck ms | hasTimeSigs (qMidiScore ms) = Right ms
                | otherwise = Left "Has no valid time signature" 

            
            