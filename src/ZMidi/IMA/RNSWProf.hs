module ZMidi.IMA.RNSWProf ( RNSWProf (..)
                          -- , toRNSWProf
                          , toRNSWProfWithTS
                          , toDoubles
                          , toCSV
                          , genHeader
                          ) where
                     
import Data.ByteString.Lazy          ( ByteString )
-- import qualified Data.ByteString.Lazy as BL 
import Data.Csv              hiding ()             
import ZMidi.Score.Datatypes 
import ZMidi.Score.Quantise          ( QBins)
import ZMidi.IMA.NSWProf             ( NSWeight (..), normSWProfByBar, NSWProf
                                     , NSWPStore (..), getProf )
import ZMidi.IMA.Analyse             ( toSWProfWithTS, SWMeterSeg
                                     , IMAStore (..), imaQBins, imaTPB )
import ZMidi.IMA.TimeSigSeg          ( TimedSeg (..))
import ZMidi.IMA.SelectProfBins      ( filterToList, Rot (..), QBinSelection, getSel )
import Data.List                     ( intercalate )
import Data.Maybe                    ( fromJust )
import Data.Ratio                    ( numerator, denominator)
import Data.Map.Strict               ( Map )
import qualified Data.Map.Strict as M( lookup )
import Text.Printf                   ( printf )

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

toDoubles :: RNSWProf -> [Double]
toDoubles (RNSWProf t w) = map nsweight w

-- | Top-level function that converts a 'QMidiFile' into CSV-writeable profiles
toCSV :: Map TimeSig [(Beat, BeatRat)] -> NSWPStore -> Either String ByteString
toCSV s n = 
  let 
      f :: (TimeSig, Map TimeSig NSWProf) -> Either String RNSWProf
      f (ts, ps) = case M.lookup ts ps of
           Just p  -> Right $ toRNSWProfWithTS (nswpsQBins n) 0 ts s p -- no rotation
           _ -> Left ("toCSV: TimeSignature not found in NSWPStore: " ++ show ts)
  
  in (mapM f . nswps $ n) >>= return . encode
               
-- | transforms a 'NSWProf' into a n 'RNSWProf' given a particular 'TimeSig'
toRNSWProfWithTS :: QBins -> Rot -> TimeSig -> Map TimeSig [(Beat, BeatRat)] 
           -> NSWProf -> RNSWProf
toRNSWProfWithTS q r ts s = RNSWProf ts . filterToList q r s ts
            
-- writes a CSV Header to a file 
genHeader :: Map TimeSig [(Beat, BeatRat)] -> String
genHeader m = (++ "\n") . intercalate "," $ "meter" : 
              (map printKey . fromJust . M.lookup (TimeSig 4 4 0 0) $ m)
            
            