module ZMidi.IMA.RNSWProf ( RNSWProf (..)
                          , toRNSWProf
                          , toDoubles
                          , toCSV
                          , genHeader
                          ) where
                     
import Data.ByteString.Lazy          ( ByteString )
-- import qualified Data.ByteString.Lazy as BL 
import Data.Csv              hiding ()             
import ZMidi.Score.Datatypes 
import ZMidi.Score.Quantise          ( QBins)
import ZMidi.IMA.NSWProf             ( NSWeight (..), normSWProfByBar )
import ZMidi.IMA.Analyse             ( toNSWProfWithTS, SWMeterSeg
                                     , IMAStore (..), imaQBins, imaTPB )
import Ragtime.TimeSigSeg            ( TimedSeg (..))
import ZMidi.IMA.SelectProfBins      ( filterToList, Rot (..) )
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

toDoubles :: Int -> RNSWProf -> [Double]
toDoubles i (RNSWProf _ts ws) = map nsweight . take i $ ws

-- type SWMeterSeg = TimedSeg TimeSig [Timed (Maybe ScoreEvent, SWeight)]

-- | Top-level function that converts a 'QMidiFile' into CSV-writeable profiles
toCSV :: Map TimeSig [(Beat, BeatRat)] -> IMAStore -> ByteString
toCSV s i = let f = toRNSWProf (imaQBins i) (imaTPB i) 0 id s -- no rotation
            in  encode . map f . swMeterSeg $ i


-- | Converts one segment into a RNSWProf, preserving only the profile bins
-- with heavy weights. The 'TimeSig' argument determines the time signature
-- of the 'RNSWProf'.
toRNSWProf :: QBins -> TPB -> Rot -> (TimeSig -> TimeSig) -> Map TimeSig [(Beat, BeatRat)] 
           -> SWMeterSeg -> RNSWProf
toRNSWProf q tb r f s (TimedSeg (Timed _ ts) d) = 
  RNSWProf (f ts) . filterToList q r s (f ts) 
                  . normSWProfByBar 
                  . toNSWProfWithTS (f ts) tb $ d 

-- writes a CSV Header to a file 
genHeader :: Map TimeSig [(Beat, BeatRat)] -> String
genHeader m = (++ "\n") . intercalate "," $ "meter" : 
              (map printKey . fromJust . M.lookup (TimeSig 4 4 0 0) $ m)
            
            