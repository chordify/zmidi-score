{-# OPTIONS_GHC -Wall           #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
module ReadPDF where
import Data.Aeson                           ( ToJSON (..), FromJSON (..), decode )
import GHC.Generics                         ( Generic )
import Data.Vector                          ( Vector )
import qualified Data.Vector          as V  ( length, head, fromList )
import Data.Matrix                          
import ZMidi.Score.Datatypes                (TimeSig (..))
-- import Data.Function                        ( on )
import Data.Ord                             ( comparing, Down (..))
import Data.List                            ( sortBy )
import qualified Data.ByteString.Lazy as BL ( readFile )


data ToPDF = ToPDF { mu        :: Vector Double
                   , sigma     :: [[Double]]
                   , inv_sigma :: [[Double]]
                   , prior     :: Vector Double -- Singleton
                   , meter     :: (Int,Int)
                   } deriving (Show, Generic)

instance ToJSON ToPDF
instance FromJSON ToPDF               
                   
-- | The type of probablility density functions
type PDF a = a -> Double
                   
-- | multivariate normal
multiNormal :: ToPDF -> PDF (Vector Double)
multiNormal tpdf xv = 
  let k        = fromIntegral . V.length . mu $ tpdf
      m        = colVector . mu $ tpdf
      x        = colVector xv
      invSigma = fromLists . inv_sigma $ tpdf
      detSigma = detLU . fromLists . sigma $ tpdf
  in (recip ((2*pi)**(k/2) * sqrt(detSigma))) 
    * exp (getElem 1 1 . negate . scaleMatrix 0.5 
    $ (transpose $ x-m) * invSigma * (x-m) )   
    
-- read the data
readPDFs :: FilePath -> IO [ToPDF]
readPDFs fp = do d <- BL.readFile fp >>= return . decode 
                 case d of
                   Just x -> return x
                   _      -> error "There was a type / JSON missmatch"


pdfTimeSig :: ToPDF -> TimeSig
pdfTimeSig tpdf = let (n,d) = meter tpdf in TimeSig n d 0 0