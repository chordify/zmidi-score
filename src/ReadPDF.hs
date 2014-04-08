{-# OPTIONS_GHC -Wall           #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
import Data.Aeson                           ( ToJSON (..), FromJSON (..), decode )
import GHC.Generics                         ( Generic )
import Data.Vector                          ( Vector )
import qualified Data.Vector          as V  ( length, head, fromList )
import Data.Matrix                          
-- import ZMidi.Score.Datatypes   (TimeSig (..))
import qualified Data.ByteString.Lazy as BL ( readFile )


data ToPDF = ToPDF { mu        :: Vector Double
                   , sigma     :: [[Double]]
                   , inv_sigma :: [[Double]]
                   , det       :: Vector Double -- Singleton
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
      detSigma = V.head . det $ tpdf
      mat1 = 
  in (recip ((2*pi)**(k/2) * sqrt(detSigma))) 
    * exp (getElem 1 1 . negate . scaleMatrix 0.5 
    $ (transpose $ x-m) * invSigma * (x-m) ) 
    
-- read the data
-- BL.readFile "out4.fit.jsonlite.json" >>= return . decode :: IO (Maybe [ToPDF])