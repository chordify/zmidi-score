{-# OPTIONS_GHC -Wall           #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
module ReadPDF ( IMAPDF (..)
               , readPDFs
               ) where

import Data.Aeson                           ( ToJSON (..), FromJSON (..), decode )
import GHC.Generics                         ( Generic )
import Data.Matrix                          
import ZMidi.Score.Datatypes                (TimeSig (..))
import qualified Data.ByteString.Lazy as BL ( readFile )

-- TODO store vector lengths
data ToPDF = ToPDF { mu        :: [ Double ]
                   , sigma     :: [[Double]]
                   , inv_sigma :: [[Double]]
                   , prior     :: [ Double ] -- Singleton
                   , meter     :: (Int,Int)
                   } deriving (Show, Generic)

instance ToJSON ToPDF
instance FromJSON ToPDF               
                   
-- | The type of probability density functions
type PDF a = a -> Double
                   
data IMAPDF = IMAPDF { pdf        :: PDF [Double]
                     , pdfTimeSig :: TimeSig
                     , pdfPrior   :: Double
                     } 
                   
-- | TODO Checks whether the sizes of all lists match
sizeCheck :: ToPDF -> Bool
sizeCheck = undefined

-- read the data
readPDFs :: FilePath -> IO [IMAPDF]
readPDFs fp = do d <- BL.readFile fp >>= return . decode :: IO (Maybe [ToPDF])
                 case d of
                   Just x -> return . map toIMAPDF $ x
                   _      -> error "There was a type / JSON missmatch"                   

toIMAPDF :: ToPDF -> IMAPDF
toIMAPDF tpdf = IMAPDF (multiNormal tpdf) (toPdfTimeSig tpdf) (toPdfPrior tpdf) 
                   
-- | multivariate normal
multiNormal :: ToPDF -> PDF [Double]
multiNormal tpdf l = 
  let k        = length . mu $ tpdf -- dimensionality 
      m        = fromList k 1 . mu $ tpdf          -- column vector of means
      x        = fromList k 1 l                    -- column vector of l
      invSigma = fromLists . inv_sigma $ tpdf      -- inverse covariance matrix
      detSigma = detLU . fromLists . sigma $ tpdf  -- the determinant
  in (recip ( (2*pi) ** (fromIntegral k /2) * sqrt detSigma ))
    * exp (getElem 1 1 . scaleMatrix (-0.5) $ (transpose $ x-m) * invSigma * (x-m))   


toPdfPrior :: ToPDF -> Double
toPdfPrior pdf = case prior pdf of
                [x] -> x
                _   -> error "Incorrect JSON: found multiple priors"
                   
toPdfTimeSig :: ToPDF -> TimeSig
toPdfTimeSig tpdf = let (n,d) = meter tpdf in TimeSig n d 0 0
