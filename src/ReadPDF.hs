{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
import Data.Aeson
import Data.Aeson.Types
-- import Data.Data
-- import Data.Typeable
import GHC.Generics
import Control.Applicative
import Control.Monad (MonadPlus (..))
import Data.Text (pack, breakOn)
import qualified Data.Text  as T (tail)
import ZMidi.Score.Datatypes (TimeSig (..))

import qualified Data.ByteString.Lazy as BL

-- data MeterPDF  = MeterPDF String ToPDF
-- data MeterPDF  = MeterPDF { pdfTimesig :: TimeSig
                          -- , meterpdf   :: ToPDF
                          -- } deriving Show -- , Data, Typeable, Generic)

-- instance FromJSON TimeSig where
  -- parseJSON (String t) = let (num, den) = breakOn (pack "/") t
                         -- in (\n d -> TimeSig n d 0 0) <$> parseJSON (String num) <*> parseJSON (String (T.tail den))
  -- parseJSON _          = mzero
                           
data ToPDF = ToPDF { -- timeSig  :: String
                     mu        :: [Double]
                   , sigma     :: [[Double]]
                   , inv_sigma :: [[Double]]
                   , meter1    :: [Int]
                   , meter2    :: [Int]
                   } deriving (Show, Generic)

-- instance ToJSON MeterPDF
-- instance FromJSON MeterPDF
instance ToJSON ToPDF
instance FromJSON ToPDF               
                   
-- read the data
-- BL.readFile "out4.fit.jsonlite.json" >>= return . decode :: IO (Maybe [ToPDF])