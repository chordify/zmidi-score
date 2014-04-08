{-# OPTIONS_GHC -Wall           #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
import Data.Aeson                           ( ToJSON (..), FromJSON (..), decode )
import GHC.Generics                         ( Generic )
-- import ZMidi.Score.Datatypes   (TimeSig (..))
import qualified Data.ByteString.Lazy as BL ( readFile )


data ToPDF = ToPDF { -- timeSig  :: String
                     mu        :: [Double]
                   , sigma     :: [[Double]]
                   , inv_sigma :: [[Double]]
                   , meter     :: (Int,Int)
                   } deriving (Show, Generic)

instance ToJSON ToPDF
instance FromJSON ToPDF               
                   
-- read the data
-- BL.readFile "out4.fit.jsonlite.json" >>= return . decode :: IO (Maybe [ToPDF])