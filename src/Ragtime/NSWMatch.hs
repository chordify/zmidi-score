{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ragtime.NSWMatch( PMatch (..)
                       , NSWDist (..)
                       , Prob (..)
                       , pickMeters
                       , printPickMeter
                       ) where
                       
import ZMidi.Score 
import Ragtime.TimeSigSeg         ( TimedSeg (..) )
import ZMidi.IMA.SelectProfBins   ( Rot (..) )

import Data.List                  ( intercalate, maximumBy )
import Data.Function              ( on )
import Text.Printf                ( printf, PrintfArg )

-- | Normalised spectral weights distance, obtained by matching two 'SWProf's
newtype NSWDist = NSWDist { nswdist :: Double }
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Floating
                             , Fractional, RealFloat, RealFrac, PrintfArg )

newtype Prob    = Prob { prob :: Double }
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Floating
                             , Fractional, RealFloat, RealFrac, PrintfArg )
                             
data PMatch = PMatch {  pmTimeSig :: TimeSig
                     ,  pmatch    :: NSWDist
                     ,  rotation  :: Rot
                     } deriving (Eq)
                     
instance Show PMatch where
  show (PMatch ts m r) = printf (show ts ++ ": %1.4f\t R: %2d") m (rot r)
  showList l s = s ++ (intercalate "\n" . map show $ l)
  
-- | Picks the best matching profile
pickMeters :: [TimedSeg TimeSig [PMatch]] 
           -> Either String [TimedSeg TimeSig PMatch]
pickMeters = Right . map (fmap (maximumBy (compare `on` pmatch)))

printPickMeter :: TimedSeg TimeSig PMatch -> String
printPickMeter (TimedSeg ts m) = 
  let ann = getEvent ts
      est = pmTimeSig m
      s = intercalate "\t" [ shwTs ann
                           , shwTs est
                           , show (ann == est)
                           , "%.3f" 
                           , "r:%2d"]
      
      shwTs :: TimeSig -> String
      shwTs x = '\'' : show x ++ "\'"
      
      -- tsEq :: TimeSig -> TimeSig -> Bool
      -- tsEq (TimeSig 4 4 _ _) (TimeSig 2 2 _ _) = True
      -- tsEq a                 b                 = a == b
  
  in printf s (pmatch m) (rot . rotation $ m)
