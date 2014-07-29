{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DeriveGeneric              #-}

module ZMidi.IMA.NSWProf ( -- | Newtypes
                           NSWeight (..)
                         , NSWProf (..)
                         , SWProf (..)
                         , NrOfBars (..)
                           -- | NSW profile functions
                         , normSWProfByBar
                           -- | Printing
                         , showNSWProf
                         , stars
                           -- | Serialization
                         , writeNSWProf  
                         , readNSWProf 
                         )where

import IMA.InnerMetricalAnalysis      ( SWeight )
import ZMidi.Score
import ZMidi.IMA.Internal  

import Data.List                      ( intercalate )
import Data.Ratio                     ( numerator, denominator )
import qualified Data.Map.Strict as M ( map )
import Data.Map.Strict                ( Map, foldrWithKey, mapAccum )
import Data.Binary                    ( Binary, encodeFile, decodeFile )
import Text.Printf                    ( PrintfArg, printf )
import GHC.Generics

-- | Normalised spectral weights (value between 0 and 1)
newtype NSWeight = NSWeight { nsweight :: Double }
                     deriving ( Eq, Show, Num, Ord, Enum, Real, Floating
                              , Fractional, RealFloat, RealFrac, PrintfArg
                              , Binary, Generic )

--------------------------------------------------------------------------------
-- IMA profiles
--------------------------------------------------------------------------------

-- | Normalised Spectral Weight Profiles
newtype SWProf = SWProf {swprof :: (NrOfBars, Map (Beat, BeatRat) SWeight)}
                    deriving ( Eq, Binary, Show )


-- | Normalised Spectral Weight Profiles
newtype NSWProf = NSWProf {nswprof :: (NrOfBars, Map (Beat, BeatRat) NSWeight)}
                    deriving ( Eq, Binary )
                    
instance Show NSWProf where
  show (NSWProf (bars, m)) = intercalate "\n" (hdr : foldrWithKey shw [] m)
  
    where hdr = "Bars: " ++ show (nrOfBars bars)
          mx  = maxVal 0 m 
    
          shw :: (Beat, BeatRat) -> NSWeight -> [String] -> [String]
          shw (Beat b, BeatRat br) w r = 
            let x = w in printf ("%1d - %2d / %2d: %.5f " ++ stars (x / mx)) 
                           b (numerator br) (denominator br) x : r

-- | Stores the number of bars
newtype NrOfBars = NrOfBars  { nrOfBars :: Int }
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral
                             , PrintfArg, Binary )

-- | Plots an 'SWProf'ile by calculating the average profile
showNSWProf :: (TimeSig, NSWProf) -> String
showNSWProf (ts, p) = show ts ++ "\n" ++ show p

toNSWProfs :: TPB -> Map TimeSig [(Beat, BeatRat)] -> [Timed (Maybe ScoreEvent, SWeight)] 
           -> Map TimeSig NSWProf
toNSWProfs sel d = undefined

-- Normalises an 'SWProf' to an 'NSWProf' (normalised SWProf), by dividing
-- the spectral weight by the square of the number of bars and taking the log
normSWProfByBar :: SWProf -> NSWProf
normSWProfByBar (SWProf (nob, wp)) = 
  let -- Take the square of the bar length
      d = fromIntegral (nob * nob)
      
      -- Apply Laplacian / additive smoothing to prevent bins with 0
      -- see: http://en.wikipedia.org/wiki/Additive_smoothing
      alpha = 1 :: NSWeight 
      
      f :: SWeight -> NSWeight
      -- f 0 = log  alpha
      f x = log ((fromIntegral x / d) + alpha)
      
  in NSWProf (nob, M.map f wp)

-- | returns the maximum value in a 'Map'. The first argument is a first value
-- that is compared.
maxVal :: Ord v => v -> Map k v -> v
maxVal s = fst . mapAccum (\v m -> (max v m, m)) s

--------------------------------------------------------------------------------
-- exporting / importing IMA profiles
--------------------------------------------------------------------------------

-- exports a normalised inner metric analysis profiles to a binary file
writeNSWProf :: FilePath -> Map TimeSig NSWProf -> IO (Map TimeSig NSWProf)
writeNSWProf fp m = encodeFile fp m >> putStrLn ("written: " ++ fp) >> return m
  
readNSWProf :: FilePath -> IO (Map TimeSig NSWProf)
readNSWProf fp = putStrLn ("read: " ++ fp) >> decodeFile fp  
  
