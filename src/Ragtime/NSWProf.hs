{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- This module could also be part of the IMA module because it hardly based on
-- any ZMidi.* code, but since it is only used in Ragtime research, I think
-- it better fits Ragtime.*
module Ragtime.NSWProf where

import ZMidi.Score                    ( TimeSig, BarRat (..), Timed (..)
                                      , GridUnit(..) )
import Ragtime.TimeSigSeg             ( TimedSeg (..) )
import Ragtime.VectorNumerics     
import Data.List                      ( intercalate )
import Data.Ratio                     ( numerator, denominator, (%) )
import qualified Data.Map.Strict as M ( map )
import Data.Map.Strict                ( Map, insertWith, foldrWithKey
                                      , unionWith, findWithDefault  )
import Data.Vector                    ( Vector, generate )
import Data.Binary                    ( Binary, encodeFile )
import Text.Printf                    ( PrintfArg, printf )


-- | Normalised spectral weights (value between 0 and 1)
newtype NSWeight = NSWeight { nsweight :: Double }
                     deriving ( Eq, Show, Num, Ord, Enum, Real, Floating
                              , Fractional, RealFloat, RealFrac, PrintfArg
                              , Binary )


stars :: NSWeight -> String
stars w = replicate (round (20 * w)) '*' 

--------------------------------------------------------------------------------
-- IMA profiles
--------------------------------------------------------------------------------

-- | Normalised Spectral Weight Profiles
type NSWProfSeg  = TimedSeg TimeSig NSWProf
type NSWProf     = (NrOfBars, Map BarRat NSWeight)

-- | Stores the number of bars
newtype NrOfBars = NrOfBars  { nrOfBars :: Int }
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral
                             , PrintfArg, Binary )


-- | Plots an 'NSWProf'ile by calculating the average profile
showNSWProf :: (TimeSig, NSWProf) -> String
showNSWProf (ts, (bars, m)) = intercalate "\n" ( show ts : foldrWithKey shw [] m )

  where shw :: BarRat -> NSWeight -> [String] -> [String]
        shw (BarRat br) w r = let x = w / fromIntegral bars
                              in (printf ("%2d / %2d: %.5f " ++ stars x) 
                                 (numerator br) (denominator br) x   ) : r

-- | Collects all profiles sorted by time signature in one map
collectNSWProf :: [NSWProfSeg] -> Map TimeSig NSWProf -> Map TimeSig NSWProf
collectNSWProf s m = foldr doSeg m s where

  doSeg :: NSWProfSeg -> Map TimeSig NSWProf -> Map TimeSig NSWProf
  doSeg (TimedSeg ts p) m' = insertWith mergeNSWProf (getEvent ts) p m'
  
-- | merges two 'NSWProf's by summing its values
mergeNSWProf :: NSWProf -> NSWProf -> NSWProf
mergeNSWProf (a, ma) (b, mb) = let m = unionWith (+) ma mb in m `seq` (a + b, m)

-- TODO create newtype around Map BarRat NSWeight and create a datatype
-- cumNSWProf for the current NSWProf
normNSWProf :: NSWProf -> Map BarRat NSWeight
normNSWProf (b, wp) = let b' = fromIntegral b in M.map (\x -> x / b') wp


--------------------------------------------------------------------------------
-- Matching IMA profiles
--------------------------------------------------------------------------------

euclDist :: Vector NSWeight -> Vector NSWeight -> Double
euclDist a b = nsweight . normL2 $ (a - b)

nSWProfToVec :: GridUnit -> Map BarRat NSWeight -> Vector NSWeight
nSWProfToVec gu m = generate (gridUnit gu) getWeight where
  
  getWeight :: Int -> NSWeight
  getWeight i = findWithDefault (NSWeight 0) (toBarRat gu i) m


getBinIDs :: GridUnit -> [BarRat]
getBinIDs gu = map (toBarRat gu) [0 .. pred (gridUnit gu)]

toBarRat :: GridUnit -> Int -> BarRat 
toBarRat (GridUnit gu) i = BarRat (i % gu)

toIx :: GridUnit -> BarRat -> Int 
toIx (GridUnit gu) (BarRat br) = numerator (br * (gu % 1))
  

--------------------------------------------------------------------------------
-- exporting / importing IMA profiles
--------------------------------------------------------------------------------

-- exports a normalised inner metric analysis profiles to a binary file
safeNSWProf :: FilePath -> Map TimeSig NSWProf -> IO (Map TimeSig NSWProf)
safeNSWProf fp m = encodeFile fp m >> putStrLn ("written: " ++ fp) >> return m
  
