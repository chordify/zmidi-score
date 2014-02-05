{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- This module could also be part of the IMA module because it hardly based on
-- any ZMidi.* code, but since it is only used in Ragtime research, I think
-- it better fits Ragtime.*
module Ragtime.NSWProf where

import ZMidi.Score             hiding (numerator, denominator)
import Ragtime.VectorNumerics     
import Data.List                      ( intercalate )
import Data.Ratio                     ( numerator, denominator, (%) )
import qualified Data.Map.Strict as M ( map )
import Data.Map.Strict                ( Map, foldrWithKey
                                      , unionWith, findWithDefault, toAscList )
import Data.Vector                    ( Vector, generate, fromList )
import Data.Binary                    ( Binary, encodeFile, decodeFile )
import Text.Printf                    ( PrintfArg, printf )
import Control.Arrow                  ( (***) )


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
type NSWProf     = (NrOfBars, Map (Beat, BeatRat) NSWeight)

-- | Stores the number of bars
newtype NrOfBars = NrOfBars  { nrOfBars :: Int }
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral
                             , PrintfArg, Binary )

-- | Plots an 'NSWProf'ile by calculating the average profile
showNSWProf :: (TimeSig, NSWProf) -> String
showNSWProf (ts, (bars, m)) = intercalate "\n" (show ts : foldrWithKey shw [] m)

  where shw :: (Beat, BeatRat) -> NSWeight -> [String] -> [String]
        shw (Beat b, BeatRat br) w r = 
          let x = w / fromIntegral bars
          in (printf ("%1d - %2d / %2d: %.5f " ++ stars x) 
                     b (numerator br) (denominator br) x ) : r
  
-- | merges two 'NSWProf's by summing its values
mergeNSWProf :: NSWProf -> NSWProf -> NSWProf
mergeNSWProf (a, ma) (b, mb) = let m = unionWith (+) ma mb in m `seq` (a + b, m)

-- TODO create newtype around Map BeatRat NSWeight and create a datatype
-- cumNSWProf for the current NSWProf
normNSWProf :: NSWProf -> Map (Beat, BeatRat) NSWeight
normNSWProf (b, wp) = let b' = fromIntegral b in M.map (\x -> x / b') wp


--------------------------------------------------------------------------------
-- Matching IMA profiles
--------------------------------------------------------------------------------

-- TODO add check for Vectors of different lengths
euclDist :: Vector NSWeight -> Vector NSWeight -> Double
euclDist a b = nsweight . normL2 $ (a - b)

toNSWVec :: TimeSig -> GridUnit -> NSWProf -> Vector NSWeight
toNSWVec (TimeSig num _ _ _) gu p = generate (num * gridUnit gu) getWeight where
  
  getWeight :: Int -> NSWeight
  getWeight i = findWithDefault (NSWeight 0) (toKey i) m
  
  toKey :: Int -> (Beat, BeatRat)
  toKey i = (Beat *** toBeatRat gu) $ (i `divMod` num)
  
  m :: Map (Beat, BeatRat) NSWeight
  m = normNSWProf p


getBinIDs :: GridUnit -> [BeatRat]
getBinIDs gu = map (toBeatRat gu) [0 .. pred (gridUnit gu)]

toBeatRat :: GridUnit -> Int -> BeatRat 
toBeatRat (GridUnit gu) i = BeatRat (i % gu)

toIx :: GridUnit -> BeatRat -> Int 
toIx (GridUnit gu) (BeatRat br) = numerator (br * (gu % 1))
  
toNSWVecSeg :: TimeSig -> GridUnit -> Map TimeSig NSWProf -> [(TimeSig, Vector NSWeight)]
toNSWVecSeg ts gu = toAscList . M.map (toNSWVec ts gu)

-- showNSWVec :: (TimeSig, Vector NSWeight) -> String
-- showNSWVec (ts, v) = show ts ++ ':' : (concatMap (printf " %.2f") . toList $ v)

--------------------------------------------------------------------------------
-- exporting / importing IMA profiles
--------------------------------------------------------------------------------

-- exports a normalised inner metric analysis profiles to a binary file
writeNSWProf :: FilePath -> Map TimeSig NSWProf -> IO (Map TimeSig NSWProf)
writeNSWProf fp m = encodeFile fp m >> putStrLn ("written: " ++ fp) >> return m
  
readNSWProf :: FilePath -> IO (Map TimeSig NSWProf)
readNSWProf fp = putStrLn ("read: " ++ fp) >> decodeFile fp  
  