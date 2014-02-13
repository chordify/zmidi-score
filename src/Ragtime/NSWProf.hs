{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- This module could also be part of the IMA module because it hardly based on
-- any ZMidi.* code, but since it is only used in Ragtime research, I think
-- it better fits Ragtime.*
module Ragtime.NSWProf ( -- | Newtypes
                         NSWeight (..)
                       , SWProf (..)
                       , NrOfBars (..)
                         -- | NSW profile functions
                       , mergeNSWProf
                       -- , normNSWProf
                         -- | Vector conversion and matching
                       , dist 
                       , vectorize  
                       , vectorizeAll  
                         -- | Printing
                       , showNSWProf
                       , stars
                       , disp
                         -- | Serialization
                       , writeNSWProf  
                       , readNSWProf 
                       )where

import IMA.InnerMetricalAnalysis      ( SWeight )
import ZMidi.Score             hiding (numerator, denominator)
import Data.List                      ( intercalate )
import Data.Ratio                     ( numerator, denominator )
import qualified Data.Map.Strict as M ( map )
import Data.Map.Strict                ( Map, foldrWithKey, mapWithKey, mapAccum
                                      , unionWith, findWithDefault, toAscList )
import qualified Data.Vector     as V ( length, splitAt, null )
import Data.Vector                    ( Vector, generate )
import Data.Binary                    ( Binary, encodeFile, decodeFile )
import Text.Printf                    ( PrintfArg, printf )
import Ragtime.VectorNumerics         ( euclDist, disp )

import Debug.Trace

-- | Normalised spectral weights (value between 0 and 1)
newtype NSWeight = NSWeight { nsweight :: Double }
                     deriving ( Eq, Show, Num, Ord, Enum, Real, Floating
                              , Fractional, RealFloat, RealFrac, PrintfArg
                              , Binary )

-- | prints a 'NSWeight' as a sequence of asterisks 
stars :: (Show a, Num a) => a -> String
-- stars w = replicate (round (20 * w)) '*' 
stars w = show w

--------------------------------------------------------------------------------
-- IMA profiles
--------------------------------------------------------------------------------

-- | Normalised Spectral Weight Profiles
newtype SWProf = SWProf {nswprof :: (NrOfBars, Map (Beat, BeatRat) SWeight)}
                    deriving ( Eq, Binary )

instance Show SWProf where
  show n@(SWProf (bars, m)) = intercalate "\n" (hdr : foldrWithKey shw [] m')
  
    where m'  = normSWProf n
          hdr = "Bars: " ++ show (nrOfBars bars)
    
          shw :: (Beat, BeatRat) -> NSWeight -> [String] -> [String]
          shw (Beat b, BeatRat br) w r = 
            -- TODO replace the code below by nromNSWProf
            let x = w / fromIntegral bars
            in (printf ("%1d - %2d / %2d: %.5f " ++ stars x) 
                       b (numerator br) (denominator br) x ) : r

-- | Stores the number of bars
newtype NrOfBars = NrOfBars  { nrOfBars :: Int }
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral
                             , PrintfArg, Binary )

-- | Plots an 'SWProf'ile by calculating the average profile
showNSWProf :: (TimeSig, SWProf) -> String
showNSWProf (ts, p) = show ts ++ "\n" ++ show p
  
-- | merges two 'SWProf's by summing its values
mergeNSWProf :: SWProf -> SWProf -> SWProf
mergeNSWProf (SWProf (a, ma)) (SWProf (b, mb)) = 
  let m = unionWith (+) ma mb in m `seq` (SWProf (a + b, m))

-- TODO create newtype around Map BeatRat NSWeight and create a datatype
-- cumNSWProf for the current SWProf
-- normNSWProf :: SWProf -> Map (Beat, BeatRat) NSWeight
-- normNSWProf (SWProf (b, wp)) = let b' = fromIntegral b in M.map (\x -> x / b') wp


normSWProf :: SWProf -> Map (Beat, BeatRat) NSWeight
normSWProf (SWProf (b, wp)) = let b' = fromIntegral b 
                                  -- m  = fst (mapAccum (\v w -> (max v w, w)) 0 wp)
                               in M.map (\x -> fromIntegral x / (b' )) wp

--------------------------------------------------------------------------------
-- Matching IMA profiles
--------------------------------------------------------------------------------

-- | Matches to 'Vectors' at every bar (i.e. every /x/ 'GridUnit's)
dist :: (Show a, Floating a, PrintfArg a) => QBins -> Vector a -> Vector a -> a
dist (QBins qb) a b 
  | la /= lb     = error "dist: comparing Vectors of different lengths"
  | laModGu /= 0 = error "dist: incompatible Vector length" 
  | otherwise    = sumDistPerBar a b / fromIntegral nrBars where
  
      la = V.length a
      lb = V.length b
      (nrBars, laModGu) = la `divMod ` qb
  
      sumDistPerBar :: (Show a, Floating a, PrintfArg a) => Vector a -> Vector a -> a
      sumDistPerBar xs ys
        | V.null xs = 0 -- we check whether both Vectors are equally long above
        | otherwise = let (x,xs') = V.splitAt qb xs
                          (y,ys') = V.splitAt qb ys
                      in trace ("matching:\n" ++ disp x ++ "\n" ++disp y ++ show (euclDist x y ))
                               (euclDist x y + sumDistPerBar xs' ys')
                      
-- | Vectorizes a 'SWProf' for matching with 'dist'
vectorize :: QBins -> TimeSig ->  SWProf -> Vector NSWeight
vectorize _  NoTimeSig _ = error "toNSWVec applied to NoTimeSig"
vectorize (QBins qb) ts@(TimeSig num _ _ _) p = generate (num * qb) getWeight 
  
  where toKey :: Int -> (Beat, BeatRat)
        toKey i = case getBeatInBar ts (Time qb) (Time i) of
                    (1, b, br) -> (b, br)
                    _  -> error ("index out of bounds: " ++ show i)
        
        getWeight :: Int -> NSWeight
        getWeight i = findWithDefault (NSWeight 0) (toKey i) m
        
        m :: Map (Beat, BeatRat) NSWeight
        m = normSWProf p

-- | Batch vectorizes a Map with 'SWProf's
vectorizeAll :: QBins -> Map TimeSig SWProf -> [(TimeSig, Vector NSWeight)]
vectorizeAll qb = toAscList . mapWithKey (vectorize qb)

-- showNSWVec :: (TimeSig, Vector NSWeight) -> String
-- showNSWVec (ts, v) = show ts ++ ':' : (concatMap (printf " %.2f") . toList $ v)

--------------------------------------------------------------------------------
-- exporting / importing IMA profiles
--------------------------------------------------------------------------------

-- exports a normalised inner metric analysis profiles to a binary file
writeNSWProf :: FilePath -> Map TimeSig SWProf -> IO (Map TimeSig SWProf)
writeNSWProf fp m = encodeFile fp m >> putStrLn ("written: " ++ fp) >> return m
  
readNSWProf :: FilePath -> IO (Map TimeSig SWProf)
readNSWProf fp = putStrLn ("read: " ++ fp) >> decodeFile fp  
  
