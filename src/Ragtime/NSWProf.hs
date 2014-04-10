{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DeriveGeneric              #-}
-- This module could also be part of the IMA module because it hardly based on
-- any ZMidi.* code, but since it is only used in Ragtime research, I think
-- it better fits Ragtime.*
module Ragtime.NSWProf ( -- | Newtypes
                         NSWeight (..)
                       , NSWProf (..)
                       , SWProf (..)
                       , NrOfBars (..)
                         -- | NSW profile functions
                       , mergeProf
                       , mergeSWProf
                       , normSWProf
                       , normSWProfByBar
                         -- | Vector conversion and matching
                       , dist 
                       , distBestRot
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
import ZMidi.Score             hiding ( numerator, denominator )
import Data.List                      ( intercalate, minimumBy )
import Data.Function                  ( on )
import Data.Ratio                     ( numerator, denominator )
import qualified Data.Map.Strict as M ( map )
import Data.Map.Strict                ( Map, foldrWithKey, mapWithKey, mapAccum
                                      , unionWith, findWithDefault, toAscList )
import qualified Data.Vector     as V ( length, splitAt, null, (++) )
import Data.Vector                    ( Vector, generate )
import Data.Binary                    ( Binary, encodeFile, decodeFile )
import Text.Printf                    ( PrintfArg, printf )
import Ragtime.VectorNumerics         ( euclDist, disp )
import GHC.Generics

-- | Normalised spectral weights (value between 0 and 1)
newtype NSWeight = NSWeight { nsweight :: Double }
                     deriving ( Eq, Show, Num, Ord, Enum, Real, Floating
                              , Fractional, RealFloat, RealFrac, PrintfArg
                              , Binary, Generic )

-- | prints a 'NSWeight' as a sequence of asterisks 
-- stars :: (Show a, Num a) => a -> String
stars :: (Show a, RealFrac a) => a -> String
stars w = replicate (round (20 * w)) '*' 
-- stars w = show w

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
    
          shw :: (Beat, BeatRat) -> NSWeight -> [String] -> [String]
          shw (Beat b, BeatRat br) w r = 
            let x = w in printf ("%1d - %2d / %2d: %.5f " ++ stars x) 
                           b (numerator br) (denominator br) x : r

-- | Stores the number of bars
newtype NrOfBars = NrOfBars  { nrOfBars :: Int }
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral
                             , PrintfArg, Binary )

-- | Plots an 'SWProf'ile by calculating the average profile
showNSWProf :: (TimeSig, NSWProf) -> String
showNSWProf (ts, p) = show ts ++ "\n" ++ show p
  
mergeSWProf :: SWProf -> SWProf -> SWProf
mergeSWProf (SWProf a) (SWProf b) = SWProf (mergeProf a b)  
  
-- | merges two 'SWProf's by summing its values
mergeProf :: Num a => (NrOfBars, Map (Beat, BeatRat) a) 
                   -> (NrOfBars, Map (Beat, BeatRat) a)
                   -> (NrOfBars, Map (Beat, BeatRat) a)
mergeProf (a, ma) (b, mb) = let m = unionWith (+) ma mb in m `seq` (a + b, m)

-- Normalises an 'SWProf' to an 'NSWProf' (normalised SWProf), which will
-- only contain values between 0 and 1
normSWProf :: SWProf -> NSWProf 
normSWProf (SWProf (b, wp)) = 
  let -- m  = trace ("max: " ++ show a) a where a = fromIntegral . fst . mapAccum (\v w -> (max v w, w)) 0 $ wp
      m = fromIntegral . fst . mapAccum (\v w -> (max v w, w)) 0 $ wp
  in NSWProf (b, M.map (\x -> fromIntegral x / m) wp)

-- Normalises an 'SWProf' to an 'NSWProf' (normalised SWProf), by dividing
-- the spectral weight by the square of the number of bars and taking the log
normSWProfByBar :: SWProf -> NSWProf
normSWProfByBar (SWProf (nob, wp)) = 
  let d = fromIntegral (nob * nob)
      
      f :: SWeight -> NSWeight
      f 0 = -100
      f x = log (fromIntegral x / d)
      
  in NSWProf (nob, M.map f wp)

  
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
                      in euclDist x y + sumDistPerBar xs' ys'
                      -- in trace ("matching:\n" ++ disp x ++ "\n" ++disp y ++ show (euclDist x y ))
                               -- (euclDist x y + sumDistPerBar xs' ys')
                      
-- | Vectorizes a 'SWProf' for matching with 'dist'
vectorize :: QBins -> TimeSig ->  NSWProf -> Vector NSWeight
vectorize _  NoTimeSig _ = error "vectorize applied to NoTimeSig"
vectorize (QBins qb) ts@(TimeSig num _ _ _) p = generate (num * qb) getWeight 
  
  where -- Given the number of quantisation bins toKey maps an index to 
        -- a (Beat, Bar) tuple of a NSWProf. N.B. the each index maps to a
        -- (Beat, Bar) bin. Hence, the ticks per beat equals the number of 
        -- quantisation bins
        toKey :: Int -> (Beat, BeatRat)
        toKey i = case getBeatInBar ts (TPB qb) (Time i) of
                    (1, b, br) -> (b, br)
                    _  -> error ("index out of bounds: " ++ show i)
        
        getWeight :: Int -> NSWeight
        getWeight i = findWithDefault (NSWeight 0) (toKey i) m
        
        m :: Map (Beat, BeatRat) NSWeight
        -- m = normNSWProf p
        m = snd (nswprof p)

-- | Batch vectorizes a Map with 'SWProf's
vectorizeAll :: QBins -> Map TimeSig NSWProf -> [(TimeSig, Vector NSWeight)]
vectorizeAll qb = toAscList . mapWithKey (vectorize qb)

-- | Rotates a vector with /n/ steps
rotate :: Int -> Vector a -> Vector a
rotate n v = let (front, back) = V.splitAt n v in back V.++ front

-- | Finds the indices with a value greater than /x/
ixOfValGT :: forall a. (Ord a) => a -> Vector a -> [Int]
ixOfValGT x v = [0 .. pred (V.length v)] where
-- ixOfValGT x = ifoldr step [] where

  -- step :: Int -> a -> [Int] -> [Int]
  -- step i y r | y > x     = i : r
             -- | otherwise = r
         
distBestRot :: forall a. (PrintfArg a, Ord a, Show a, Floating a) 
            => QBins -> a -> Vector a -> Vector a -> (a, Int)
distBestRot b x v rot = minimumBy (compare `on` fst) $ distRotate b x v rot
         
-- | Matches two vectors by rotating the second vector moving all indices to the
-- first position that store a value greater then /x/, e.g.
-- 
-- >>> distRotate 10 0.0 (fromList [1.0 .. 10.0]) 
-- >>>                   (rotate 5 $ fromList [1.0 .. 10.0] :: Vector Double)
-- >>> [(15.811388300841896,0),(15.491933384829668,1),(14.491376746189438,2)
-- >>>  ,(12.649110640673518,3),(9.486832980505138,4),(0.0,5)
-- >>>  ,(9.486832980505138,6),(12.649110640673518,7),(14.491376746189438,8)
-- >>>  ,(15.491933384829668,9)]
--
distRotate :: forall a. (PrintfArg a, Ord a, Show a, Floating a) 
           => QBins -> a -> Vector a -> Vector a -> [(a, Int)]
distRotate b x v rot = map rDist . ixOfValGT x $ rot

  where rDist :: Int -> (a, Int)
        rDist i = (dist b v (rotate i rot), i)
             
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
  
