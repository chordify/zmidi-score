{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  IMA.InnerMetricalAnalysis
-- Copyright   :  (c) 2012--2014 Utrecht University
-- License     :  LGPL-3
--
-- Maintainer  :  W. Bas de Haas <bash@cs.uu.nl>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: implements the Inner Metrical Analysis model 
-- 
-- see: Volk, Anja. "The study of syncopation using inner metric analysis: 
-- Linking theoretical and experimental analysis of metre in music." Journal of 
-- New Music Research 37.4 (2008): 259-273. 
-- http://dx.doi.org/10.1080/09298210802680758
--------------------------------------------------------------------------------
module IMA.InnerMetricalAnalysis ( -- * Types for Local Meters
                               Time    (..)
                             , Period  (..)
                             , Len     (..)
                             , MWeight (..)
                             , SWeight (..)
                             , showMeterMap
                             -- * The Inner Metrical Analysis
                             , getLocalMeters
                             , getMetricWeight
                             , getSpectralWeight
                             -- * Utilities
                             , maxPeriod
                             , addMaxPerCheck
                             ) where

import Data.List                  ( foldl' )
import Data.IntMap                ( empty, IntMap, insert, toAscList, elems
                                  , foldrWithKey, insertWith, split
                                  , filterWithKey, mapKeysMonotonic, toList )
import qualified Data.IntMap as M ( lookup, null, map )
import Data.Vector                ( Vector, freeze, thaw, unsafeIndex )
import qualified Data.Vector as V ( fromList, toList, length, empty, replicate )
import Data.Vector.Mutable        ( MVector )
import Data.Vector.Mutable   as MV ( unsafeRead, unsafeWrite )
import Data.Foldable              ( foldrM )
import Control.Monad.ST
import Control.Monad.Primitive

import IMA.LocalMeter

--------------------------------------------------------------------------------
-- MeterMap and printing it
--------------------------------------------------------------------------------

type MeterMap = IntMap (IntMap Len)

showMeterMap :: MeterMap -> String
showMeterMap = concatMap showPer . toAscList 

showPer :: (Int, OnsetMap) -> String
showPer (p, l) =  "Period: " ++ show p 
               ++ concatMap (showMeter p) (toAscList l) ++ "\n"

showMeter :: Int -> (Int, Len) -> String
showMeter p (t, Len l) = 
  " (onset="++ show t++ " per=" ++ show p ++ " len="++ show l ++ ")"


--------------------------------------------------------------------------------
-- Local Meters
--------------------------------------------------------------------------------

-- | Inner Metric Analysis defines the pulses of a piece of music solely on 
-- the base of note onsets. The model considers all the pulses, called 
-- local metres, that can overlay with each other with very different periods 
-- and shifted at all phases.
getLocalMeters :: Period -> Period -> [Time] -> MeterMap
getLocalMeters _  _   []  = empty
getLocalMeters ml mxP ons = scaleMeterMap ml 
                          $ foldl' onePeriod empty [1, 2 .. (mxP `div` ml)] where
   
   -- normalise the onset times by dividing them by the minimum length
   ons' = divideByMinLength ml ons 
   -- pre-process onsets for fast lookup
   v    = V.fromList $ onsetGrid [0 .. last ons'] ons'
   
   onePeriod :: MeterMap -> Period -> MeterMap
   onePeriod m p = insertMeters (factors p) m p . foldl' oneMeter empty $ ons' 

     where oneMeter :: OnsetMap -> Time -> OnsetMap
           oneMeter l (Time t) 
             | (len x) >= 2 && isMax p l p t x = insert t x l
             | otherwise                       =            l
                 where x = getLength v p (Time t)

divideByMinLength :: Period -> [Time] -> [Time]
divideByMinLength (Period l) = map divErr  where

  divErr :: Time -> Time
  divErr (Time t) = case t `divMod` l of
                      (t',0) -> Time t'
                      _      -> error ("cannot normalise onset: " ++ show t ++
                                       " cannot be divided by "   ++ show l)
     
scaleMeterMap :: Period -> MeterMap -> MeterMap
scaleMeterMap (Period p) = f . M.map f where f = mapKeysMonotonic (* p)
     
-- | Creates a grid of 'Bool's where 'True' represents an onset and 'False'
-- no onset
--
-- >>> onsetGrid [0..10] [2,3,6,7]
-- >>> [False,False,True,True,False,False,True,True,False,False,False]
onsetGrid :: [Time] -> [Time] -> [Bool]
onsetGrid []     []     = []
onsetGrid []     os     = error ("onsetGrid: grid to small for onsets" ++ show os)
onsetGrid gs     []     = replicate (length gs) False
onsetGrid (g:gs) (o:os) | g == o = True  : onsetGrid gs    os
                        | g <  o = False : onsetGrid gs (o:os)
                        | o >  g = error "onsetGrid: non-monotone onsets"
onsetGrid _      _     = error "onsetGrid: non-monotone onsets"


-- | Calculates the length of a local meter by projecting it forward. The
-- first argument is a vector of 'Bool's that represents the metrical grid,
-- where True represents an onset
getLength :: Vector Bool -> Period -> Time -> Len
getLength v (Period p) o = pred $ project o where

  -- cache the length of the vector
  lv = V.length v

  project :: Time -> Len
  {-# INLINE project #-}
  project (Time t) | t < lv && v `unsafeIndex` t = succ (project (Time (t + p)))
                   | otherwise                   = 0

insertMeters :: [Period] -> MeterMap -> Period -> OnsetMap -> MeterMap
insertMeters fs mm p om 
  | M.null om' = mm
  | otherwise  = insert (period p) om' mm
      where om' = filterWithKey (isMaximal mm) om

            isMaximal :: MeterMap -> Int -> Len -> Bool
            isMaximal m t l = foldr isMaxInMeterMap True fs where

              isMaxInMeterMap :: Period -> Bool -> Bool
              {-# INLINE isMaxInMeterMap #-}
              isMaxInMeterMap f r = let y = period f
                                    in case M.lookup y m of
                                       Just x -> r && isMax f x p t l 
                                       _      -> r

isMax :: Period -> OnsetMap -> Period -> Int -> Len -> Bool
{-# INLINE isMax #-}
isMax (Period f) m (Period pb) tb (Len lb) = 
  foldrWithKey noSubSet True (subMap m tb) -- select all meters that start earlier

    where noSubSet :: Int -> Len -> Bool -> Bool
          {-# INLINE noSubSet #-}
          noSubSet ta (Len la) r = 
             r && (  ta `mod` f    /= tb `mod` f       -- not in phase
                  || ta + (la * f)  < tb + (lb * pb) ) -- ends later

          subMap :: OnsetMap -> Int -> OnsetMap
          {-# INLINE subMap #-}
          subMap om t = fst $ split (succ t) om

--------------------------------------------------------------------------------
-- Inner Metrical Analysis Weights
--------------------------------------------------------------------------------

newtype SWeight = SWeight {sweight :: Int} 
                  deriving ( Eq, Show, Num, Ord, Enum, Real, Integral )
newtype MWeight = MWeight {mweight :: Int}
                  deriving ( Eq, Show, Num, Ord, Enum, Real, Integral )
 
type MWeightMap = IntMap MWeight
-- type SWeightMap = IntMap SWeight
type OnsetMap   = IntMap Len


-- | Based on the detection of all local metres in a given piece a metric weight 
-- for each onset is defined that reflects the amount of local metres that 
-- coincide at this onset. Hence, the basic idea is similar. Onsets where many 
-- pulses coincide get a greater weight than onsets where fewer pulses coincide. 
-- Moreover, the intuition modelled in the metric weight is that longer 
-- repetitions should contribute more weight than shorter ones.
getMetricWeight :: Period -> [Time] -> [MWeight]
getMetricWeight p ons = elems . getMetricMap p (maxPeriod ons) $ ons

getMetricMap :: Period -> Period -> [Time] -> MWeightMap
getMetricMap ml mP ons = foldrWithKey onePeriod initMap 
                       $ getLocalMeters ml mP ons where
   
   initMap :: MWeightMap 
   initMap = foldr (\o m -> insertWith (+) o (MWeight 0) m) empty (map time ons)
   
   onePeriod :: Int -> OnsetMap -> MWeightMap -> MWeightMap
   onePeriod p om wm = foldrWithKey oneMeter wm om where
     
     oneMeter :: Int -> Len -> MWeightMap -> MWeightMap
     oneMeter t (Len l) m' = foldr addWeight m' [t, t+p .. t + (l*p)] where
       
       w = MWeight (l ^ (2 :: Int))
       
       addWeight :: Int -> MWeightMap -> MWeightMap
       addWeight o m'' = insertWith (+) o w m''

-- | The spectral weight is based on the extension of each local metre throughout 
-- the entire piece.
getSpectralWeight :: Period -> [Time] -> [(Int, SWeight)]
getSpectralWeight _ []  = []
getSpectralWeight p os = 
  let grd = createGrid p os
  in zip grd . V.toList . getSpectralVec p (maxPeriod os) os $ grd
  
          
-- | Creates a grid that to align the spectral weights to:
-- 
-- >>> createGrid 2 [2,6,8,12,16]
-- >>> [2,4,6,8,10,12,14,16]
--
createGrid :: Period -> [Time] -> [Int] 
createGrid _          []     = []
createGrid (Period p) os = [time (head os), (p + time (head os)) .. time (last os)]

maxPeriod :: [Time] -> Period
maxPeriod [] = error "maxPeriod: empty list"
maxPeriod ts = Period ((time . last $ ts) `div` 2)

-- | Tests whether 'maxPeriod' is not more than 1.5 times the median or
-- less then 05 times the median of the onsets. This function can be used
-- to flag outliers as they can bias the calculation of IMA. After all, one 
-- very late onset, can bias the maximal period and trigger quite a lot of
-- (useless) computation
maxPeriodTest :: [Time] -> Bool
maxPeriodTest [] = error "maxPeriodTest: empty list"
maxPeriodTest ts = let md = fromIntegral (ts !! (length ts `div` 2)) :: Float
                       mx = fromIntegral (maxPeriod ts)              :: Float
                   in (1.5 * mx) > md && (0.5 * mx) < md

addMaxPerCheck :: ([Time] -> a) -> [Time] -> Either String a
addMaxPerCheck f ts | maxPeriodTest ts = Right (f ts)
                    | otherwise        = Left "spurious onsets found"
                   
type MVecSW m = MVector (PrimState m) SWeight 

getSpectralVec :: Period -> Period -> [Time] -> [Int] -> Vector SWeight
getSpectralVec _  _  _   []   = V.empty
getSpectralVec ml mP ons grid = runST $ (initVec >>= process >>= freeze) where

   -- the main fold that processes all meters and calculates all weights
   process :: (PrimMonad m) => MVecSW m -> m (MVecSW m)
   process v = foldrM onePeriod v . toList . getLocalMeters ml mP $ ons
   
   -- Initialise a mutable Vector based on the grid positions
   initVec :: (PrimMonad m) => m (MVecSW m)
   initVec = thaw . V.replicate (succ . length $ grid) . SWeight $ 0
  
   start = head grid 
   end   = last grid  
  
   -- an indexing function mapping a grid position to its index in the MVector
   toGridIx :: Int -> Int
   {-# INLINE toGridIx #-}
   toGridIx t = (t - start) `div` (period ml)
   
   -- calculate the spectral onsets that belong to a local meter
   spectralGrid :: Int -> Int -> [Int]
   spectralGrid ms p = let rm = ms `mod` p
                       in dropWhile (< start) [ rm, (rm + p) .. end ]
   
   onePeriod :: (PrimMonad m) => (Int, OnsetMap) -> MVecSW m -> m (MVecSW m)
   onePeriod (p, om) wm = foldrM oneMeter wm (toList om) where
     
     oneMeter :: (PrimMonad m) => (Int, Len) -> MVecSW m -> m (MVecSW m)
     oneMeter (t, Len l) mv = foldrM addWeight mv $ spectralGrid t p where
       
       -- addWeigth is executed very often, hence we pre-compute some values
       w  = SWeight (l ^ (2 :: Int))
     
       -- adds the spectral onsets to the weight vector
       addWeight :: (PrimMonad m) => Int -> MVecSW m -> m (MVecSW m)
       {-# INLINE addWeight #-}
       addWeight i mv' = do curW <- MV.unsafeRead mv' (toGridIx i) --no bounds checking
                            MV.unsafeWrite mv' (toGridIx i) (curW + w)
                            return mv'
