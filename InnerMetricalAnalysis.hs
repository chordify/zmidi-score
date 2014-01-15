{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  InnerMetricalAnalysis
-- Copyright   :  (c) 2012--2013 Utrecht University
-- License     :  LGPL-3
--
-- Maintainer  :  W. Bas de Haas <bash@cs.uu.nl>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: implements the Inner Metrical Analysis model 
-- see: 
--------------------------------------------------------------------------------
module InnerMetricalAnalysis ( -- * Types for Local Meters
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
                             ) where

import Data.List                  ( foldl' )
import Data.IntMap                ( empty, IntMap, insert, toAscList, elems
                                  , foldrWithKey, insertWith, split, assocs
                                  , filterWithKey, mapKeysMonotonic, toList )
import qualified Data.IntMap as M ( lookup, null, map )
import Data.Vector                ( Vector, (!), generate, freeze, thaw )
import qualified Data.Vector as V ( fromList, length, empty, replicate )
import Data.Vector.Mutable        ( MVector )
import Data.Vector.Mutable   as MV ( read, write )
import Data.Foldable              ( foldrM )
import Control.Monad.ST
import Control.Monad.Primitive
import LocalMeter

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
   -- preprocess onsets for fast lookup
   v    = V.fromList $ onsetGrid [0 .. last ons'] ons'
   
   onePeriod :: MeterMap -> Period -> MeterMap
   onePeriod m p = insertMeters facts m  p . foldl oneMeter empty $ ons' where
   
     facts = factors p
     
     -- oneMeter :: [(Time, Len)] -> Time -> [(Time, Len)]     
     oneMeter :: OnsetMap -> Time -> OnsetMap
     oneMeter l t = addLMeter l p t (getLength v p t)

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

  project :: Time -> Len
  {-# INLINE project #-}
  project (Time t) | t < V.length v && v ! t = 1 + project (Time (t + p))
                   | otherwise               = 0

                   
addLMeter :: OnsetMap -> Period -> Time -> Len -> OnsetMap
addLMeter m p t l 
  | (len l) >= 2 && isMax p m p (time t) l = insert (time t) l m
  | otherwise                              =                   m

insertMeters :: [Period] -> MeterMap -> Period -> OnsetMap -> MeterMap
insertMeters fs mm p om 
  | M.null om' = mm
  | otherwise  = insert (period p) om' mm
      where om' = filterWithKey (isMaximal fs mm p) om

isMaximal :: [Period] -> MeterMap -> Period -> Int -> Len -> Bool
isMaximal fs m p t l = foldr isMaxInMeterMap True fs where

  isMaxInMeterMap :: Period -> Bool -> Bool
  isMaxInMeterMap f r = case M.lookup (period f) m of
                           Nothing -> r
                           Just om -> isMax f om p t l && r

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
subMap m t = fst $ split (succ t) m

--------------------------------------------------------------------------------
-- Inner Metrical Analysis Weights
--------------------------------------------------------------------------------

newtype SWeight = SWeight {sweight :: Int} 
                  deriving ( Eq, Show, Num, Ord, Enum, Real, Integral )
newtype MWeight = MWeight {mweight :: Int}
                  deriving ( Eq, Show, Num, Ord, Enum, Real, Integral )
 
type MWeightMap = IntMap MWeight
type SWeightMap = IntMap SWeight
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
getSpectralWeight p os = assocs $ getSpectralMap p (maxPeriod os) os (createGrid p os)
          
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

getSpectralMap :: Period -> Period -> [Time] -> [Int] -> SWeightMap
getSpectralMap _  _  _   []   = empty
getSpectralMap ml mP ons grid = foldrWithKey onePeriod initMap 
                              $ getLocalMeters ml mP ons where
   
   initMap :: SWeightMap 
   initMap = foldr (\o m -> insertWith (+) o (SWeight 0) m) empty grid
   
   start = head grid 
   end   = last grid  
   
   -- calculate the spectral onsets that belong to a local meter
   spectralGrid :: Int -> Int -> [Int]
   spectralGrid ms p = let rm = ms `mod` p
                       in dropWhile (< start) [ rm, (rm + p) .. end ]
   
   onePeriod :: Int -> OnsetMap -> SWeightMap -> SWeightMap
   onePeriod p om wm = foldrWithKey oneMeter wm om where
     
     oneMeter :: Int -> Len -> SWeightMap -> SWeightMap
     oneMeter t (Len l) m' = foldr addWeight m' $ spectralGrid t p where
       
       -- addWeigth is executed very often, hence we pre-compute some values
       -- tp = t `mod` p
       w  = SWeight (l ^ (2 :: Int))
     
       -- adds the spectral onsets to the weight map
       addWeight :: Int -> SWeightMap -> SWeightMap
       addWeight o m'' = insertWith (+) o w m''

-- type MVecSW = (PrimMonad m) => MVector (PrimState m) SWeight 
type MVecSW m = MVector (PrimState m) SWeight 
       
getSpectralVec :: Period -> Period -> [Time] -> [Int] -> Vector SWeight
getSpectralVec _  _  _   []   = V.empty
getSpectralVec ml mP ons grid = runST $ do v <- initVec 
                                           (foldrM onePeriod v . toList . getLocalMeters ml mP $ ons) >>= freeze
                                           where
   
   initVec :: (PrimMonad m) => m (MVecSW m)
   initVec = thaw . V.replicate end . SWeight $ 0
   
   start = head grid 
   end   = last grid  
   
   -- calculate the spectral onsets that belong to a local meter
   spectralGrid :: Int -> Int -> [Int]
   spectralGrid ms p = let rm = ms `mod` p
                       in dropWhile (< start) [ rm, (rm + p) .. end ]
   
   onePeriod :: (PrimMonad m) => (Int, OnsetMap) -> MVecSW m -> m (MVecSW m)
   onePeriod (p, om) wm = foldrM oneMeter wm (toList om) where
     
     oneMeter :: (PrimMonad m) => (Int, Len) -> MVecSW m -> m (MVecSW m)
     oneMeter (t, Len l) mv = foldrM addWeight mv $ spectralGrid t p where
       
       -- addWeigth is executed very often, hence we pre-compute some values
       -- tp = t `mod` p
       w  = SWeight (l ^ (2 :: Int))
     
       -- adds the spectral onsets to the weight vector
       addWeight :: (PrimMonad m) => Int -> MVecSW m -> m (MVecSW m)
       addWeight i mv' = do curW <- MV.read mv' i
                            MV.write mv' i (curW + w)
                            return mv'