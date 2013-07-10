{-# OPTIONS_GHC -Wall #-}
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
                               LMeter (..)
                             , Time
                             , Weight
                             , Period
                             , Length
                               -- * The Inner Metrical Analysis
                             , getLocalMeters
                             , getMetricWeight
                             , getSpectralWeight
                               -- * parameters
                             -- , maximumPhase
                             -- , phaseStepSize
                             )where

import Data.List                 ( tails              )
import qualified Data.Set as Set ( foldr, filter      ) 
import Data.Set                  ( Set, insert, empty )

--------------------------------------------------------------------------------
-- parameters
--------------------------------------------------------------------------------

-- | The maximum phase that can contribute to a local meter
-- maximumPhase  :: Period
-- maximumPhase  = 100

-- | The minimal phase step size 
-- phaseStepSize :: Period
-- phaseStepSize = 10

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

data LMeter = LMeter { start   :: Time
                     , period  :: Period
                     , mlength :: Length } deriving (Eq)
                     
instance Ord LMeter where
  compare a b = case compare (period a) (period b) of
                  EQ -> case compare (period a) (period b) of
                          EQ -> compare (start a) (start b)
                          cs -> cs
                  cp -> cp
                                  
instance Show LMeter where
  show (LMeter s p l) = '(' : show s ++ ", " ++ show p ++ ", " ++ show l ++ ")"

type Length = Int
type Period = Int
type Time   = Int
type Weight = Int

--------------------------------------------------------------------------------
-- Local Meters
--------------------------------------------------------------------------------

-- | Inner Metric Analysis defines the pulses of a piece of music solely on 
-- the base of note onsets. The model considers all the pulses, called 
-- local metres, that can overlay with each other with very different periods 
-- and shifted at all phases.
getLocalMeters :: Period -> [Time] -> Set LMeter
getLocalMeters phs = filterMax . foldr projectMeter empty . tails   where
  
  -- project the meters for the head of the list of onsets
  projectMeter :: [Time] -> Set LMeter -> Set LMeter
  projectMeter []  m = m
  projectMeter ons m = foldr (project (head ons) 0 ons) m  
                        [phs, (2 * phs) .. (last ons `div` 2)] 

  -- given a phase (IOI), projects a local meter forward
  project :: Time -> Length -> [Time] -> Period -> Set LMeter -> Set LMeter
  project s l []  p m  = stop m s l p
  project s l [_] p m  = stop m s l p
  project s l (x : y : tl) p m
    -- phase is larger than the ioi, go to the next ioi
    | ioi  < p = project s       l  (x : tl) p m
    -- phase and ioi are identical, we add 1 to the length of this local meter
    | ioi == p = project s (succ l) (y : tl) p m
    -- ioi > p: phase is smaller than the ioi, we're done with this local meter
    -- Next, if the local meter is at least projected twice (l=2) we add
    -- an LMeter and otherwise we continue
    | otherwise = stop m s l p 
        where ioi = y - x     
  
  -- stop and create a new Local Meter if its size is larger than 2
  stop :: Set LMeter -> Time -> Length -> Period -> Set LMeter
  stop m s l p | l >= 2    = insert (LMeter s p l) m
               | otherwise = m

  -- delete the 'LMeter's that are a subset of other 'LMeter's
  filterMax :: Set LMeter -> Set LMeter
  filterMax s = Set.filter (not . hasSuperSet s) s where

    hasSuperSet :: Set LMeter -> LMeter -> Bool
    hasSuperSet x m = Set.foldr step False x where

      step :: LMeter -> Bool -> Bool
      step _  True  = True
      step lm False = m `isSubSet` lm

-- | Returns True if the first 'LMeter' is a subset of the second 'LMeter'. 
-- 'isSubSet' returns true of the two 'LMeter's are equal.
isSubSet :: LMeter -> LMeter -> Bool
isSubSet a b =  
     (period a) `mod` (period b) == 0 -- the phase of a is a multitude that of b
  && start  a    >=    start  b       -- a starts after b
  && lMeterEnd a <=    lMeterEnd b    -- a ends before b
  && not (a == b)                     -- a is not identical to b

-- returns the ending postion ('Int') of an 'LMeter'
lMeterEnd :: LMeter -> Int
lMeterEnd (LMeter s p l) = s + (l *p)

--------------------------------------------------------------------------------
-- Inner Metrical Analysis Weights
--------------------------------------------------------------------------------

-- | Based on the detection of all local metres in a given piece a metric weight 
-- for each onset is defined that reflects the amount of local metres that 
-- coincide at this onset. Hence, the basic idea is similar. Onsets where many 
-- pulses coincide get a greater weight than onsets where fewer pulses coincide. 
-- Moreover, the intuition modelled in the metric weight is that longer 
-- repetitions should contribute more weight than shorter ones.
getMetricWeight :: Period -> [Time] -> [Weight]
getMetricWeight phs = map snd . getMetricWeight' phs 

getMetricWeight' :: Period -> [Time] -> [(Time, Weight)]
getMetricWeight' phs ons = 
  let ms = getLocalMeters phs ons 
      
      getWeight :: Time -> (Time, Weight)
      getWeight o = (o, sumPowers2 . Set.filter ( hasLocalMeter o) $ ms)
      
      hasLocalMeter :: Time -> LMeter -> Bool
      hasLocalMeter o m = matchesPhase o m && o >= (start m) && o <= lMeterEnd m
      
  in  map getWeight ons

-- | The spectral weight is based on the extension of each local metre throughout 
-- the entire piece.
getSpectralWeight :: Period -> [Time] -> [Weight]
getSpectralWeight phs = map snd . getSpectralWeight' phs 

getSpectralWeight' :: Period -> [Time] -> [(Time, Weight)]
getSpectralWeight' phs ons = 
  let ms = getLocalMeters phs ons 
      
      getWeight :: Time -> (Time, Weight)
      getWeight o = (o, sumPowers2 . Set.filter (matchesPhase o) $ ms )
 
  in map getWeight [head ons, (phs + head ons)  .. last ons]
  
-- given an onset and an 'LMeter' returns True if both have the same 
-- phase, which means that the onset coincides with the grid of the 'LMeter'
matchesPhase :: Time -> LMeter -> Bool
matchesPhase o (LMeter strt per _len) = (o - strt) `mod` per == 0 

    
-- takes a set of 'LMeter's, takes of every length the power of 2 and sums
-- teh results
sumPowers2 :: Set LMeter -> Int
sumPowers2 = Set.foldr ((+) . (^ (2 ::Int)). mlength) 0 

normalise :: [Weight] -> [Float]
normalise ws = let mx = fromIntegral (maximum ws) 
               in map (\x -> fromIntegral x / mx) ws

