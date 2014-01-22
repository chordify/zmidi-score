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
module InnerMetricalAnalysisOld ( getMetricWeightOld
                                , getSpectralWeightOld 
                                , getLocalMetersOld
                                , showMeterMap2
                                , startProj
                                -- , isMax
                                ) where

import Prelude
import Data.List                  ( tails, foldl' )
import Data.IntMap                ( empty, IntMap, insert, mapWithKey, toAscList )
import qualified Data.IntMap as M ( lookup, foldr )
import LocalMeter

type Weight = Int

--------------------------------------------------------------------------------
-- Local Meters
--------------------------------------------------------------------------------

-- | Inner Metric Analysis defines the pulses of a piece of music solely on 
-- the base of note onsets. The model considers all the pulses, called 
-- local metres, that can overlay with each other with very different periods 
-- and shifted at all phases.
getLocalMetersOld :: Period -> [Time] -> MeterMap2
getLocalMetersOld _   []  = empty
getLocalMetersOld phs ons = foldl' onePeriod empty 
              -- todo change this into a parameter
              ([phs, (2 * phs) .. (Period (time . last $ ons) `div` 2)])    where
  
  onePeriod :: MeterMap2 -> Period -> MeterMap2
  onePeriod m p = insertMeters2 m p $ foldl' (startProj p) [] (tails ons)

startProj :: Period -> [(Time,Len)] -> [Time] -> [(Time,Len)]
startProj _ m []  = m
startProj p m tls = project p (head tls) 0 m tls where
-- startProj p m tls = traceShow (p,a) a where a = project (head tls) 0 tls p m
    
  -- given a phase (IOI), projects a local meter forward
  project :: Period -> Time -> Len -> [(Time,Len)] -> [Time] -> [(Time,Len)]
  project p t l m []  = addLMeter m p t l
  project p t l m [_] = addLMeter m p t l
  project p t l m (x : y : tl)
    -- phase is larger than the ioi, go to the next ioi
    | ioi  < period p = project p t       l  m (x : tl) 
    -- phase and ioi are identical, we add 1 to the Len of this local meter
    | ioi == period p = project p t (succ l) m (y : tl)
    -- ioi > p: phase is smaller than the ioi, we're done with this local meter
    -- Next, if the local meter is at least projected twice (l=2) we add
    -- an LMeter and otherwise we continue
    | otherwise = addLMeter m p t l
        where ioi = time (y - x)

-- Adds a new local meter to a list of local meters with the same period
addLMeter :: [(Time, Len)] -> Period -> Time -> Len -> [(Time,Len)]
addLMeter m p t l | isMax p m p (t,l) && (len l) >= 2 = (t,l) : m
                  | otherwise                         =         m
-- addLMeter m p t l | isMax p m p (t,l) && (len l) >= 2 = trace (show m ++ ": add: " ++ show (p,t,l)) ((t,l) : m)
                  -- | otherwise                         = trace (show m ++ ": no add: " ++ show (p,t,l))        m
                  
insertMeters2 :: MeterMap2 -> Period -> [(Time, Len)] -> MeterMap2
insertMeters2 m p l = case filter (isMaximal m p) l of 
                        [] -> m 
                        x  -> insert (period p) x m
                        -- [] -> trace ("skip: " ++ show p ++ show l) m 
                        -- x  -> trace ("insert: " ++ show p ++ show l) (insert (period p) x m)

type MeterMap2 = IntMap [(Time, Len)]

showMeterMap2 :: MeterMap2 -> String
showMeterMap2 = concatMap showPer . toAscList

showPer :: (Int, [(Time,Len)]) -> String
showPer (p, l) =  "Period: " ++ show p 
               ++ concatMap (showMeter p) l ++ "\n"

showMeter :: Int -> (Time, Len) -> String
showMeter p (t, Len l) = " (onset="++ show t++ " per=" ++ show p ++ " len="++ show l ++ ")"

isMaximal :: MeterMap2 -> Period -> (Time, Len) -> Bool
isMaximal m p tl = and . map (isMaxInMeterMap m) . factors $ p where

  isMaxInMeterMap :: MeterMap2 -> Period -> Bool
  isMaxInMeterMap m' f  = case M.lookup (period f) m' of
                            Nothing -> True
                            Just l  -> isMax f l p tl
                            
-- being maximal means not being a subset
isMax :: Period -> [(Time, Len)] -> Period -> (Time, Len) -> Bool
isMax f m  p x = and $ map (not . isSubSet p x f) m
-- isMax f m p x =  traceShow (p,x,a) a where a = and $ map (not . isSubSet p x f) m

-- returns true if the first pair is a meter that is a subset of the
-- second meter pair
-- N.B. precondition: pb > pa
isSubSet :: Period -> (Time, Len) -> Period -> (Time, Len) -> Bool
isSubSet (Period pa) (Time ta, Len la) (Period pb) (Time tb, Len lb) = 
-- isSubSet :: LMeter -> LMeter -> Bool
-- isSubSet (LMeter ta pa la) (LMeter tb pb lb) =
     ta             >= tb             -- starts later
  && ta `mod` pb    == tb `mod` pb    -- has the same phase
  && ta + (la * pa) <= tb + (lb * pb) -- ends earlier


--------------------------------------------------------------------------------
-- Inner Metrical Analysis Weights
--------------------------------------------------------------------------------

-- | Based on the detection of all local metres in a given piece a metric weight 
-- for each onset is defined that reflects the amount of local metres that 
-- coincide at this onset. Hence, the basic idea is similar. Onsets where many 
-- pulses coincide get a greater weight than onsets where fewer pulses coincide. 
-- Moreover, the intuition modelled in the metric weight is that longer 
-- repetitions should contribute more weight than shorter ones.
getMetricWeightOld :: Period -> [Time] -> [Weight]
getMetricWeightOld phs ons = map snd $ getWeight partOfLMeter phs ons ons

-- | The spectral weight is based on the extension of each local metre throughout 
-- the entire piece.
getSpectralWeightOld :: Period -> [Time] -> [Weight]
getSpectralWeightOld _ []          = []
getSpectralWeightOld phs ons@(h:t) = map snd $ getWeight matchPhase phs ons 
                                           [h, (h + Time (period phs)) .. (last ons)]


getWeight :: (Time -> Int -> (Time,Len) -> Bool)
          -> Period -> [Time] -> [Time] -> [(Time, Weight)]
getWeight f phs ons grid = 
  let ms = getLocalMetersOld phs ons 
      
      makePair :: Time -> (Time, Weight)
      makePair o = (o, sumPowers2 . mapWithKey ( filterMetersPer (f o) ) $ ms)
      
  in  map makePair grid


filterMetersPer :: (Int -> (Time,Len) -> Bool)
                -> Int -> [(Time, Len)] -> [(Time, Len)]
filterMetersPer f p = filter (f p) 

-- partOfLMeter :: Time -> Period -> (Time, Length) -> Bool
-- partOfLMeter (Time o) (Period p) (Time t, Len l) = 
partOfLMeter :: Time -> Int -> (Time, Len) -> Bool
partOfLMeter (Time o) p (Time t, Len l) = 
  o >= t &&               -- located after the meter starts
  o <= t + (l * p) &&     -- located before the meter ends
  o `mod` p == t `mod` p  -- matches the phase of the meter

matchPhase :: Time -> Int -> (Time, Len) -> Bool
matchPhase (Time o) p (Time t, _) = o `mod` p == t `mod` p

-- takes a set of 'LMeter's, takes of every Len the power of 2 and sums
-- the results
sumPowers2 :: MeterMap2 -> Int
sumPowers2 = M.foldr ((+) . sumPower2Len ) 0 where

 sumPower2Len :: [(Time, Len)] -> Int
 sumPower2Len = sum . map ((^ (2 ::Int)) . len . snd)

