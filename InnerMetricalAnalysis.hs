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
module Main -- ( -- * Types for Local Meters
                               -- LMeter (..)
                             -- , Time
                             -- , Weight
                             -- , Period
                             -- , Len
                               -- -- * The Inner Metrical Analysis
                             -- , getLocalMeters
                             -- , getMetricWeight
                             -- , getSpectralWeight
                             -- , normalise
                               -- -- * parameters
                             -- -- , maximumPhase
                             -- -- , phaseStepSize
                             -- , main
                             -- , toList
                             -- )
                             where

import Data.List                  ( tails, concatMap, foldl'         )
import Data.IntMap                ( empty, IntMap, insert, toAscList )
import qualified Data.IntMap as M ( lookup )
import LocalMeter

import Debug.Trace

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

-- data LMeter = LMeter { start   :: Time
                     -- , period  :: Period
                     -- , mLen :: Len } deriving (Eq)
                     
-- instance Ord LMeter where
  -- compare a b = case compare (period a) (period b) of
                  -- EQ -> case compare (period a) (period b) of
                          -- EQ -> compare (start a) (start b)
                          -- cs -> cs
                  -- cp -> cp
                                  
-- instance Show LMeter where
  -- show (LMeter s p l) = '(' : show s ++ ", " ++ show p ++ ", " ++ show l ++ ")"

-- type Len = Int
-- type Period = Int
-- type Time   = Int
type Weight = Int

--------------------------------------------------------------------------------
-- Local Meters
--------------------------------------------------------------------------------

-- | Inner Metric Analysis defines the pulses of a piece of music solely on 
-- the base of note onsets. The model considers all the pulses, called 
-- local metres, that can overlay with each other with very different periods 
-- and shifted at all phases.
getLocalMeters :: Period -> [Time] -> MeterMap2
getLocalMeters _   []  = error "getLocalMeters: no onsets"
getLocalMeters phs ons = foldl' onePeriod empty 
              -- todo change this into a parameter
              ([phs, (2 * phs) .. (Period (time . last $ ons) `div` 2)])    where
  
  onePeriod :: MeterMap2 -> Period -> MeterMap2
  -- onePeriod :: Period -> MeterMap2 -> MeterMap2
  onePeriod m p = insertMeters2 m p $ foldl' (startProj p) [] (tails ons)
  -- onePeriod m p = insertMeters2 m p $ foldr (startProj p) [] (tails ons)
  
  startProj :: Period -> [(Time,Len)] -> [Time] -> [(Time,Len)]
  -- startProj p ons m = project (head ons) 0 ons p m
  -- startProj p ons m =  traceShow (p,a) a where a = project (head ons) 0 ons p m
  startProj _ m []  = m
  startProj p m tls = project (head tls) 0 tls p m
  -- startProj p m tls = traceShow (p,a) a where a = project (head tls) 0 tls p m
  
  -- given a phase (IOI), projects a local meter forward
  project :: Time -> Len -> [Time] -> Period -> [(Time,Len)] -> [(Time,Len)]
  project s l []  p m  = addLMeter m p s l
  project s l [_] p m  = addLMeter m p s l
  project s l (x : y : tl) p m
    -- phase is larger than the ioi, go to the next ioi
    | ioi  < period p = project s       l  (x : tl) p m
    -- phase and ioi are identical, we add 1 to the Len of this local meter
    | ioi == period p = project s (succ l) (y : tl) p m
    -- ioi > p: phase is smaller than the ioi, we're done with this local meter
    -- Next, if the local meter is at least projected twice (l=2) we add
    -- an LMeter and otherwise we continue
    | otherwise = addLMeter m p s l
        where ioi = time (y - x)

-- Adds a new local meter to a list of local meters with the same period
addLMeter :: [(Time, Len)] -> Period -> Time -> Len -> [(Time,Len)]
-- addLMeter m p t l | isMax p m p (t,l) && (len l) >= 2 = (t,l) : m
                  -- | otherwise                         =         m
addLMeter m p t l | isMax p m p (t,l) && (len l) >= 2 = trace ("add: " ++ show (p,t,l)) ((t,l) : m)
                  | otherwise                         = trace ("no add: " ++ show (p,t,l))        m
                  
insertMeters2 :: MeterMap2 -> Period -> [(Time, Len)] -> MeterMap2
insertMeters2 m p l = case filter (isMaximal m p) l of 
                        -- [] -> m 
                        -- x  -> insert (period p) x m
                        [] -> trace ("skip: " ++ show p ++ show l) m 
                        x  -> trace ("insert: " ++ show p ++ show l) (insert (period p) x m)

type MeterMap2 = IntMap [(Time, Len)]

isMaximal :: MeterMap2 -> Period -> (Time, Len) -> Bool
isMaximal m p tl = and . map (isMaxInMeterMap m) . factors $ p where

  isMaxInMeterMap :: MeterMap2 -> Period -> Bool
  isMaxInMeterMap m f  = case M.lookup (period f) m of
                           Nothing -> True
                           Just l  -> isMax f l p tl

-- being maximal means not being a subset
isMax :: Period -> [(Time, Len)] -> Period -> (Time, Len) -> Bool
isMax f m p x = and $ map (not . isSubSet p x f) m
-- isMax f m p x =  traceShow (p,x,a) a where a = and $ map (not . isSubSet p x f) m

-- returns true if the first pair is a meter that is a subset of the
-- second meter pair
-- N.B. precondition: pb > pa
isSubSet :: Period -> (Time, Len) -> Period -> (Time, Len) -> Bool
isSubSet (Period pa) (Time ta, Len la) (Period pb) (Time tb, Len lb) = 
-- isSubSet :: LMeter -> LMeter -> Bool
-- isSubSet (LMeter ta pa la) (LMeter tb pb lb) =
     ta             >= tb            -- starts later
  && ta `mod` pb    == tb `mod` pb    -- has the same phase
  && ta + (la * pa) <= tb + (lb * pb) -- ends earlier

{-
contains :: Period -> (Time, Len) -> Period -> (Time, Len) -> Bool
contains (Period pa) (Time ta, Len la) (Period pb) (Time tb, Len lb) = 
  not (  ta             <  tb            -- starts later
      || ta `mod` pa    /= tb `mod` pb    -- has the same phase
      || ta + (la * pa) >  tb + (lb * pb)) -- ends earlier
  
-- contains == isSubSet  
pContains :: Period -> (Time, Len) -> Period -> (Time, Len) -> Bool
pContains pa a pb b = isSubSet pa a pb b == contains pa a pb b
-}

showMeterMap2 :: MeterMap2 -> String
showMeterMap2 = concatMap showPer . toAscList

showPer :: (Int, [(Time, Len)]) -> String
showPer (p, l) = "Period: " ++ show p ++ concatMap (showMeter p) l ++ "\n"

showMeter :: Int -> (Time, Len) -> String
showMeter p (Time t, Len l) = " (onset="++ show t++ " per=" ++ show p ++ " len="++ show l ++ ")"
                           
  -- stop and create a new Local Meter if its size is larger than 2
  -- stop :: Set LMeter -> Time -> Len -> Period -> Set LMeter
  -- stop m s l p | l >= 2    = insert (LMeter s p l) m
               -- | otherwise = m

  -- delete the 'LMeter's that are a subset of other 'LMeter's
  -- filterMax :: Set LMeter -> Set LMeter
  -- filterMax s = Set.filter (not . hasSuperSet s) s where

    -- hasSuperSet :: Set LMeter -> LMeter -> Bool
    -- hasSuperSet x m = Set.foldr step False x where

      -- step :: LMeter -> Bool -> Bool
      -- step _  True  = True
      -- step lm False = m `isSubSet` lm


-- | Returns True if the first 'LMeter' is a subset of the second 'LMeter'. 
-- 'isSubSet' returns true of the two 'LMeter's are equal.
-- isSubSet :: LMeter -> LMeter -> Bool
-- isSubSet a b =  
     -- -- (period a) `mod` (period b) == 0 -- the phase of a is a multitude that of b
     -- period a    ==    period b       -- the above is wrong, I believe
  -- && start  a    >=    start  b       -- a starts after b
  -- && lMeterEnd a <=    lMeterEnd b    -- a ends before b
  -- && not (a == b)                     -- a is not identical to b

-- -- returns the ending postion ('Int') of an 'LMeter'
-- lMeterEnd :: LMeter -> Int
-- lMeterEnd (LMeter s p l) = s + (l *p)

--------------------------------------------------------------------------------
-- Inner Metrical Analysis Weights
--------------------------------------------------------------------------------

-- | Based on the detection of all local metres in a given piece a metric weight 
-- for each onset is defined that reflects the amount of local metres that 
-- coincide at this onset. Hence, the basic idea is similar. Onsets where many 
-- pulses coincide get a greater weight than onsets where fewer pulses coincide. 
-- Moreover, the intuition modelled in the metric weight is that longer 
-- repetitions should contribute more weight than shorter ones.

{-
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
-- matchesPhase :: Time -> LMeter -> Bool
-- matchesPhase o (LMeter strt per _len) = (o - strt) `mod` per == 0 
-}
    
-- takes a set of 'LMeter's, takes of every Len the power of 2 and sums
-- teh results
-- sumPowers2 :: Set LMeter -> Int
-- sumPowers2 = Set.foldr ((+) . (^ (2 ::Int)). mLen) 0 

-- normalise :: [Weight] -> [Float]
-- normalise ws = let mx = fromIntegral (maximum ws) 
               -- in map (\x -> fromIntegral x / mx) ws

-- testing
-- main :: IO ()
-- main = print $ getMetricWeight 1 [6,10,16,18,20,22,30,34,38,44,46,48,50,58,62,66,68,70,72,74,82,86,94,96,98,100,102,112,114,122,174,176,178,180,182,188,189,190,192,194,202,204,206,208,210,212,214,216,218,220,222,224,226,234,246,250,256,258,260,262,270,274,280,282,284,286,292,293,294,296,298,300,302,308,309,310,312,314]
               
               
jmrEx :: [Time]
jmrEx = [0,1,2,6,8,9,10,14,16,17,18,22,24,25,26,30]