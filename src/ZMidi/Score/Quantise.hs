{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
-- |
-- Module      :  ZMidi.Score.Quantise
-- Copyright   :  (c) 2012--2014, Utrecht University 
-- License     :  LGPL-3
--
-- Maintainer  :  W. Bas de Haas <w.b.dehaas@uu.nl>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Functions for quantising a 'MidiScore'
module ZMidi.Score.Quantise ( -- * Quantisation specific datatypes
                              QMidiScore (..)
                            , ShortestNote (..)
                            , GridUnit (..)
                            , QBins (..)
                            , QDev (..)
                            , QDevPerc (..)
                            , QOpts (..)
                              -- * Quantisation functions
                            , quantise
                            , quantiseSafe
                            , quantiseQDevSafe
                              -- * Utilities
                            , avgQDev
                            , avgQDevQMS
                            , canBeQuantisedAt
                            , removeOverlap
                            , getMinGridSize
                            , qToQBins
                            , toQBins
                            , getNumForQBins
                            ) where

import ZMidi.Score.Datatypes
import ZMidi.Score.Utilities

import Control.Arrow              ( second )
import Text.Printf                ( printf, PrintfArg )

import Data.Binary                 ( Binary )
import GHC.Generics                ( Generic )
import Control.DeepSeq             ( NFData )
import Data.Ratio                  ( Ratio, numerator, denominator )

--------------------------------------------------------------------------------
-- Quantisation datatypes
--------------------------------------------------------------------------------
                      
-- | QMidiScore wraps around a 'MidiScore' and stores some additional 
-- information about the quantisation process.
data QMidiScore = QMidiScore { -- | The quantised 'MidiScore'
                               qMidiScore    :: MidiScore 
                               -- | The 'ShortestNote' that reflects the number
                               -- of quantisation bins (see 'toQBins')
                             , qShortestNote :: ShortestNote
                               -- | The 'GridUnit' describes the minimal length 
                               -- of a quantised event
                             , qGridUnit     :: GridUnit
                               -- | The cumulative quantisation deviation
                             , totDeviation  :: QDev
                             } deriving (Show, Eq, Generic)
instance Binary QMidiScore

-- | The 'ShortestNote' determines the minimal grid length of a quantised 
-- 'QMidiScore', when quantised with 'quantise'.
data ShortestNote = Eighth | Sixteenth | ThirtySecond 
                  | FourtyEighth | SixtyFourth
                    deriving (Eq, Show, Generic)
instance Binary ShortestNote

-- | The 'GridUnit' describes the minimal length of a quantised event and
-- is controlled by the number of 'QBins' 
newtype GridUnit  = GridUnit { gridUnit :: Int } 
                      deriving ( Eq, Show, Num, Ord, Enum, Real, Integral, Binary )

-- | The 'QBins' describes the number of quantisation bins per (annotated) beat 
-- length and is generally controlled by the 'ShortestNote' parameter. 
-- (see also: 'toQBins' )
newtype QBins     = QBins    { qbins    :: Int } 
                      deriving ( Eq, Show, Num, Ord, Enum, Real, Integral, Binary, NFData )

-- | Represents a cumulative quantisation deviation, i.e. the number of ticks 
-- that an event was moved to match the time grid.
newtype QDev      = QDev     { qDev     :: Int }
                      deriving ( Eq, Show, Num, Ord, Enum, Real, Integral, Binary )
                      
-- | Represents the average quantisation deviation per onset
newtype QDevPerc = QDevPerc { qDevPerc :: Double }
                     deriving ( Eq, Show, Num, Ord, Enum, Real, Floating
                              , Fractional, RealFloat, RealFrac, PrintfArg, Binary )

-- | A datatype to store and collect Quantisation options. If the 'Maybe 
-- QDevPerc' is set to 'Nothing' there will be no selection based on
-- quantisation deviation.
data QOpts = QOpts { shortNoteOpt :: ShortestNote
                   , accQDevOpt   :: Maybe QDevPerc
                   } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Quantisation functions
--------------------------------------------------------------------------------
                      
-- | Quantises a 'MidiScore' snapping all events to a 'ShortestNote' grid.
quantise :: ShortestNote -> MidiScore -> QMidiScore
quantise sn = either error id . quantiseSafe sn

-- | Quantises a 'MidiScore' or returns a warning if the quantisation deviation
-- exceeds the 'acceptableQuantisationDeviation'.
quantiseQDevSafe :: QOpts -> MidiScore -> Either String QMidiScore
quantiseQDevSafe qo ms =   quantiseSafe (shortNoteOpt qo) ms
                       >>= maybe return qDevCheck (accQDevOpt qo)

-- | Checks if a 'QMidiScore' exceeds the 'acceptableQuantisationDeviation'
-- and return the 'QMidiScore' or a warning.
qDevCheck :: QDevPerc -> QMidiScore -> Either String QMidiScore
qDevCheck aqd qm 
  | d < aqd = Right qm
  | otherwise = Left (" ZMidi.Score.Quantise: " ++
                      "the average quantisation deviation is above " ++
                      printf "allowed deviation: %.3f < %.3f" d aqd )
                      
    where d = avgQDevQMS qm
    
-- | calculating the average quantisation deviation                 
avgQDevQMS :: QMidiScore -> QDevPerc
avgQDevQMS qm = avgQDev (qGridUnit qm) (totDeviation qm) 
                        (nrOfNotes . qMidiScore $ qm) where
    
-- | Calculates the average quantisation deviation per onset. The third
-- argument is assumed to be the number of notes.
avgQDev :: GridUnit -> QDev -> Int -> QDevPerc
avgQDev gu qd nrn = 
  QDevPerc ((fromIntegral qd / fromIntegral nrn) / fromIntegral gu)

-- | Quantises a 'MidiScore' snapping all events to a 'ShortestNote' grid. 
-- The absolute size of the grid is based on the 'GridUnit', which is the
-- 'ticksPerBeat' divided by the number of quantization bins per beat. Besides
-- the quantised 'MidiScore' and the 'GridUnit' also the cumulative deviation 
-- from the grid is returned.
quantiseSafe :: ShortestNote -> MidiScore -> Either String QMidiScore
quantiseSafe sn (MidiScore k ts (TPB dv) mf tp _md vs) =  
  -- the grid unit is the number of ticks per beat divided by the maximum
  -- number of notes in one beat
  case dv `divMod` (qbins . toQBins $ sn) of
    (gu, 0) -> let -- snap all events to a grid, and remove possible overlaps 
                   (vs',d) = unzip . map (quantiseVoice . GridUnit $ gu) $ vs 
                   -- the minimum duration might have changed
                   md' = gcIOId . buildTickMap $ vs'
                   -- also align the time signatures to a grid
                   ts' = map (snapTimed . GridUnit $ gu) ts
                   -- update the MidiScore
                   ms' = MidiScore k ts' (TPB dv) mf tp md' vs'
               in Right (QMidiScore ms' sn (GridUnit gu) (sum d))
    _       ->    Left ("ZMidi.Score.Quantise: MidiFile cannot be quantised: " 
                    ++ show dv ++ " cannot be divided by " ++ show (toQBins sn))
  
-- | quantises a 'Timed' event
snapTimed :: GridUnit -> Timed a -> Timed a
snapTimed gu t = t {onset = fst . snap gu $ onset t}  

-- | quantises a 'Voice', and returns the cumulative 'Deviation'.
quantiseVoice :: GridUnit -> Voice -> (Voice, QDev)
quantiseVoice gu = second sum . unzip . map snapEvent  where

  snapEvent :: Timed ScoreEvent -> (Timed ScoreEvent, QDev)
  snapEvent (Timed ons dat) = let (t', d) = snap gu ons 
                              in case dat of
    (NoteEvent c p v l) -> (Timed t' (NoteEvent c p v (fst $ snap gu l)), d)
    _                   -> (Timed t' dat                                , d)
    
-- | snaps a 'Time' to a grid
snap :: GridUnit -> Time -> (Time, QDev) 
snap gu t | m == 0  = (t, 0)          -- score event is on the grid
          | m >  0  = if (g - m) >= m -- score event is off the grid
                      -- and closer to the past grid point
                      then let t' =    d  * g 
                           in (t', fromIntegral (t  - t')) -- snap backwards
                      -- or closer to the next grid point
                      else let t' = (1+d) * g 
                           in (t', fromIntegral (t' - t )) -- snap forwards
          | otherwise = error "ZMidi.Score.Quantise: Negative time stamp found"
              where (d,m) = t `divMod` g
                    g     = fromIntegral gu


-- | Returns true if the number of ticks per beat can be divided by the 
-- maximal number of quantisation bins.
canBeQuantisedAt :: ShortestNote -> MidiScore -> Bool
canBeQuantisedAt sn ms =   ((tpb . ticksPerBeat   $ ms)
                      `mod` (qbins . toQBins $ sn)) == 0
    
-- | Although 'quantise' also quantises the duration of 'NoteEvents', it can
-- happen that melody notes do still overlap. This function removes the overlap
-- N.B. This function is designed only for monophonic melodies, it does not 
-- work on a polyphonic score.
removeOverlap :: Voice -> Voice
removeOverlap = foldr step [] where
  
  step :: Timed ScoreEvent -> [Timed ScoreEvent] -> [Timed ScoreEvent]
  step t [] = [t]
  step t n  = updateDur : n where
    
    updateDur :: Timed ScoreEvent
    updateDur = case getEvent t of
      (NoteEvent c p v d) -> let nxt = onset . head $ n
                                 d'  = if onset t + d > nxt 
                                       then nxt - onset t else d
                             in  t {getEvent = NoteEvent c p v d'}
      _                   -> t


-- | Returns the minimal grid size of a 'MidiScore' if it has been quantised. 
-- This is the 'ticksPerBeat' divided by the number of quantisation bins.
-- N.B. this function does not check whether a file is quantised.
getMinGridSize :: QMidiScore -> TPB
getMinGridSize ms = case (tpb . ticksPerBeat . qMidiScore $ ms) `divMod` 
                         (qbins . qToQBins $ ms) of
                        (d,0) -> TPB d
                        _     -> error ("ZMidi.Score.Quantise: " ++
                                        "getMinGridSize: invalid quantisation")
      
-- | Applies 'toQBins' to the 'ShortestNote' in a 'QMidiScore'
qToQBins :: QMidiScore -> QBins
qToQBins = toQBins . qShortestNote
      
-- | takes the quantisation granularity parameter 'ShortestNote' and returns
-- a the 'GridUnit' that the beat length should be divided by. The resulting 
-- value we name 'GridUnit'; it describes the minimal length of an event.
toQBins :: ShortestNote -> QBins
toQBins Eighth       = QBins 2
toQBins Sixteenth    = QBins 4
toQBins ThirtySecond = QBins 8
toQBins FourtyEighth = QBins 12
toQBins SixtyFourth  = QBins 16
  

-- | Returns the numerator of a Ratio given a certain 'QBins' as denominator.
-- The standard Ratio implementation simplifies the Ration, e.g. 3 % 12 
-- is converted into 1 % 4. This function reverses that process: 
-- 
-- >>> getNumForQBins 12 (1 % 4) 
-- >>> 3
-- 
-- >>> getNumForQBins 12 (1 % 1) 
-- >>> 12
getNumForQBins :: QBins -> Ratio Int -> Int
getNumForQBins (QBins q) r = numerator r * (q `div` denominator r)

  
  