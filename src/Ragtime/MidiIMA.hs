
-- | applying the Inner Metric Analysis to Midi files ('ZMidi.Score')
module Ragtime.MidiIMA ( 
                         collectNSWProf
                       , findMeter
                       , toNSWProfSegs 
                       , toNSWProf
                       , doIMA
                       , NSWMeterSeg
                       ) where

import ZMidi.Score         hiding ( numerator, denominator )
import ZMidi.IO.Common            ( readQMidiScoreSafe, mapDirInDir, mapDir, warning )
import ZMidi.Skyline.MelFind      ( mergeTracks )
import Ragtime.TimeSigSeg
import Ragtime.NSWProf

import IMA.InnerMetricalAnalysis hiding           ( Time(..) )
import qualified IMA.InnerMetricalAnalysis as IMA ( Time(..) )

import System.Environment          ( getArgs )
import Data.List                   ( nubBy, foldl', maximumBy )
import Data.Ratio                  ( numerator, denominator )
import Data.Function               ( on )
import Data.Map.Strict             ( empty, Map, insertWith, unionWith, toList )
import Control.Arrow               ( first, second )
import Text.Printf                 ( printf )
import Data.Vector                 ( Vector, generate )




findMeter :: [(TimeSig, Vector NSWeight)] -> QMidiScore -> Either String [TimedSeg TimeSig TimeSig]
findMeter m qm = toNSWProfSegs qm >>= return . map updateSeg where

  updateSeg :: NSWProfSeg -> TimedSeg TimeSig TimeSig
  updateSeg p = p { seg = fst . bestMatch . toNSWVec (qGridUnit qm) . seg $ p }
  
  bestMatch :: Vector NSWeight -> (TimeSig, Double)
  bestMatch v = maximumBy (compare `on` snd) . map (second (euclDist v)) $ m

--------------------------------------------------------------------------------
-- Performing the Inner Metrical Analysis
--------------------------------------------------------------------------------

type NSWProfSeg = TimedSeg TimeSig NSWProf
type NSWVecSeg = TimedSeg TimeSig NSWProf

-- | Collects all profiles sorted by time signature in one map
collectNSWProf :: [NSWProfSeg] -> Map TimeSig NSWProf -> Map TimeSig NSWProf
collectNSWProf s m = foldr doSeg m s where

  doSeg :: NSWProfSeg -> Map TimeSig NSWProf -> Map TimeSig NSWProf
  doSeg (TimedSeg ts p) m' = insertWith mergeNSWProf (getEvent ts) p m'
  
  
-- | Transforms a quantised midi score into a set of meter profiles segmented
-- by the time signatures as prescribed in the midi file.
toNSWProfSegs :: QMidiScore -> Either String [NSWProfSeg]
toNSWProfSegs m = doIMA m >>= return . map (toNSWProf (ticksPerBeat . qMidiScore $ m))

-- | Calculates sums the NSW profiles for a meter section
toNSWProf :: Time ->  NSWMeterSeg -> NSWProfSeg
toNSWProf tpb (TimedSeg ts s) = TimedSeg ts (foldl' toProf (1,empty) s) where

  toProf :: NSWProf -> Timed (Maybe ScoreEvent, NSWeight) -> NSWProf
  toProf (_b, m) (Timed g (_se,w)) = 
    let (br, bt) = getBeatInBar (getEvent ts) tpb g 
        m'       = insertWith (+) bt w m 
    in  m' `seq` (fromIntegral br, m')

--------------------------------------------------------------------------------
-- Performing the Inner Metrical Analysis
--------------------------------------------------------------------------------

type NSWMeterSeg = TimedSeg TimeSig [Timed (Maybe ScoreEvent, NSWeight)]
-- TODO create a MPMidiScore for monophonic MidiScores
-- TODO create a QMPMidiScore for quantised monophonic MidiScores

doIMA :: QMidiScore -> Either String [NSWMeterSeg]
doIMA qms = 
  let v    = toMonoVoice . qMidiScore $ qms
      md   = fromIntegral . minDur . qMidiScore $ qms
  in     return (toIMAOnset v)
     >>= addMaxPerCheck 
     >>= return . getSpectralWeight md
     -- >>= return . getMetricWeightGrid md
     >>= return . matchScore v
     >>= return . segment (getTimeSig . qMidiScore $ qms)
                

-- merges all tracks and applies 'makeMono' to the result
toMonoVoice :: MidiScore -> Voice 
toMonoVoice = makeMono . head . getVoices . mergeTracks where

  -- filters NoteEvents, and deletes events with the same onset time
  makeMono :: Voice -> Voice 
  -- TODO : we should be able to use Data.List.Ordered, but this nub give
  -- other results, this must be investigated
  makeMono = nubBy ((==) `on` onset) . filter isNoteEvent
      
-- Transforms a 'Voice' into a list of IMA onsets
toIMAOnset :: Voice -> [IMA.Time]
toIMAOnset = map fromIntegral . toOnsets 

-- combines a 'Voice' with its spectral weights
matchScore :: Voice -> [(Int, SWeight)] -> [Timed (Maybe ScoreEvent, NSWeight)]
matchScore v s = match (map (first Time) s) v where

  -- The maximum 'Weight' found among the weights
  mx = maximum . map snd $ s
  
  -- | matches a grid with spectral weights with the onsets that created the
  -- weights. 
  match :: [(Time, SWeight)] -> Voice -> [Timed (Maybe ScoreEvent, NSWeight)]
  match [] []              = []
  match ((g, w):ws) []     =          addWeight w (Left g) : match ws []
  match ((g, w):ws) (t:ts) | g <  o = addWeight w (Left g) : match ws (t:ts)
                           | g == o = addWeight w (Right t): match ws ts
                           | otherwise = error "unmatched onset"
                               where o = onset t
  match _ _                = error "list of unequal lengths"             

  -- Normalises a spectral weight and combines it with a possible score event
  addWeight :: SWeight -> Either Time (Timed ScoreEvent) 
            -> Timed (Maybe ScoreEvent, NSWeight)
  addWeight w e = either ((flip Timed) (Nothing, w')) f e
    
    where w'  = fromIntegral w / fromIntegral mx
          f t = t {getEvent = (Just $ getEvent t, w')}
          