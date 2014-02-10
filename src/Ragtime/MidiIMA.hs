
-- | applying the Inner Metric Analysis to Midi files ('ZMidi.Score')
module Ragtime.MidiIMA ( 
                         findMeter
                       , meterMatch
                       , collectNSWProf
                       , toNSWProfSegs 
                       , printIMA
                       , printMeterMatch
                       , printMeterMatchVerb
                       ) where

import ZMidi.Score         hiding ( numerator, denominator )
import ZMidi.Skyline.MelFind      ( mergeTracks )
import Ragtime.TimeSigSeg         ( TimedSeg (..), segment )
import Ragtime.NSWProf

import IMA.InnerMetricalAnalysis hiding           ( Time(..) )
import qualified IMA.InnerMetricalAnalysis as IMA ( Time(..) )

import Data.List                   ( nubBy, foldl', minimumBy )
import Data.Function               ( on )
import Data.Map.Strict             ( empty, Map, insertWith )
import qualified Data.Map.Strict as M ( lookup )
import Control.Arrow               ( first, second )
import Data.Vector                 ( Vector )
import Text.Printf                 ( printf )
import Data.Ratio                  ( numerator, denominator, )
import Ragtime.VectorNumerics      ( euclDist, disp )

findMeter :: [(TimeSig, Vector NSWeight)] -> QMidiScore 
          -> Either String [TimedSeg TimeSig TimeSig]
findMeter m qm = toNSWProfSegs qm >>= return . map updateSeg where

  updateSeg :: NSWProfSeg -> TimedSeg TimeSig TimeSig
  updateSeg (TimedSeg ts p) = 
    TimedSeg ts (fst . bestMatch . toNSWVec (getEvent ts) (qGridUnit qm) $ p)
  
  bestMatch :: Vector NSWeight -> (TimeSig, NSWeight)
  bestMatch v = minimumBy (compare `on` snd) . map (second (euclDist v)) $ m

meterMatch :: Map TimeSig NSWProf -> QMidiScore 
          -> Either String [TimedSeg TimeSig (NSWProf, NSWProf, NSWeight)]
meterMatch m qm = toNSWProfSegs qm >>= return . map match where

  match :: TimedSeg TimeSig NSWProf -> TimedSeg TimeSig (NSWProf, NSWProf, NSWeight)
  match (TimedSeg ts pa) = case M.lookup (getEvent ts) m of
                             Nothing -> error ("TimeSig not found: " ++ show ts)
                             Just pb -> TimedSeg ts (pa, pb, euclDist (f pa) (f pb))
                                where f = toNSWVec (getEvent ts) (qGridUnit qm)



  
--------------------------------------------------------------------------------
-- Calculate Normalised Spectral Weight Profiles
--------------------------------------------------------------------------------

type NSWProfSeg = TimedSeg TimeSig NSWProf

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
    let (Bar br, bib, bt) = getBeatInBar (getEvent ts) tpb g 
        m'       = insertWith (+) (bib,bt) w m 
    in  m' `seq` (NrOfBars br, m')

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
          
--------------------------------------------------------------------------------
-- Printing the Inner Metrical Analysis
--------------------------------------------------------------------------------
printMeterMatchVerb :: TimedSeg TimeSig (NSWProf, NSWProf, NSWeight) -> String
printMeterMatchVerb (TimedSeg ts (a,b,d)) = 
  "\nsong:\n"     ++ showNSWProf (getEvent ts, a) ++
  "\ntemplate:\n" ++ showNSWProf (getEvent ts, b) ++
  -- | N.B. hardcoded Gridunit...
  "\nsong:\n"     ++ disp (toNSWVec (getEvent ts) (GridUnit 12) a) ++
  "\ntemplate:\n" ++ disp (toNSWVec (getEvent ts) (GridUnit 12) b) ++
  printf ('\n' : (show . getEvent $ ts) ++ "\t%.6f ") d


  
printMeterMatch :: TimedSeg TimeSig (NSWProf, NSWProf, NSWeight) -> String
printMeterMatch (TimedSeg ts (_,_,d)) = 
  printf ('\'' : (show . getEvent $ ts) ++ "\'\t%.3f ") d

printIMA :: QMidiScore -> IO ([NSWProfSeg])
printIMA qm = do let tpb = ticksPerBeat . qMidiScore $ qm
                 mapM (toNSWProfPrint tpb) . either error id . doIMA $ qm where
                
  toNSWProfPrint :: Time ->  NSWMeterSeg -> IO (NSWProfSeg)
  toNSWProfPrint t s = starMeter t s >> return (toNSWProf t s)
                
          
-- Prints an Inner metrical analysis
starMeter :: Time -> NSWMeterSeg -> IO ()
starMeter tpb (TimedSeg (Timed t ts) s) = 
  do putStrLn . printf ("%6d: ======================= " ++ show ts 
                         ++ " =======================" ) $ t
     mapM_ (toStar t ts) s where
                    
  -- prints one line e.g. "1152 1 3 1C  ***************"
  toStar :: Time -> TimeSig -> Timed (Maybe ScoreEvent, NSWeight) -> IO ()
  toStar os x (Timed g (se,w)) = 
    let (br, bib, BeatRat r) = getBeatInBar x tpb g
    in putStrLn (printf ("%6d: %3d.%1d - %2d / %2d: " ++ showMSE se ++ ": " ++ stars w) 
                (g+os) br bib (numerator r) (denominator r)) 
                
  showMSE :: Maybe ScoreEvent -> String
  showMSE = maybe "    " (show . pitch) 
          