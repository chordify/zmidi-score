{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | applying the Inner Metric Analysis to Midi files ('ZMidi.Score')
module Ragtime.MidiIMA ( 
                         pickMeters
                       , matchMeters
                       , meterCheck
                       , selectMeters
                       , fourBarFilter
                       , collectNSWProf
                       , toNSWProfSegs 
                       , printIMA
                       , printPickMeter
                       , printMeterMatchVerb
                       ) where

import Ragtime.NSWProf 
import ZMidi.Score         hiding ( numerator, denominator )
import ZMidi.Skyline.MelFind      ( mergeTracks )
import Ragtime.TimeSigSeg         ( TimedSeg (..), segment )

import IMA.InnerMetricalAnalysis hiding           ( Time(..) )
import qualified IMA.InnerMetricalAnalysis as IMA ( Time(..) )

import Data.List                   ( nubBy, foldl', minimumBy, intercalate )
import Data.Function               ( on )
import Data.Map.Strict             ( empty, Map, insertWith, filterWithKey )
import qualified Data.Map.Strict as M ( lookup )
import Control.Arrow               ( first )
import Data.Vector                 ( Vector )

import Text.Printf                 ( printf, PrintfArg )
import Data.Ratio                  ( numerator, denominator, )


-- | Normalised spectral weights distance, obtained by matching two 'NSWProf's
-- newtype NSWDist = NSWDist { nswdist :: Double }
newtype NSWDist = NSWDist Double 
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Floating
                             , Fractional, RealFloat, RealFrac, PrintfArg )

data PMatch = PMatch {  pmTimeSig :: TimeSig
                     ,  pmatch    :: NSWDist
                     , _pmProf    :: NSWProf
                     } deriving (Eq)
                     
instance Show PMatch where
  show (PMatch ts m p) = printf (show ts ++ ": %1.4f\n" ++ show p) m 
  showList l s = s ++ (intercalate "\n" . map show $ l)
                 
-- | Picks the best matching profile
pickMeters :: [TimedSeg TimeSig [PMatch]] 
           -> Either String [TimedSeg TimeSig PMatch]
pickMeters = Right . map (fmap (minimumBy (compare `on` pmatch)))

-- | Given 'vectorize'd NSWProf'es, matches every meter segment in 
-- a 'QMidiScore' to the vectorized profiles
matchMeters :: [(TimeSig, Vector NSWeight)] -> QMidiScore 
            -> Either String [TimedSeg TimeSig [PMatch]]
matchMeters m qm = doIMA qm >>= fourBarFilter tpb >>= return . map matchAll where

  tpb = ticksPerBeat . qMidiScore $ qm

  matchAll :: NSWMeterSeg -> TimedSeg TimeSig [PMatch]
  matchAll = fmap (\td -> map (match td) m) 
  
  match :: [Timed (Maybe ScoreEvent, SWeight)] -> (TimeSig, Vector NSWeight) -> PMatch
  match td v = matchTS (qToQBins qm) tpb td v
  
-- | Calculates the match between an annotated and IMA estimated meter
meterCheck :: Map TimeSig NSWProf -> QMidiScore 
          -> Either String [TimedSeg TimeSig (NSWProf, NSWProf, NSWeight)]
meterCheck m qm = toNSWProfSegs qm >>= return . map match where

  match :: TimedSeg TimeSig NSWProf -> TimedSeg TimeSig (NSWProf, NSWProf, NSWeight)
  match (TimedSeg ts pa) = case M.lookup (getEvent ts) m of
                             Nothing -> error ("TimeSig not found: " ++ show ts)
                             Just pb -> TimedSeg ts (pa, pb, dist (qToQBins qm) (f pa) (f pb))
                                where f = vectorize (qToQBins qm) (getEvent ts)

matchTS :: QBins -> Time -> [Timed (Maybe ScoreEvent, SWeight)] 
        -> (TimeSig, Vector NSWeight) -> PMatch
matchTS qb tpb td (ts,v) = let p = toNSWProfWithTS ts tpb td
                               m = dist qb v (vectorize qb ts p)
                           in  PMatch ts (NSWDist . nsweight $ m) p
                                

  
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
toNSWProfSegs m =   doIMA m 
                >>= fourBarFilter (ticksPerBeat . qMidiScore $ m)
                >>= return . map (toNSWProf (ticksPerBeat . qMidiScore $ m))

-- | Sums all NSW profiles per bar for a meter section using the annotated
-- meter of that section
toNSWProf :: Time ->  NSWMeterSeg -> NSWProfSeg
toNSWProf tpb s = fmap (toNSWProfWithTS (getEvent . boundary $ s) tpb) s

-- | Sums all NSW profiles per bar for a meter section using a specific meter
toNSWProfWithTS :: TimeSig -> Time -> [Timed (Maybe ScoreEvent, SWeight)] 
                -> NSWProf
toNSWProfWithTS NoTimeSig _ _ = error "toNSWProfWithTS applied to NoTimeSig"
toNSWProfWithTS ts tpb td = foldl' toProf (NSWProf (1, empty)) td

  where toProf :: NSWProf -> Timed (Maybe ScoreEvent, SWeight) -> NSWProf
        toProf (NSWProf (_b, m)) (Timed g (_se,w)) = 
          let (Bar br, bib, bt) = getBeatInBar ts tpb g 
              m'                = insertWith (+) (bib,bt) w m 
              -- Every iteration we update the number of bars (lazily) 
              -- Hence, the last call to toProf will contain the final 
              -- number of bars correctly
          in  m' `seq` NSWProf (NrOfBars br, m')

-- | Removes the 'NSWProf's from the map of which the keys are not in the 
-- list of 'TimeSig's
selectMeters :: [TimeSig] -> Map TimeSig NSWProf -> Map TimeSig NSWProf
selectMeters ts = filterWithKey (\k _ -> k `elem` ts)
          
--------------------------------------------------------------------------------
-- Performing the Inner Metrical Analysis
--------------------------------------------------------------------------------

fourBarFilter :: Time -> [NSWMeterSeg] -> Either String [NSWMeterSeg]
fourBarFilter tpb = minBarLenFilter tpb (NrOfBars 4)

minBarLenFilter :: Time -> NrOfBars -> [TimedSeg TimeSig [Timed a]] 
                -> Either String [TimedSeg TimeSig [Timed a]]
minBarLenFilter tpb bs s = 
  case filter (\x -> notEmpty x && getNrOfBars tpb x > bs) s of
    [] -> Left ("minBarLenFilter: no segments longer then " ++ show bs)
    s' -> Right s'
  
getNrOfBars :: Time -> TimedSeg TimeSig [Timed a] -> NrOfBars
getNrOfBars tpb (TimedSeg ts []) = error "getNrOfBeats: empty List"
getNrOfBars tpb (TimedSeg ts x ) = 
  let (br, _beat, _btrat) = getBeatInBar (getEvent ts) tpb (onset . last $ x)
  in  NrOfBars (bar br)

notEmpty :: TimedSeg a [b] -> Bool
notEmpty (TimedSeg _ []) = False
notEmpty _               = True
  
type NSWMeterSeg = TimedSeg TimeSig [Timed (Maybe ScoreEvent, SWeight)]
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
matchScore :: Voice -> [(Int, SWeight)] -> [Timed (Maybe ScoreEvent, SWeight)]
matchScore v s = match (map (first Time) s) v where

  -- The maximum 'Weight' found among the weights
  mx = maximum . map snd $ s
  
  -- | matches a grid with spectral weights with the onsets that created the
  -- weights. 
  match :: [(Time, SWeight)] -> Voice -> [Timed (Maybe ScoreEvent, SWeight)]
  match [] []              = []
  match ((g, w):ws) []     =          addWeight w (Left g) : match ws []
  match ((g, w):ws) (t:ts) | g <  o = addWeight w (Left g) : match ws (t:ts)
                           | g == o = addWeight w (Right t): match ws ts
                           | otherwise = error "unmatched onset"
                               where o = onset t
  match _ _                = error "list of unequal lengths"             

  -- Normalises a spectral weight and combines it with a possible score event
  addWeight :: SWeight -> Either Time (Timed ScoreEvent) 
            -> Timed (Maybe ScoreEvent, SWeight)
  addWeight w e = either ((flip Timed) (Nothing, w)) f e
    
    -- where w'  = fromIntegral w / fromIntegral mx
    where -- f t = t {getEvent = (Just $ getEvent t, w')}
          f = fmap (\x -> (Just x, w))
          
--------------------------------------------------------------------------------
-- Printing the Inner Metrical Analysis
--------------------------------------------------------------------------------

printMeterMatchVerb :: QBins -> TimedSeg TimeSig (NSWProf, NSWProf, NSWeight) -> String
printMeterMatchVerb qb (TimedSeg (Timed _ ts) (a,b,d)) = 
  "\nsong:\n"     ++ show ts ++ show a ++
  "\ntemplate:\n" ++ show ts ++ show b ++
  "\nsong:\n"     ++ disp (vectorize qb ts a) ++
  "\ntemplate:\n" ++ disp (vectorize qb ts b) ++
  printf ('\n' : (show ts) ++ "\t%.6f ") d
  
printPickMeter :: TimedSeg TimeSig PMatch -> String
printPickMeter (TimedSeg ts m) = 
  let ann = getEvent ts
      est = pmTimeSig m
      s = intercalate "\t" [shwTs ann, shwTs est, show (ann == est), "%.3f "]
      
      shwTs :: TimeSig -> String
      shwTs x = '\'' : show x ++ "\'"
  
  in printf s (pmatch m)

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
  toStar :: Time -> TimeSig -> Timed (Maybe ScoreEvent, SWeight) -> IO ()
  toStar os x (Timed g (se,w)) = 
    let (br, bib, BeatRat r) = getBeatInBar x tpb g
    in putStrLn (printf ("%6d: %3d.%1d - %2d / %2d: " ++ showMSE se ++ ": " ++ stars w) 
                (g+os) br bib (numerator r) (denominator r)) 
                
  showMSE :: Maybe ScoreEvent -> String
  showMSE = maybe "    " (show . pitch) 
          