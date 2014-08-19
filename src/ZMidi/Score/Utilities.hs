{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
module ZMidi.Score.Utilities ( 
                             -- * Minimum length calculation
                               TickMap
                             , buildTickMap
                             , gcIOId
                             -- * Utilities
                             , isTempoChange
                             , isTimeSig
                             , isKeyChange
                             , isNoteEvent
                             , nrOfNotes
                             , toIOIs
                             , toOnsets
                             , toMidiNr
                             , toPitch
                             , getPitch
                             , getInterval
                             , changePitch
                             , pitchClass
                             , hasTimeSigs
                             , updateTimeSig
                             -- * MidiFile Utitlites
                             , removeLabels
                             , hasNotes
                             ) where

import ZMidi.Score.Internal
import ZMidi.Score.Datatypes
import ZMidi.Core          ( MidiFile (..), MidiEvent (..)
                           , MidiVoiceEvent (..), MidiMetaEvent (..)
                           , MidiMessage, MidiTrack (..)
                           )
import Control.Monad.State ( State, modify, execState )
import Control.Arrow       ( first, (***) )
import Data.Word           ( Word8 )
import Data.Maybe          ( isJust )
import Data.List           ( find, sort )
import qualified Data.List.Ordered as Sort ( nub )
import Data.Foldable       ( foldrM )
import Data.IntMap.Lazy    ( insertWith, IntMap, keys )
import qualified Data.IntMap.Lazy  as M    ( empty )
  
--------------------------------------------------------------------------------
-- Analysing durations
--------------------------------------------------------------------------------

type TickMap = IntMap Time

-- | The Inter Onset Interval that is the greatest common divider. It can be
--used to estimate whether a track is quantised or not.
gcIOId :: TickMap -> Time
gcIOId tm = case keys $ tm of
  [] -> 0
  l  -> Time . foldr1 gcd $ l

buildTickMap :: [Voice] -> TickMap
buildTickMap = foldr oneVoice M.empty where

  -- calculated all IOIs and order them per duration
  oneVoice :: Voice -> TickMap -> TickMap
  oneVoice [] tm = tm -- to account for offsets we add the first onset too
  oneVoice vs tm = step (onset . head $ vs) . foldr step tm $ (toIOIs vs)

  step :: Time -> TickMap -> TickMap
  step (Time se) tm = insertWith succIfExists se 0 tm 
  
  succIfExists :: Time -> Time -> Time
  succIfExists _ old = succ old

-- printTickMap :: TickMap -> String
-- printTickMap tm = "tickmap:\n" ++ (concatMap showTick . toAscList $ tm) where
  
  -- showTick :: (Int, Time) -> String
  -- showTick (i, t) = show i ++ ": " ++ show t ++ "\n"

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Returns the number of 'ScoreEvent's in a 'MidiScore'
nrOfNotes :: MidiScore -> Int
nrOfNotes = sum . map length . getVoices

-- | Returns True if the 'ScoreEvent' is a time signature change  
isTimeSig :: Timed ScoreEvent -> Bool
isTimeSig (Timed _ (TimeSigChange _ )) = True
isTimeSig _                            = False

-- | Returns True if the 'ScoreEvent' is a key change
isKeyChange :: Timed ScoreEvent -> Bool
isKeyChange (Timed _ (KeyChange   _ )) = True
isKeyChange _                          = False

-- | Returns True if the 'ScoreEvent' is a key change
isTempoChange :: Timed ScoreEvent -> Bool
isTempoChange (Timed _ (TempoChange _ )) = True
isTempoChange _                          = False

-- | Returns True if the 'ScoreEvent' is a 'NoteEvent'
isNoteEvent :: Timed ScoreEvent -> Bool
isNoteEvent (Timed _ (NoteEvent _ _ _ _ )) = True
isNoteEvent _                              = False

-- | Returns the 'PitchClass' of a particular 'Pitch'.
pitchClass :: Pitch -> PitchClass
pitchClass (Pitch (_, pc)) = pc

-- | Returns the 'Pitch' of a 'Timed' 'ScoreEvent'. In case of a non-'NoteEvent'
-- an error will be thrown
getPitch :: Timed ScoreEvent -> Pitch
getPitch tse = case getEvent tse of 
  (NoteEvent _c p _v _d) -> p
  se                     -> error ("unexpected ScoreEvent: " ++ show se)

-- | Returns the posibly negative 'Interval' between two 'Pitch'es
getInterval :: Pitch -> Pitch -> Interval
getInterval (Pitch (Octave fo, PitchClass fpc)) 
            (Pitch (Octave to, PitchClass tpc)) = 
  let (oct', pc') = divMod (tpc - fpc) 12 
  in Interval ((12 * (to - fo + oct')) + pc')
  
-- | Changes a 'Pitch' with a particular 'Interval'
changePitch :: Pitch -> Interval -> Pitch
changePitch (Pitch (Octave oct, PitchClass pc)) (Interval i) = 
  let (octi, pci) = divMod  i         12 
      (oct', pc') = divMod (pc + pci) 12    
  in Pitch (Octave (oct + octi + oct'), PitchClass pc')

-- | Ignores all pitch information and returns a list of onsets. N.B. in case
-- of a polyphonic track duplicate onsets are deleted.
toOnsets :: Voice -> [Time]
toOnsets = Sort.nub . map onset
                                  
-- | Transforms a 'Voice' into a list of Inter Onset Intervals (IOIs)
toIOIs :: Voice -> [Time]
toIOIs v = execState (foldrM step [] v) [] where

  step :: Timed ScoreEvent -> [Timed ScoreEvent] 
       -> State [Time] [Timed ScoreEvent]
  step t []         = return [t]
  step t ts@(h : _) = do modify ((onset h - onset t) :)
                         return (t : ts)

-- | Converts a 'Pitch' into a MIDI note number
toMidiNr :: Pitch -> Word8
toMidiNr (Pitch (Octave o, PitchClass p)) = fromIntegral (((o + 5) * 12) + p)

-- | Converts a MIDI note number into an octave and a pitch class, a.k.a 'Pitch'
toPitch :: Word8 -> Pitch
toPitch = Pitch . (Octave *** PitchClass) . midiNrToPitch where
                           
  midiNrToPitch :: Word8 -> (Int, Int)
  midiNrToPitch p | p < 0     = invalidMidiNumberError p 
                  | p > 127   = invalidMidiNumberError p 
                  | otherwise = first (+ (-5)) (fromIntegral p `divMod` 12)

-- | Returns True if the 'MidiScore' has time signatures other than 'NoTimeSig'
hasTimeSigs :: MidiScore -> Bool
hasTimeSigs = not . null . filter (not . (== NoTimeSig) . getEvent) . getTimeSig

updateTimeSig :: MidiScore -> Timed TimeSig -> Timed TimeSig 
              -> Either String MidiScore
updateTimeSig ms old new
  | ts == ts' = Left ("updateTimeSig: TimeSig " ++ show old ++ " not found")
  | otherwise = Right $ ms {getTimeSig = sort (new : ts') }
    where ts  = getTimeSig ms
          ts' = filter (== old) ts
    
--------------------------------------------------------------------------------
-- Some MidiFile utilities
--------------------------------------------------------------------------------

hasNotes :: MidiTrack -> Bool
hasNotes = isJust . find isNoteOnEvent . getTrackMessages 

isNoteOnEvent :: MidiMessage -> Bool
isNoteOnEvent (_, (VoiceEvent _ (NoteOn _ _ _))) = True
isNoteOnEvent _                                  = False

removeLabels :: MidiFile -> MidiFile
removeLabels f = f { mf_tracks = map filterLab . mf_tracks $ f } where
  
  filterLab :: MidiTrack -> MidiTrack
  filterLab = MidiTrack . filter (not . isLab) . getTrackMessages
  
  isLab :: MidiMessage -> Bool
  isLab (_, (MetaEvent (TextEvent _ _))) = True
  isLab _                                = False
  