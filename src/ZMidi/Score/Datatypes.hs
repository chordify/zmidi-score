{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
module ZMidi.Score.Datatypes ( -- * Score representation of a MidiFile
                    MidiScore (..)
                  , Key (..)
                  , TimeSig (..)
                  , Voice 
                  , Channel (..)
                  , Pitch (..)
                  , PitchClass (..)
                  , Interval
                  , Velocity (..)
                  , Timed (..)
                  , Time (..)
                  , Bar (..)
                  , Beat (..)
                  , BeatRat (..)
                  , TPB (..)
                  , ScoreEvent (..)
                  -- * Minimum length calculation
                  , buildTickMap
                  , gcIOId
                  -- * Bar and beat positions
                  , getBeatInBar
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
                  -- * MidiFile Utilities
                  , hasNotes 
                  , isNoteOnEvent
                  , removeLabels
                  -- * Showing
                  , showMidiScore
                  , showVoices
                  ) where

import ZMidi.Core          ( MidiFile (..), MidiEvent (..), MidiFormat (..)
                           , MidiVoiceEvent (..), MidiMetaEvent (..)
                           , MidiMessage, MidiTrack (..), MidiScaleType (..)
                           )
import Control.Monad.State ( State, modify, get
                           , evalState, execState )
import Control.Monad       ( mapAndUnzipM )
import Control.Arrow       ( first, (***) )
import Data.Ratio          ( (%), Ratio )
import Data.Word           ( Word8 )
import Data.Int            ( Int8 )
import Data.Char           ( toLower )
import Data.Maybe          ( isJust )
import Data.List           ( intercalate, find, sort )
import qualified Data.List.Ordered as Sort ( nub )
import Data.Foldable       ( foldrM )
import Data.IntMap.Lazy    ( insertWith, IntMap, keys )
import qualified Data.IntMap.Lazy  as M    ( empty )
import Text.Printf         ( printf, PrintfArg )
import Data.Binary         ( Binary, Get )
import qualified Data.Binary       as B    ( get, put )
import GHC.Generics        ( Generic )

--------------------------------------------------------------------------------                                   
-- A less low-level MIDI data representation
--------------------------------------------------------------------------------

-- | Stores the main elements of a musical score that can be derived from a 
-- midifile
data MidiScore  = MidiScore     { -- | The 'Key's of the piece with time stamps
                                  getKey     :: [Timed Key]
                                  -- | The 'TimeSig'natures of the piece with time stamps
                                , getTimeSig :: [Timed TimeSig]
                                  -- | The number of MIDI-ticks-per-beat
                                , ticksPerBeat :: TPB  
                                  -- | The kind of midi file that created this score
                                , midiFormat :: MidiFormat
                                  -- | The microseconds per quarter note
                                , tempo      :: [Timed Time]                                
                                  -- | The minimum note length found.
                                , minDur     :: Time
                                  -- | The midi 'Voice's
                                , getVoices  :: [Voice]
                                } deriving (Eq, Show, Generic)
                     
data Key        = Key           { keyRoot    :: Int8
                                , keyMode    :: MidiScaleType
                                } 
                | NoKey           deriving (Eq, Ord, Generic)
               
-- | A 'TimeSig'nature has a fraction, e.g. 4/4, 3/4, or 6/8.
data TimeSig    = TimeSig       { tsNum      :: Int 
                                , tsDen      :: Int
                                , metronome  :: Word8
                                , nr32ndNotes:: Word8
                                }
                | NoTimeSig       deriving (Generic)

-- Note: we could consider adding a Voice label, like there is a track label
-- | A 'Voice' is a list of 'ScoreEvent's that have time stamps.
type Voice      = [Timed ScoreEvent]

newtype Channel    = Channel {channel :: Word8 }
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral, Binary )
-- TODO: better changed to Pitch (Int, PitchClass)
newtype Pitch   = Pitch    (Int, Int) deriving (Eq, Ord, Binary) -- (Octave, Pitch class)
type    Interval= Int

newtype PitchClass = PitchClass Int deriving (Eq, Ord) 

newtype Velocity = Velocity { velocity :: Word8 }
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral, Binary )
newtype Time    = Time { time :: Int } 
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral, Binary, PrintfArg )
newtype Bar     = Bar  { bar  :: Int } 
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral, Binary, PrintfArg )
newtype Beat    = Beat { beat :: Int } 
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral, Binary, PrintfArg )
newtype BeatRat = BeatRat { beatRat  :: Ratio Int } 
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Binary )                    
newtype TPB     = TPB { tpb :: Int } 
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral, Binary, PrintfArg )
                    
-- perhaps add duration??
data Timed a    = Timed         { onset       :: Time 
                                , getEvent    :: a
                                } deriving (Functor, Eq, Ord, Generic)
                                
data ScoreEvent = NoteEvent     { chan        :: Channel
                                , pitch       :: Pitch
                                , velo        :: Velocity
                                -- No, this should this not be in Timed 
                                -- because only a NoteEvent has a duration
                                -- (or we should give Key and TimeSig changes 
                                -- also a duration)
                                , duration    :: Time 
                                } 
                | KeyChange     { keyChange   :: Key
                                } 
                | TimeSigChange { tsChange    :: TimeSig
                                } 
                | TempoChange   { tempChange  :: Time
                                } deriving (Eq, Ord, Show, Generic)

type TickMap = IntMap Time

--------------------------------------------------------------------------------
-- Some ad-hoc show instances
--------------------------------------------------------------------------------

instance Show a => Show (Timed a) where
  show (Timed t a) = show a ++ " @ " ++ show (time t)

instance Eq TimeSig where
  (TimeSig a1 b1 _ _) == (TimeSig a2 b2 _ _) = a1 == a2 && b1 == b2
  NoTimeSig           == NoTimeSig           = True
  _                   == _                   = False

instance Ord TimeSig where
  compare _         NoTimeSig                     = GT
  compare NoTimeSig _                             = LT
  compare (TimeSig a1 b1 _ _) (TimeSig a2 b2 _ _) = 
    case compare b1 b2 of 
      EQ -> compare a1 a2
      c  -> c

instance Show TimeSig where
  show (TimeSig n d _ _) = show n ++ '/' : show d
  show NoTimeSig         = "NoTimeSig"  

instance Show Key where
  show NoKey      = "NoKey"
  show (Key rt m) = showRoot rt ++ ' ' : (map toLower . show $ m) where
      
    showRoot :: Int8 -> String
    showRoot i = let r = fromIntegral i in case compare r 0 of
      LT -> replicate (abs r) 'b'
      EQ -> "0"
      GT -> replicate r '#'

instance Show Pitch where
  show (Pitch (oct, p)) = showOct oct ++ showPitch p where
  
    showOct :: Int -> String
    showOct i | i < 0     = show i
              | otherwise = ' ' : show i

instance Show PitchClass where
  show (PitchClass p) = showPitch p
  
-- shows the Midi number in a musical way
-- N.B. ignoring all pitch spelling, at the moment
showPitch :: Int -> String
showPitch  0 = "C "
showPitch  1 = "C#"
showPitch  2 = "D "
showPitch  3 = "D#"
showPitch  4 = "E "
showPitch  5 = "F "
showPitch  6 = "F#"
showPitch  7 = "G "
showPitch  8 = "G#"
showPitch  9 = "A "
showPitch 10 = "Bb"
showPitch 11 = "B "
showPitch n  = invalidMidiNumberError n

-- Binary instances
instance Binary MidiScore
instance Binary TimeSig
instance Binary ScoreEvent
instance Binary Key  
instance (Binary a) => Binary (Timed a)
instance Binary MidiScaleType where
  put MAJOR           =    B.put (0 :: Word8)
  put MINOR           =    B.put (1 :: Word8)
  put (SCALE_OTHER i) = do B.put (2 :: Word8)
                           B.put i
                           
  get = do t <- B.get :: Get Word8
           case t of 
             0 -> return MAJOR
             1 -> return MINOR
             2 -> do i <- B.get
                     return (SCALE_OTHER i)
             _ -> error "invalid binary encoding of MidiScaleType"
      
instance Binary MidiFormat where
  put MF0 = B.put (0 :: Word8)
  put MF1 = B.put (1 :: Word8)
  put MF2 = B.put (2 :: Word8)
  
  get = do t <- B.get :: Get Word8
           case t of 
             0 -> return MF0           
             1 -> return MF1           
             2 -> return MF2
             _ -> error "invalid binary encoding of MidiFormat"
--------------------------------------------------------------------------------
-- Printing MidiScores
--------------------------------------------------------------------------------

-- Show a MidiScore in a readable way
showMidiScore :: MidiScore -> String
showMidiScore ms@(MidiScore k ts tb mf tp st _vs) = "Key: "      ++ show k 
                                     ++ "\nMeter: "  ++ show ts
                                     ++ "\nTicks per Beat: "  ++ show tb 
                                     ++ "\nMidi format: " ++ show mf 
                                     ++ "\nTempo: "  ++ show tp 
                                     ++ "\nShortest tick: "   ++ show st
                                     ++ "\nNotes:\n" ++ showVoices ms

-- Shows the voices in a MidiScore in a readable way, but this function
-- only works for monophonic channels. TODO: fix
showVoices :: MidiScore -> String
showVoices ms = intercalate "\n" $ evalState (showTimeSlice . getVoices $ ms) 0 
  where
  
  -- shows a the sounding notes at a specific time 
  showTimeSlice :: [Voice] -> State Time [String]
  showTimeSlice vs = 
    do (str, vs') <- mapAndUnzipM showVoiceAtTime vs
       case noMoreNotesToShow vs' of        -- the stopping condition
         True  -> return []
         False -> do t <- get
                     --  update the clock
                     modify (+ minDur ms)
                     x <- showTimeSlice vs' -- recursively calculate a next slice
                     -- the output string:
                     let out = intercalate "\t" (printf "%7d" (time t) : str)                                    
                     return (out : x)

  -- The stopping condition: when there are no more notes in all of the tracks
  noMoreNotesToShow :: [Voice] -> Bool
  noMoreNotesToShow = and . map null 
  
  -- Takes a Voice and shows the head of the voice appropriately depending on
  -- the location in time. showVoicAtTime also returns the tail of the voice.
  showVoiceAtTime :: Voice -> State Time (String, Voice)
  showVoiceAtTime []       = return ("  ",[])
  showVoiceAtTime x@(v:vs) = 
    do t <- get
       case hasStarted t v of
         True  -> case hasEnded t v of
                    -- if v has ended, an new note at the same voice
                    -- might start at the same time. Hence, call ourselves again
                    True  -> showVoiceAtTime vs 
                    False -> return (show . pitch . getEvent $ v, x)
         False ->            return ("  "                       , x)

  hasStarted, hasEnded :: Time -> Timed ScoreEvent -> Bool
  -- returns true if the Timed ScoreEvent is has started to sound at Time t
  hasStarted t tse = t >= onset tse 
  -- returns true if the Timed ScoreEvent is still sounding at Time t
  hasEnded t (Timed ons se) = ons + duration se  <  t

  
--------------------------------------------------------------------------------
-- Analysing durations
--------------------------------------------------------------------------------

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
-- Bar & Beat position
--------------------------------------------------------------------------------

-- Within a 'MidiScore' we can musically describe every (quantised)
-- position in time in 'Bar', Beat, and 'BarRat'. Therefore, we need the 
-- 'TimeSig'nature, the length of a beat ('TPB', in ticks), and the actual
-- 'Time' stamp.
-- TODO move these newtypes to the Quantise module?
getBeatInBar :: TimeSig -> TPB -> Time -> (Bar, Beat, BeatRat)
getBeatInBar NoTimeSig _ _ = error "getBeatInBar applied to noTimeSig"
getBeatInBar (TimeSig num _den _ _) t o = 
  let (bt, rat) = getRatInBeat t o
      (br, bib) = (succ *** succ) $ fromIntegral bt `divMod` num 
  in (Bar br, Beat bib, rat)

-- | Returns the position within a 'Bar', see 'getBeatInBar'.
getRatInBeat :: TPB -> Time -> (Beat, BeatRat)
getRatInBeat (TPB t) (Time o) = 
  ((Beat) *** (BeatRat . (% t))) (o `divMod` t)

 
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
pitchClass (Pitch (_, pc)) = PitchClass pc

-- | Returns the 'Pitch' of a 'Timed' 'ScoreEvent'. In case of a non-'NoteEvent'
-- an error will be thrown
getPitch :: Timed ScoreEvent -> Pitch
getPitch tse = case getEvent tse of 
  (NoteEvent _c p _v _d) -> p
  se                     -> error ("unexpected ScoreEvent: " ++ show se)

-- | Returns the posibly negative 'Interval' between two 'Pitch'es
getInterval :: Pitch -> Pitch -> Interval
getInterval (Pitch (fo, fpc)) (Pitch (to, tpc)) = 
  let (oct', pc') = divMod (tpc - fpc) 12 in ((12 * (to - fo + oct')) + pc')
  
-- | Changes a 'Pitch' with a particular 'Interval'
changePitch :: Pitch -> Interval -> Pitch
changePitch (Pitch (oct, pc)) i = let (octi, pci) = divMod  i         12 
                                      (oct', pc') = divMod (pc + pci) 12    
                                  in Pitch (oct + octi + oct', pc')

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
toMidiNr (Pitch (oct, p)) = fromIntegral (((oct + 5) * 12) + p)

-- | Converts a MIDI note number into an octave and a pitch class, a.k.a 'Pitch'
toPitch :: Word8 -> Pitch
toPitch = Pitch . midiNrToPitch where
                           
  midiNrToPitch :: Word8 -> (Int, Int)
  midiNrToPitch p | p < 0     = invalidMidiNumberError p 
                  | p > 127   = invalidMidiNumberError p 
                  | otherwise = first (+ (-5)) (fromIntegral p `divMod` 12)


invalidMidiNumberError :: Show a => a -> b
invalidMidiNumberError w = error ("invalid MIDI note number" ++ show w)

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
  