{-# OPTIONS_GHC -Wall                #-}
{-# LANGUAGE DeriveFunctor           #-}
{-# LANGUAGE ScopedTypeVariables     #-}
module ZMidiBasic ( -- * Score representation of a MidiFile
                    MidiScore (..)
                  , Key (..)
                  , TimeSig (..)
                  , Voice 
                  , Channel
                  , Pitch (..)
                  , PitchClass (..)
                  , Interval
                  , Velocity
                  , Timed (..)
                  , Time
                  , ScoreEvent (..)
                  , Deviation
                  -- * Transformation
                  , midiFileToMidiScore
                  , midiScoreToMidiFile
                  -- * Quantisation
                  , quantise
                  , quantiseDev
                  , quantiseVoice
                  , removeOverlap
                  , ShortestNote (..)
                  , GridUnit
                  , toGridUnit
                  , buildTickMap
                  , gcIOId
                  -- * Utilities
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
                  , getMinGridSize
                  , getBeatInBar
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
                           , MidiMessage, MidiTrack (..), MidiHeader (..) 
                           , MidiScaleType (..), MidiTimeDivision (..)
                           , MidiRunningStatus (..), DeltaTime
                           )
import Control.Monad.State ( State, modify, get, gets, put
                           , evalState, execState )
import Control.Monad       ( mapAndUnzipM )
import Control.Arrow       ( first, second, (***) )
import Data.Ratio          ( (%), Ratio )
import Data.Word           ( Word8 )
import Data.Int            ( Int8 )
import Data.Char           ( toLower )
import Data.Maybe          ( catMaybes, mapMaybe, isJust )
import Data.Ord            ( comparing )
import Data.List           ( partition, intersperse, sortBy, sort, nub
                           , genericLength, find )
import qualified Data.List.Ordered as Sort ( nub )
import Data.Foldable       ( foldrM )
import Data.IntMap.Lazy    ( insertWith, IntMap, keys )
import qualified Data.IntMap.Lazy  as M    ( empty )
import Text.Printf         ( printf )
import GHC.Float           ( integerLogBase )

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
                                , ticksPerBeat :: Time  
                                  -- | The kind of midi file that created this score
                                , midiFormat :: MidiFormat
                                  -- | The microseconds per quarter note
                                , tempo      :: [Timed Time]                                
                                  -- | The minimum note length found.
                                , minDur     :: Time
                                  -- | The midi 'Voice's
                                , getVoices  :: [Voice]
                                } deriving (Eq, Show)
                     
data Key        = Key           { keyRoot    :: Int8
                                , keyMode    :: MidiScaleType
                                } 
                | NoKey           deriving (Eq, Ord)
               
-- | A 'TimeSig'nature has a fraction, e.g. 4/4, 3/4, or 6/8.
data TimeSig    = TimeSig       { numerator  :: Int 
                                , denomenator:: Int
                                , metronome  :: Word8
                                , nr32ndNotes:: Word8
                                }
                | NoTimeSig

-- Note: we could consider adding a Voice label, like there is a track label
-- | A 'Voice' is a list of 'ScoreEvent's that have time stamps.
type Voice      = [Timed ScoreEvent]

type Channel    = Word8
-- TODO: better changed to Pitch (Int, PitchClass)
newtype Pitch   = Pitch    (Int, Int) deriving (Eq, Ord) -- (Octave, Pitch class)
type    Interval= Int

newtype PitchClass = PitchClass Int deriving (Eq, Ord) 

type Velocity   = Word8
type Time       = Int

-- perhaps add duration??
data Timed a    = Timed         { onset       :: Time 
                                , getEvent    :: a
                                } deriving (Functor, Eq, Ord)
                                
data ScoreEvent = NoteEvent     { channel     :: Channel
                                , pitch       :: Pitch
                                , velocity    :: Velocity
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
                                } deriving (Eq, Ord, Show)

type TickMap = IntMap Time

--------------------------------------------------------------------------------
-- Some ad-hoc show instances
--------------------------------------------------------------------------------

instance Show a => Show (Timed a) where
  show (Timed t a) = show a ++ " @ " ++ show t

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
  show (Pitch (oct, p)) = show oct ++ showPitch p where

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

-- instance Ord Pitch where
  -- compare (Pitch (octA, pcA)) 
          -- (Pitch (octB, pcB)) = case compare octA octB of
                                  -- EQ   -> compare pcA pcB
                                  -- golt -> golt -- greater or smaller
      
--------------------------------------------------------------------------------
-- Printing MidiScores
--------------------------------------------------------------------------------

-- Show a MidiScore in a readable way
showMidiScore :: MidiScore -> String
showMidiScore ms@(MidiScore k ts tpb mf tp st _vs) = "Key: "      ++ show k 
                                     ++ "\nMeter: "  ++ show ts
                                     ++ "\nTicks per Beat: "  ++ show tpb 
                                     ++ "\nMidi format: " ++ show mf 
                                     ++ "\nTempo: "  ++ show tp 
                                     ++ "\nShortest tick: "   ++ show st
                                     ++ "\nNotes:\n" ++ showVoices ms

-- Shows the voices in a MidiScore in a readable way, but this function
-- only works for monophonic channels. TODO: fix
showVoices :: MidiScore -> String
showVoices ms = concat . intersperse "\n" 
              $ evalState (showTimeSlice . getVoices $ ms) 0 where
  
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
                     let out = concat $ intersperse "\t" (printf "%7d" t : str)                                    
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

-- | The 'ShortestNote' determines the minimal grid length of a quantised 
-- 'MidiScore', when quantised with 'quantise'.
data ShortestNote = Eighth | Sixteenth | ThirtySecond 
                  | FourtyEighth | SixtyFourth
                    deriving (Eq, Show)

type GridUnit = Time

type Deviation = Time
-- | Quantises a 'MidiScore' snapping all events to a 'ShortestNote' grid
-- similar to 'quantiseDev', but then discarding the cumulative deviation.
quantise :: ShortestNote -> MidiScore -> MidiScore
quantise s ms = let (ms', _, _) = quantiseDev s ms in ms'

-- | Quantises a 'MidiScore' snapping all events to a 'ShortestNote' grid. 
-- The absolute size of the grid is based on the 'GridUnit', which is the
-- 'ticksPerBeat' divided by the number of quantization bins per beat. Besides
-- the quantised 'MidiScore' and the 'GridUnit' also the cumulative deviation 
-- from the grid is returned.
quantiseDev :: ShortestNote -> MidiScore -> (MidiScore, Deviation, GridUnit)
quantiseDev sn (MidiScore k ts dv mf tp _md vs) =  
  (MidiScore k ts' dv mf tp md' vs', sum d, gu)  where
  
  gu = dv `div` toGridUnit sn
  -- snap all events to a grid, and remove possible overlaps 
  (vs',d) = unzip . map (quantiseVoice gu) $ vs 
  -- the minimum duration might have changed
  md' = gcIOId . buildTickMap $ vs'
  -- also align the time signatures to a grid
  ts' = map snapTS ts
  
  snapTS :: Timed TimeSig -> Timed TimeSig
  snapTS t = t {onset = fst . snap gu $ onset t}  

-- | quantises a 'Voice', and returns the cumulative 'Deviation'.
quantiseVoice :: GridUnit -> Voice -> (Voice, Deviation)
quantiseVoice gu = second sum . unzip . map snapEvent  where

  snapEvent :: Timed ScoreEvent -> (Timed ScoreEvent, Deviation)
  snapEvent (Timed ons dat) = let (t', d) = snap gu ons 
                              in case dat of
    (NoteEvent c p v l) -> (Timed t' (NoteEvent c p v (fst $ snap gu l)), d)
    _                   -> (Timed t' dat                                , d)
    
-- | snaps a 'Time' to a grid
snap :: GridUnit -> Time -> (Time, Deviation) 
snap g t | m == 0  = (t, 0)          -- score event is on the grid
         | m >  0  = if (g - m) >= m -- score event is off the grid
                     -- and closer to the past grid point
                     then let t' =    d  * g in (t', t  - t') -- snap backwards
                     -- or closer to the next grid point
                     else let t' = (1+d) * g in (t', t' - t ) -- snap forwards
         | otherwise = error "Negative time stamp found"
             where (d,m) = t `divMod` g

    
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

-- | takes the quantisation granularity parameter 'ShortestNote' and returns
-- a the 'Int' that the beat length should be divided by. The resulting 
-- value we name 'GridUnit'; it describes the minimal length of an event.
toGridUnit :: ShortestNote -> GridUnit
toGridUnit Eighth       = 2
toGridUnit Sixteenth    = 4
toGridUnit ThirtySecond = 8
toGridUnit FourtyEighth = 12
toGridUnit SixtyFourth  = 16

-- | The Inter Onset Interval that is the greatest common divider. It can be
--used to estimate whether a track is quantised or not.
gcIOId :: TickMap -> Time
gcIOId tm = case keys $ tm of
  [] -> 0
  l  -> foldr1 gcd l

-- | Return the IOI of the event that has the shortest IOI
-- getMinDur :: TickMap -> Time
-- getMinDur tm = case fst (findMin tm) of
                 -- 0 -> fst . findMin . delete 0 $ tm
                 -- n -> n

buildTickMap :: [Voice] -> TickMap
buildTickMap = foldr oneVoice M.empty where

  -- calculated all IOIs and order them per duration
  oneVoice :: Voice -> TickMap -> TickMap
  oneVoice [] tm = tm -- to account for offsets we add the first onset too
  oneVoice vs tm = step (onset . head $ vs) . foldr step tm $ (toIOIs vs)

  step :: Time -> TickMap -> TickMap
  step se tm = insertWith succIfExists se 0 tm 
  
  succIfExists :: a -> Int -> Int
  succIfExists _ old = succ old

-- printTickMap :: TickMap -> String
-- printTickMap tm = "tickmap:\n" ++ (concatMap showTick . toAscList $ tm) where
  
  -- showTick :: (Int, Time) -> String
  -- showTick (i, t) = show i ++ ": " ++ show t ++ "\n"
  
--------------------------------------------------------------------------------
-- Converting a MidiFile
--------------------------------------------------------------------------------

-- | Transforms a 'MidiFile' into a 'MidiScore'
midiFileToMidiScore :: MidiFile -> MidiScore
midiFileToMidiScore mf = MidiScore (select isKeyChange keyChange NoKey meta) 
                                   -- (map (updTimeSig tpb) . nub $ select isTimeSig tsChange NoTimeSig meta) 
                                   (nub $ select isTimeSig tsChange NoTimeSig meta) 
                                   tpb
                                   (hdr_format  . mf_header $ mf)
                                   (select isTempoChange tempChange 500000 meta)
                                   (gcIOId . buildTickMap $ trks)
                                   (filter (not . null) trks) where
  
  tpb          = getDivision . mf_header $ mf
  (trks, meta) = second concat . -- merge all meta data into one list
                 -- separate meta data from note data
                 unzip . map (partition isNoteEvent . midiTrackToVoice)
                       . mf_tracks $ mf -- get midi tracks
                       
  -- Given a filter function, a transformation function and a default value
  -- this function transforms a list of score events, which can possibly be
  -- empty, into a Timed meta information
  select :: (Timed ScoreEvent -> Bool) -> (ScoreEvent -> a) -> a
         -> [Timed ScoreEvent] -> [Timed a]
  select f c def ses = case filter f ses of
    [] -> [Timed 0 def]
    t  -> map (fmap c) t
    
  -- tsEq Timed TimeSig -> Timed TimeSig -> Bool
  -- tsEq 
    
  -- Returns the time division of the MidiScore, which is the length of a
  -- quarter note
  getDivision :: MidiHeader -> Time
  getDivision hd = case time_division hd of
                    (FPS _ ) -> error "no division found"
                    (TPB b ) -> fromIntegral b
    
    
-- Transforms a 'MidiTrack' into a 'Voice'
midiTrackToVoice :: MidiTrack -> Voice
midiTrackToVoice m = 
  sortBy (comparing onset) . catMaybes 
  $ evalState (mapM toScoreEvent . getTrackMessages $ m) (0, []) where
    
    -- Transforms a 'MidiMessage' into a 'ScoreEvent'
    toScoreEvent :: MidiMessage -> State MidiState (Maybe (Timed ScoreEvent))
    toScoreEvent mm@(dt, me) = do -- update the (absolute) midi clock
                                  modify (stateTimeWith (+ (fromIntegral dt)))
                                  case me of
                                    (VoiceEvent _ _) -> voiceEvent mm
                                    (MetaEvent  _)   -> metaEvent  mm
                                    _                -> return Nothing
                            
    -- Transforms a 'MidiMessage' containing a 'VoiceEvent' into a 'ScoreEvent'
    voiceEvent :: MidiMessage -> State MidiState (Maybe (Timed ScoreEvent))
    voiceEvent mm = case getVoiceEvent mm of
      Just   (NoteOff  chn  ptch _vel) -> toMidiNote chn ptch
      Just   (NoteOn   chn  ptch 0   ) -> toMidiNote chn ptch
      Just n@(NoteOn  _chn _ptch _vel)
         -> do  t <- gets fst
                -- replace the deltaTime in the noteOn event by the absolute
                -- time as stored in the MidiState
                modify . addMessage $ (t, n)
                return Nothing
      _  ->     return Nothing    -- ignore NoteAftertouch, Controller,
                                  -- ProgramChange, ChanAftertouch, PitchBend
    
    -- Transforms a 'MidiMessage' containing a 'MetaEvent' into a 'ScoreEvent'
    metaEvent :: MidiMessage -> State MidiState (Maybe (Timed ScoreEvent))
    metaEvent mm = 
      do t <- gets fst
         case getMetaEvent mm of
            -- tempo meta event
            Just (SetTempo tp)  
              -> return . Just . Timed t . TempoChange . fromIntegral $ tp
            -- timesignature meta event
            Just (TimeSignature num den metr n32n) 
              -> return . Just . Timed t $ TimeSigChange 
                                (TimeSig (fromIntegral num) (2 ^ den) metr n32n)
            -- key meta event
            Just (KeySignature root scale)
              -> return . Just $ Timed t (KeyChange (Key root scale ))
            _ -> return Nothing
    
    -- Some utilities:
    -- Given a note-on and a note off event, creates a new MidiNote ScoreEvent
    toMidiNote :: Word8 -> Word8 -> State MidiState (Maybe (Timed ScoreEvent))
    toMidiNote c p = 
      do ms <- gets snd
         case span (not . isNoteOnMatch c p) ms of
           -- TODO: perhaps we should store the missed note offs??
           (_, []               ) ->  -- trace ("no note on found for: " ++ show p
                                      --      ++  " on channel: "      ++ show c)
                                           (return Nothing)
           (x, (ons, noteOn) : y) -> 
              do modify (setMessages (x ++ y))
                 t  <- gets fst
                 -- N.B. the delta time in the note on has been replace 
                 -- by an absolute timestamp
                 return . Just . Timed ons $ NoteEvent (fromIntegral c) 
                                   (toPitch p) (getVelocity noteOn) (t - ons)
            
    -- returns True if the NoteOn MidiMessage maches a Channel and Pitch
    isNoteOnMatch :: Word8 -> Word8 -> (Time, MidiVoiceEvent) -> Bool
    isNoteOnMatch offc offp (_t, NoteOn onc onp _v) = onc == offc && onp == offp
    isNoteOnMatch _c   _p    _                      = False

    -- Given a MidiMessage, returns the MidiVoiceEvent and Nothing if it 
    -- contains something else. 
    getVoiceEvent :: MidiMessage -> Maybe MidiVoiceEvent
    getVoiceEvent (_t, (VoiceEvent _ e)) = Just e
    getVoiceEvent _                      = Nothing
      
    -- Given a MidiMessage, returns the MidiMetaEvent and Nothing if it 
    -- contains something else.
    getMetaEvent :: MidiMessage -> Maybe MidiMetaEvent
    getMetaEvent (_t, (MetaEvent e)) = Just e
    getMetaEvent _                   = Nothing

    getVelocity :: MidiVoiceEvent -> Velocity
    getVelocity (NoteOn  _ _ v) = v
    getVelocity (NoteOff _ _ v) = v
    getVelocity _               = error "not a noteOn or a noteOff event"
    
-- We define a stat to store the absolute midi clock 
-- and a list of note-on events.
type MidiState = (Time, [(Time, MidiVoiceEvent)])

-- We also define some accessor functions for our MidiState. 'setMessages' 
-- replaces the list with 'MidiMessages' with a new one.
setMessages :: [(Time, MidiVoiceEvent)] -> MidiState -> MidiState
setMessages ms = second (const ms)

-- adds a 'MidiMessage' to the list of 'MidiMessages'
addMessage :: (Time, MidiVoiceEvent) -> MidiState -> MidiState
addMessage m = second (m :)

-- applies a function to the 'Time' field in our 'MidiState'
stateTimeWith :: (Time -> Time) -> MidiState -> MidiState
stateTimeWith f = first f 

--------------------------------------------------------------------------------
-- Converting a MidiScore into a MidiFile
--------------------------------------------------------------------------------

-- | Transforms a 'MidiFile' into a 'MidiScore'
midiScoreToMidiFile :: MidiScore -> MidiFile
midiScoreToMidiFile (MidiScore ks ts dv mf tp _ vs) = MidiFile hdr trks where
    
    hdr  = MidiHeader mf (genericLength trks) (TPB . fromIntegral $ dv) 
    trks = metaToMidiEvent : map voiceToTrack vs -- the MidiTracks
    
    -- Takes the Key and TimeSig fields and tranforms them into 
    -- a MidiTrack containing only MetaEvents
    metaToMidiEvent :: MidiTrack
    metaToMidiEvent = mkMidiTrack MetaEvent (   mapMaybe keyToMidiEvent   ks 
                                             -- ++ (mapMaybe tsToMidiEvent . map (revTimeSig dv) $ ts)
                                             ++ (mapMaybe tsToMidiEvent ts)
                                             ++ map      tempoToMidiEvent tp)
      
    -- transforms a Key into a MetaEvent
    keyToMidiEvent :: Timed Key -> Maybe (Timed MidiMetaEvent)
    keyToMidiEvent (Timed _ NoKey    ) = Nothing
    keyToMidiEvent (Timed o (Key r s)) = Just $ Timed o (KeySignature r s)

    -- transforms a TimeSig into a MetaEvent
    tsToMidiEvent :: Timed TimeSig -> Maybe (Timed MidiMetaEvent)
    tsToMidiEvent (Timed _  NoTimeSig       ) = Nothing
    tsToMidiEvent (Timed o (TimeSig n d m n32)) = 
      Just $ Timed o (TimeSignature (fromIntegral n) 
                     (fromIntegral . integerLogBase 2 . fromIntegral $ d) m n32)
                     
    
    -- transforms a tempo into a MetaEvent
    tempoToMidiEvent :: Timed Time -> Timed MidiMetaEvent
    tempoToMidiEvent = fmap (SetTempo . fromIntegral)
    
    -- transforms a Voice into a MidiTrack
    voiceToTrack :: Voice -> MidiTrack
    voiceToTrack = mkMidiTrack (VoiceEvent RS_OFF) . concatMap toMidiNote 

    -- transforms a NoteEvent into a MidiVoiceEvent 
    toMidiNote :: Timed ScoreEvent -> [Timed MidiVoiceEvent] 
    toMidiNote (Timed o (NoteEvent c p v d)) = 
      let p' = toMidiNr p
      in [Timed o (NoteOn c p' v), Timed (o + d) (NoteOff c p' 0)]
    toMidiNote _ = error "noteEventToMidiNote: not a NoteEvent."  
     
    -- this is where the magic happens. A list of timed events is made relative
    -- such that timestamps denote the time between elements. We close 
    -- the track by appending a EndOfTrack marker with final time stamp.
    -- The track is sorted by midi tick
    mkMidiTrack :: forall a. Ord a => (a -> MidiEvent) -> [Timed a] -> MidiTrack
    mkMidiTrack f e = MidiTrack $ (trk ++ [(0, MetaEvent EndOfTrack)])
    
      where trk = evalState (mapM mkRelative . sort $ e) 0
      
            mkRelative :: Timed a -> State DeltaTime MidiMessage
            mkRelative (Timed o me) = do let o' = fromIntegral o
                                         t <- get ; put o'
                                         return (o' - t, f me) 



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

-- | Returns the minimal grid size of a 'MidiScore' if it has been quantised. 
-- This is the 'ticksPerBeat' divided by the number of quantisation bins.
-- N.B. this function does not check whether a file is quantised.
getMinGridSize :: ShortestNote -> MidiScore -> Time
getMinGridSize q ms = case ticksPerBeat ms `divMod` (toGridUnit q) of
                        (d,0) -> d
                        _     -> error "getMinGridSize: invalid quantisation"

getBeatInBar :: TimeSig -> Time -> Time -> (Time, Ratio Time)
getBeatInBar NoTimeSig _ _ = error "getBeatInBar applied to noTimeSig"
getBeatInBar (TimeSig _num _den _ _) tpb o = (succ *** (% tpb)) (o `divMod` tpb)
    
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
  