{-# OPTIONS_GHC -Wall                #-}
{-# LANGUAGE DeriveFunctor           #-}
module ZMidiBasic ( -- * Score representation of a MidiFile
                    MidiScore (..)
                  , Key (..)
                  , TimeSig (..)
                  , Voice 
                  , Channel
                  , Pitch
                  , Velocity
                  , Timed (..)
                  , ScoreEvent (..)
                  -- * Transformation
                  , midiFileToMidiScore
                  -- * Quantisation
                  , quantise
                  , buildTickMap
                  , getMinDur
                  , gcIOId
                  -- * Utilities
                  , nrOfNotes
                  , toIOIs
                  -- * Showing
                  , showMidiScore
                  , showVoices
                  ) where

import ZMidi.Core ( MidiFile (..), MidiEvent (..)
                  , MidiVoiceEvent (..), MidiMetaEvent (..)
                  , MidiMessage, MidiTrack (..), MidiHeader (..) 
                  , MidiScaleType (..), MidiTimeDivision (..)
                  )

import Control.Monad.State ( State, modify, get, gets, evalState, execState )
import Control.Monad       ( mapAndUnzipM )
import Control.Arrow       ( first, second )
import Data.Word           ( Word8 )
import Data.Int            ( Int8 )
import Data.Char           ( toLower )
import Data.Maybe          ( catMaybes )
import Data.Ord            ( comparing )
import Data.List           ( partition, intersperse, sortBy )
import Data.Foldable       ( foldrM )
import Data.IntMap.Lazy    ( empty, insertWith, IntMap, findMin, keys, delete )
import Text.Printf         ( printf )
                    
-- import Debug.Trace (trace)
                    
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
                                , division   :: Time
                                  -- | The minimum note length found.
                                , minDur     :: Time
                                  -- | The midi 'Voice's
                                , getVoices  :: [Voice]
                                } deriving (Eq, Ord, Show)
                     
data Key        = Key           { keyRoot    :: Int8
                                , keyMode    :: MidiScaleType
                                } 
                | NoKey           deriving (Eq, Ord)
               
-- | A 'TimeSig'nature has a fraction, e.g. 4/4, 3/4, or 6/8.
data TimeSig    = TimeSig       { numerator  :: Int 
                                , denomenator::Int
                                }
                | NoTimeSig       deriving (Eq, Ord)

-- | A 'Voice' is a list of 'ScoreEvent's that have time stamps.
type Voice      = [Timed ScoreEvent]

type Channel    = Int
type Pitch      = Word8
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
                                } deriving (Eq, Ord, Show)

type TickMap = IntMap Time

--------------------------------------------------------------------------------
-- Some ad-hoc show instances
--------------------------------------------------------------------------------

instance Show a => Show (Timed a) where
  show (Timed t a) = show a ++ " @ " ++ show t


instance Show TimeSig where
  show (TimeSig n d) = show n ++ '/' : show d
  show NoTimeSig     = "NoTimeSig"

instance Show Key where
  show NoKey      = "NoKey"
  show (Key rt m) = showRoot rt ++ ' ' : (map toLower . show $ m) where
      
    showRoot :: Int8 -> String
    showRoot i = let r = fromIntegral i in case compare r 0 of
      LT -> replicate (abs r) 'b'
      EQ -> "0"
      GT -> replicate r '#'
                                
--------------------------------------------------------------------------------                                   
-- Printing MidiScores
--------------------------------------------------------------------------------

-- Show a MidiScore in a readable way
showMidiScore :: MidiScore -> String
showMidiScore ms@(MidiScore k ts tpb st _vs) = "Key: "      ++ show k 
                                     ++ "\nMeter: "  ++ show ts
                                     ++ "\nTicks per Beat: "  ++ show tpb 
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
type GridUnit = Time

-- | Quantises a 'MidiScore' snapping all events to a 1/32 grid
quantise :: MidiScore -> MidiScore
quantise (MidiScore k ts dv _md vs) =  MidiScore k ts dv md' vs' where

  vs' = map (map (snapEvent (dv `div` 8))) vs -- snap all events to a grid
  md' = getMinDur . buildTickMap $ vs'-- the minimum duration might have changed
          
  snapEvent :: GridUnit -> Timed ScoreEvent -> Timed ScoreEvent
  snapEvent g (Timed ons dat) = case dat of
    (NoteEvent c p v d) -> Timed (snap g ons) (NoteEvent c p v (snap g d))
    _                   -> Timed (snap g ons) dat

snap :: GridUnit -> Time -> Time
snap g t | m == 0  = t               -- score event is on the grid
         | m >  0  = if (g - m) >= m -- score event is off the grid
                     -- and closer to the past grid point
                     then    d  * g  -- snap backwards
                     -- or closer to the next grid point
                     else (1+d) * g  -- snap forwards
         | otherwise = error "Negative time stamp found"
             where (d,m) = t `divMod` g

gcIOId :: TickMap -> Time
gcIOId tm = case keys $ tm of
  [] -> 0
  l  -> foldr1 gcd l

-- | Return the IOI of the event that has the shortest IOI
getMinDur :: TickMap -> Time
getMinDur tm = case fst (findMin tm) of
                 0 -> fst . findMin . delete 0 $ tm
                 n -> n

buildTickMap :: [Voice] -> TickMap
buildTickMap = foldr oneVoice empty where

  oneVoice :: Voice -> TickMap -> TickMap
  oneVoice vs tm = foldr step tm (toIOIs vs)

  step :: Time -> TickMap -> TickMap
  step se tm = insertWith succIfExists se 0 tm 
  
  succIfExists :: a -> Int -> Int
  succIfExists _ old = succ old

  
--------------------------------------------------------------------------------
-- Converting a MidiFile
--------------------------------------------------------------------------------

-- | Transforms a 'MidiFile' into a 'MidiScore'
midiFileToMidiScore :: MidiFile -> MidiScore
midiFileToMidiScore mf = MidiScore (selectKey meta) 
                                   (selectTS  meta) 
                                   getDivision
                                   (getMinDur . buildTickMap $ trks)
                                   (filter (not . null) trks) where
                         
  (trks, meta) = second concat . -- merge all meta data into one list
                 -- separate meta data from note data
                 unzip . map (partition isNoteEvent . midiTrackToVoice)
                       . mf_tracks $ mf -- get midi tracks
  
  getDivision :: Time
  getDivision= case time_division . mf_header $ mf of
                 (FPS _ ) -> error "no devision found"
                 (TPB b ) -> fromIntegral b
  
  selectKey :: [Timed ScoreEvent] -> [Timed Key]
  selectKey ses = case filter isKeyChange ses of 
    [] -> [Timed 0 NoKey]
    k  -> map (fmap keyChange) k

  selectTS :: [Timed ScoreEvent] -> [Timed TimeSig]
  selectTS ses = case filter isTimeSig ses of
    [] -> [Timed 0 NoTimeSig]
    t  -> map (fmap tsChange) t
  
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
            -- Just (EndOfTrack)
            Just (TimeSignature num den _frac _subfr) 
              -> return . Just . Timed t $ TimeSigChange 
                                (TimeSig (fromIntegral num) (2 ^ den))
            Just (KeySignature root scale)
              -> return . Just $ Timed t (KeyChange (Key root scale ))
            _ -> return Nothing
    
    -- Some utilities:
    -- Given a note-on and a note off event, creates a new MidiNote ScoreEvent
    toMidiNote :: Word8 -> Pitch -> State MidiState (Maybe (Timed ScoreEvent))
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
                 return . Just . Timed ons $ NoteEvent (fromIntegral c) p 
                                             (getVelocity noteOn) (t - ons)
            
    -- returns True if the NoteOn MidiMessage maches a Channel and Pitch
    isNoteOnMatch :: Word8 -> Pitch -> (Time, MidiVoiceEvent) -> Bool
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

-- | Returns True if the 'ScoreEvent' is a 'NoteEvent'
isNoteEvent :: Timed ScoreEvent -> Bool
isNoteEvent (Timed _ (NoteEvent _ _ _ _ )) = True
isNoteEvent _                              = False

-- | Transforms a 'Voice' into a list of Inter Onset Intervals (IOIs)
toIOIs :: Voice -> [Time]
toIOIs v = execState (foldrM step [] v) [] where

  step :: Timed ScoreEvent -> [Timed ScoreEvent] 
       -> State [Time] [Timed ScoreEvent]
  step t []         = return [t]
  step t ts@(h : _) = do modify ((onset h - onset t) :)
                         return (t : ts)

