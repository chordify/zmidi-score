{-# OPTIONS_GHC -Wall                #-}
{-# LANGUAGE DeriveFunctor           #-}
module ZMidiBasic where



import ZMidi.Core ( readMidi, MidiFile (..), MidiEvent (..)
                  , MidiVoiceEvent (..), MidiMetaEvent (..)
                  , MidiMessage, MidiTrack (..), MidiHeader (..) 
                  , MidiScaleType (..), MidiTimeDivision (..)
                  , printMidi
                  )

import Control.Monad.State (State, modify, get, gets, evalState)
import Control.Monad (mapAndUnzipM)
import Control.Arrow (first, second)
import Data.Word (Word8)
import Data.Int  (Int8)
import Data.Char (toLower)
import Data.Maybe (catMaybes)
import Data.Function (on)
import Data.List (partition, sortBy, intersperse)
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do arg <- getArgs
          case arg of
            [f] -> readMidiFile f
            _   -> putStrLn "usage: MidiCSV <filename> "


readMidiFile :: FilePath -> IO ()
readMidiFile f = do mf <- readMidi f
                    case mf of
                      Left  err -> print err
                      Right mid -> do printMidi mid
                                      putStrLn . showMidiScore . midiFileToMidiScore $ mid 
                                   -- print . midiFileToMidiScore $ mid 

                                   
--------------------------------------------------------------------------------                                   
-- MIDI data representation
--------------------------------------------------------------------------------

data MidiScore  = MidiScore     { getKey     :: Key
                                , getTimeSig :: TimeSig
                                , devision   :: Time                                
                                , getTracks  :: [Voice]
                                } deriving (Eq, Ord, Show)
                     
data Key        = Key           { keyRoot    :: Int8
                                , keyMode    :: MidiScaleType
                                } deriving (Eq, Ord, Show)
               
newtype TimeSig = TimeSig   (Int, Int) deriving (Eq, Ord, Show)

type Voice      = [Timed ScoreEvent]

type Channel    = Int
type Pitch      = Word8
type Velocity   = Word8
type Time       = Int

-- perhaps add duration??
data Timed a    = Timed         { onset       :: Time 
                                , getEvent    :: a
                                } deriving (Functor, Eq, Ord, Show)

data ScoreEvent = NoteEvent     { channel     :: Channel
                                , pitch       :: Pitch
                                , velocity    :: Velocity
                                , duration    :: Time
                                } 
                | KeyChange     { keyChange   :: Key
                                } 
                | TimeSigChange { tsChange    :: TimeSig
                                } deriving (Eq, Ord, Show)

noKey :: Key
noKey = Key 255 MAJOR

noTS :: TimeSig
noTS = TimeSig (-1,-1)

--------------------------------------------------------------------------------                                   
-- Printing MidiScores
--------------------------------------------------------------------------------

-- Show a MidiScore in a readable way
showMidiScore :: MidiScore -> String
showMidiScore (MidiScore k ts tpb vs) = "Key: "      ++ showKey k ++ 
                                        "\nMeter: "  ++ showTimeSig ts ++
                                        "\nTicks per Beat: "  ++ show tpb ++
                                        "\nNotes:\n" ++ showVoices tpb vs

showVoices :: Time -> [Voice] -> String
showVoices d a = concat . intersperse "\n" $ evalState (showTimeSlice a) 0 where
  
  -- shows a the sounding notes at a specific time 
  showTimeSlice :: [Voice] -> State Time [String]
  showTimeSlice vs = 
    do (str, vs') <- mapAndUnzipM showVoiceAtTime vs
       case noMoreNotesToShow vs' of        -- the stopping condition
         True  -> return []
         False -> do t <- get
                     --  update the clock
                     modify (+ (d `div` 4)) -- 32nth notes are the minimum length
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
                    False -> return (showScoreEvent . getEvent $ v, x)
         False ->            return ("  "                         , x)

  hasStarted, hasEnded :: Time -> Timed ScoreEvent -> Bool
  -- returns true if the Timed ScoreEvent is has started to sound at Time t
  hasStarted t tse = t >= onset tse 
  -- returns true if the Timed ScoreEvent is still sounding at Time t
  hasEnded t (Timed ons se) = ons + duration se  <  t


showScoreEvent :: ScoreEvent -> String
showScoreEvent (NoteEvent _c p _v _d) = show p
showScoreEvent (TimeSigChange ts)     = "Meter: " ++ showTimeSig ts
showScoreEvent (KeyChange k)          = "Key: " ++ showKey k

showTimeSig :: TimeSig -> String
showTimeSig (TimeSig (n,d)) = show n ++ '/' : show d

showKey :: Key -> String
showKey (Key rt m) = showRoot rt ++ ' ' : (map toLower . show $ m) where
      
    showRoot :: Int8 -> String
    showRoot r = let r' = fromIntegral r in case compare (signum r) 0 of
      LT -> replicate r' 'b'
      EQ -> "0"
      GT -> replicate r' '#'

--------------------------------------------------------------------------------                                   
-- Converting a MidiFile
--------------------------------------------------------------------------------

midiFileToMidiScore :: MidiFile -> MidiScore
midiFileToMidiScore mf = MidiScore (selectKey meta) 
                                   (selectTS  meta) 
                                   getDivision
                                   (filter (not . null) trks) where
                         
  (trks, meta) = second concat . -- merge all meta data into one list
                 -- separate meta data from note data
                 unzip . map (partition isNoteEvent . midiTrackToVoice)
                       . mf_tracks $ mf -- get midi tracks
  
  getDivision :: Time
  getDivision= case time_division . mf_header $ mf of
                 (FPS _ ) -> error "unquantised midifile"
                 (TPB b ) -> fromIntegral b
  
  selectKey :: [Timed ScoreEvent] -> Key
  selectKey ses = case filter isKeyChange ses of 
    []  -> noKey
    [k] -> keyChange . getEvent $ k
    ks  -> keyChange . getEvent . head . sortBy (compare `on` onset) $ ks  

  selectTS :: [Timed ScoreEvent] ->TimeSig
  selectTS ses = case filter isTimeSig ses of
    []  -> noTS
    [t] -> tsChange . getEvent $ t
    ts  -> tsChange . getEvent . head. sortBy (compare `on` onset) $ ts
    
  isTimeSig :: Timed ScoreEvent -> Bool
  isTimeSig (Timed _ (TimeSigChange _ )) = True
  isTimeSig _                            = False

  isKeyChange :: Timed ScoreEvent -> Bool
  isKeyChange (Timed _ (KeyChange   _ )) = True
  isKeyChange _                          = False
  
  isNoteEvent :: Timed ScoreEvent -> Bool
  isNoteEvent (Timed _ (NoteEvent _ _ _ _ )) = True
  isNoteEvent _                              = False
  
-- Transforms a 'MidiTrack' into a 'Voice'
midiTrackToVoice :: MidiTrack -> Voice
midiTrackToVoice m = 
  catMaybes $ evalState (mapM toScoreEvent . getTrackMessages $ m) (0, []) where
    
    -- Transforms a 'MidiMessage' into a 'ScoreEvent'
    toScoreEvent :: MidiMessage -> State MidiState (Maybe (Timed ScoreEvent))
    toScoreEvent mm@(dt, me) = do modify (stateTimeWith (+ (fromIntegral dt)))
                                  case me of
                                    (VoiceEvent _) -> voiceEvent mm
                                    (MetaEvent  _) -> metaEvent  mm
                                    _              -> return Nothing
                            
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
                                (TimeSig (fromIntegral num, 2 ^ den))
            Just (KeySignature root scale)
              -> return . Just $ Timed t (KeyChange (Key root scale ))
            _ -> return Nothing
    
    -- Some utilities:
    -- Given a note-on and a note off event, creates a new MidiNote ScoreEvent
    toMidiNote :: Word8 -> Pitch -> State MidiState (Maybe (Timed ScoreEvent))
    toMidiNote c p = 
      do ms <- gets snd
         let (x, (ons, noteOn) : y) = span (not . isNoteOnMatch c p) ms
         modify (setMessages (x ++ y))
         t  <- gets fst
         -- N.B. the delta time in the note on has been replace by an absolute
         -- timestamp
         return . Just . Timed ons $ NoteEvent (fromIntegral c)     p 
                                               (getVelocity noteOn) (t - ons)
    
    -- returns True if the NoteOn MidiMessage maches a Channel and Pitch
    isNoteOnMatch :: Word8 -> Pitch -> (Time, MidiVoiceEvent) -> Bool
    isNoteOnMatch offc offp (_t, NoteOn onc onp _v) = onc == offc && onp == offp
    isNoteOnMatch _c   _p    _                      = False

    -- Given a MidiMessage, returns the MidiVoiceEvent and Nothing if it 
    -- contains something else.
    getVoiceEvent :: MidiMessage -> Maybe MidiVoiceEvent
    getVoiceEvent (_t, (VoiceEvent e)) = Just e
    getVoiceEvent _                    = Nothing
      
    -- Given a MidiMessage, returns the MidiMetaEvent and Nothing if it 
    -- contains something else.
    getMetaEvent :: MidiMessage -> Maybe MidiMetaEvent
    getMetaEvent (_t, (MetaEvent e)) = Just e
    getMetaEvent _                   = Nothing

    getVelocity :: MidiVoiceEvent -> Velocity
    getVelocity (NoteOn  _ _ v) = v
    getVelocity (NoteOff _ _ v) = v
    getVelocity _               = error "not a noteOn or a noteOff event"
    
-- we create a small state datatype for storing some information obtained from
-- the midi file, that is needed again at a later stage, such as note-on events.
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

