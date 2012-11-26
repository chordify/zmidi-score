{-# OPTIONS_GHC -Wall                #-}
module ZMidiBasic where



import ZMidi.Core ( readMidi, MidiFile (..), MidiEvent (..)
                  , MidiVoiceEvent (..), MidiMetaEvent (..)
                  , MidiMessage, MidiTrack (..)
                  , MidiScaleType (..))

import Control.Monad.State (State, modify, gets, evalState)
import Control.Arrow (first, second)
import Data.Word (Word8)
import Data.Int  (Int8)
import Data.Char (toLower)
import Data.Maybe (catMaybes)
import Data.Function (on)
import Data.List (partition, sortBy)
import System.Environment (getArgs)

main :: IO ()
main = do arg <- getArgs
          case arg of
            [f] -> readMidiFile f
            _   -> putStrLn "usage: MidiCSV <filename> "


readMidiFile :: FilePath -> IO ()
readMidiFile f = do mf <- readMidi f
                    case mf of
                      Left  err -> print err
                      Right mid -> print . midiFileToMidiScore $ mid 
                                   -- printMidi mid
                                   -- putStr . concat . intersperse "\n" 
                                   -- $ evalState (mapM showTrack (mf_tracks mid)) 1
                                   
--------------------------------------------------------------------------------                                   
-- MIDI data representation
--------------------------------------------------------------------------------

data MidiScore  = MidiScore     { getKey     :: Key
                                , getTimeSig :: TimeSig
                                , getTracks  :: [Voice]
                                } deriving (Eq, Ord, Show)
                                
data Key        = Key           { keyRoot    :: Int8
                                , keyMode    :: MidiScaleType
                                } deriving (Eq, Ord, Show)
               
newtype TimeSig = TimeSig   (Int, Int) deriving (Eq, Ord, Show)

type Voice      = [ScoreEvent]

type Channel    = Word8
type Pitch      = Word8
type Velocity   = Word8
type Time       = Int

-- Perhaps rename to ScoreEvent??
data ScoreEvent = NoteEvent     { channel     :: Channel
                                , pitch       :: Pitch
                                , velocity    :: Velocity
                                , duration    :: Time
                                , noteTime    :: Time
                                } 
                | KeyChange     { keyChange   :: Key
                                , keyTime     :: Time
                                } 
                | TimeSigChange { tsChange    :: TimeSig
                                , tsTime      :: Time
                                } deriving (Eq, Ord, Show)

noKey :: Key
noKey = Key 255 MAJOR

noTS :: TimeSig
noTS = TimeSig (-1,-1)

--------------------------------------------------------------------------------                                   
-- Printing MidiScores
--------------------------------------------------------------------------------

showMidiScore :: MidiScore -> String
showMidiScore ms = undefined

showVoices :: [Voice] -> String
showVoices = undefined

type ShowSt = (Time, [ScoreEvent])

increaseTime :: ShowSt -> ShowSt
increaseTime = first (+ 128)

showVcs :: [Voice] -> State ShowSt String
showVcs = undefined

getActive :: Voice -> State ShowSt (Maybe ScoreEvent)
getActive []     = return Nothing
getActive (v:_s) = do t <- gets fst
                      case compare (noteTime v) t of
                        LT -> error ("getActive: found a past event " ++ show v)
                        EQ -> return . Just $ v
                        GT -> return . Just $ v

showScoreEvent :: ScoreEvent -> String
showScoreEvent (NoteEvent _c p _v _d _o) = show p
showScoreEvent (TimeSigChange (TimeSig (n,d)) _t) =
  "Meter: " ++ show n ++ '/' : show d
showScoreEvent (KeyChange (Key rt m) _t) = 
  "Key: " ++ showRoot rt ++ ' ' : (map toLower . show $ m) where
      
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
                                   (filter (not . null) trks) where
                         
  (trks, meta) = second concat . -- merge all meta data into one list
                 -- separate meta data from note data
                 unzip . map (partition isNoteEvent . midiTrackToVoice)
                       . mf_tracks $ mf -- get midi tracks

  selectKey :: [ScoreEvent] -> Key
  selectKey ses = case filter isKeyChange ses of 
    []  -> noKey
    [k] -> keyChange k
    ks  -> keyChange . head . sortBy (compare `on` keyTime) $ ks  
    -- _   -> error "not a key change" -- cannot happen

  selectTS :: [ScoreEvent] ->TimeSig
  selectTS ses = case filter isTimeSig ses of
    []  -> noTS
    [t] -> tsChange t
    ts  -> tsChange . head. sortBy (compare `on` tsTime) $ ts
    -- _   -> error "not a time signature change" -- cannot happen
    
  isTimeSig :: ScoreEvent -> Bool
  isTimeSig (TimeSigChange _ _) = True
  isTimeSig _                   = False

  isKeyChange :: ScoreEvent -> Bool
  isKeyChange (KeyChange _ _) = True
  isKeyChange _               = False
  
  isNoteEvent :: ScoreEvent -> Bool
  isNoteEvent (NoteEvent _ _ _ _ _) = True
  isNoteEvent _                     = False

-- Transforms a 'MidiTrack' into a 'Voice'
midiTrackToVoice :: MidiTrack -> Voice
midiTrackToVoice m = 
  catMaybes $ evalState (mapM toScoreEvent . getTrackMessages $ m) (0, []) where
    
    -- Transforms a 'MidiMessage' into a 'ScoreEvent'
    toScoreEvent :: MidiMessage -> State MidiState (Maybe ScoreEvent)
    toScoreEvent mm@(dt, me) = do modify (stateTimeWith (+ (fromIntegral dt)))
                                  case me of
                                    (VoiceEvent _) -> voiceEvent mm
                                    (MetaEvent  _) -> metaEvent  mm
                                    _              -> return Nothing
                            
    -- Transforms a 'MidiMessage' containing a 'VoiceEvent' into a 'ScoreEvent'
    voiceEvent :: MidiMessage -> State MidiState (Maybe ScoreEvent)
    voiceEvent mm = case getVoiceEvent mm of
      Just (NoteOff chn  ptch _vel) 
         -> do ms <- gets snd
               let (x, noteOn : y) = span (not . isNoteOnMatch chn ptch) ms
               modify (setMessages (x ++ y))
               fmap Just $ toMidiNote noteOn 
      Just n@(NoteOn _chn _ptch _vel)
         -> do  t <- gets fst
                -- replace the deltaTime in the noteOn event by the absolute
                -- time as stored in the MidiState
                modify . addMessage $ (t, n)
                return Nothing
      _  ->     return Nothing    -- ignore NoteAftertouch, Controller,
                                  -- ProgramChange, ChanAftertouch, PitchBend

    -- Transforms a 'MidiMessage' containing a 'MetaEvent' into a 'ScoreEvent'
    metaEvent :: MidiMessage -> State MidiState (Maybe ScoreEvent)
    metaEvent mm = 
      do t <- gets fst
         case getMetaEvent mm of
            -- Just (EndOfTrack)
            Just (TimeSignature num den _frac _subfr) 
              -> return . Just $ TimeSigChange 
                                (TimeSig (fromIntegral num, fromIntegral den)) t
            Just (KeySignature root scale)
              -> return . Just $ KeyChange (Key root scale ) t
            _ -> return Nothing
    
    -- Some utilities:
    -- Given a note-on and a note off event, creates a new MidiNote ScoreEvent
    toMidiNote :: (Time, MidiVoiceEvent) -> State MidiState ScoreEvent
    toMidiNote ( ons, NoteOn c p v ) = 
      do t <- gets fst
         -- N.B. the delta time in the note on has been replace by an absolute
         -- timestamp
         return $ NoteEvent c p v (t - ons) ons
    toMidiNote _  = error "toMidiNote: not a noteOn" -- impossible
    
    -- returns True if the NoteOn MidiMessage maches a Channel and Pitch
    isNoteOnMatch :: Channel -> Pitch -> (Time, MidiVoiceEvent) -> Bool
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

