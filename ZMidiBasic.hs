{-# OPTIONS_GHC -Wall                #-}
module ZMidiBasic where



import ZMidi.Core ( readMidi, MidiFile (..), MidiEvent (..), printMidi
                  , MidiVoiceEvent (..) -- MidiMetaEvent (..), MidiDataEvent (..)
                  , MidiMessage, MidiTrack (..)
                  , MidiScaleType)

import Control.Monad.State (State, modify, gets, evalState)
import Control.Arrow (first, second)
import Data.Word (Word8)
import Data.Maybe (catMaybes)
-- import Data.List (intersperse)
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
                      Right mid -> printMidi mid
                                   -- putStr . concat . intersperse "\n" 
                                   -- $ evalState (mapM showTrack (mf_tracks mid)) 1
                                   
--------------------------------------------------------------------------------                                   
-- MIDI data representation
--------------------------------------------------------------------------------

data MidiScore  = MidiScore     { getKey     :: Key
                                , getTimeSig :: TimeSig
                                , getTracks  :: [Voice]
                                } deriving (Eq, Ord, Show)
                                
data Key        = Key           { keyRoot    :: Pitch
                                , keyMode    :: MidiScaleType
                                } deriving (Eq, Ord, Show)
               
newtype TimeSig = TimeSig   (Int, Int) deriving (Eq, Ord, Show)

type Voice      = [NoteEvent]

type Channel    = Word8
type Pitch      = Word8
type Velocity   = Word8
type Time       = Int

-- Perhaps rename to ScoreEvent??
data NoteEvent  = NoteEvent     { channel     :: Channel
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
                           
-- type Time = Int
-- showMess :: MidiMessage -> State Time String
-- showMess (dt, me) = do modify (+ (fromIntegral dt))
                       -- t <- get
                       -- return . concat $ intersperse ", " (show t : ppEvent me)
                           
-- toMidiScore :: MidiFile -> MidiScore
-- toMidiScore mf = evalState ()

type MidiState = (Time, [MidiMessage])

setMessages :: [MidiMessage] -> MidiState -> MidiState
setMessages ms = second (const ms)

addMessage :: MidiMessage -> MidiState -> MidiState
addMessage m = second (m :)

stateTimeWith :: (Time -> Time) -> MidiState -> MidiState
stateTimeWith f = first f 

-- getTime :: MidiState -> Time
-- getTime = fst

midiTrackToVoice :: MidiTrack -> Voice
midiTrackToVoice m = 
  catMaybes $ evalState (mapM toNoteEvent . getTrackMessages $ m) (0, [])
  
toNoteEvent :: MidiMessage -> State MidiState (Maybe NoteEvent)
toNoteEvent mm@(dt, me) = do modify (stateTimeWith (+ (fromIntegral dt)))
                             case me of
                               (VoiceEvent _) -> voiceEvent mm
                               -- (MetaEvent e)  -> fmap Just (metaEvent e)
                               _              -> return Nothing
                            

voiceEvent :: MidiMessage -> State MidiState (Maybe NoteEvent)
voiceEvent    (_t, (VoiceEvent (NoteOff chn  ptch _vel))) = 
  do ms <- gets snd
     let (x, noteOn : y) = span (not . isNoteOnMatch chn ptch) ms
     modify (setMessages (x ++ y))
     gets fst >>= return . Just . toMidiNote noteOn
voiceEvent on@(_t, (VoiceEvent (NoteOn _chn _ptch _vel))) = 
  do modify (addMessage on)
     return Nothing
voiceEvent _ = return Nothing -- ignore NoteAftertouch, Controller,
                              -- ProgramChange, ChanAftertouch, PitchBend
     

isNoteOnMatch :: Channel -> Pitch -> MidiMessage -> Bool
isNoteOnMatch offc offp (_t, (VoiceEvent (NoteOn onc onp _onv))) = 
   onc == offc && onp == offp
isNoteOnMatch _    _    _                                        = False 

toMidiNote :: MidiMessage -> Time -> NoteEvent
toMidiNote (on,(VoiceEvent (NoteOn c p v))) off = 
  let on' = fromIntegral on in  NoteEvent c p v (off - on') on'
toMidiNote _  _ = error "toMidiNote: not a noteOn" -- impossible

-- -- findAndRemove :: [MidiVoiceEvent] -> MidiVoiceEvent -> (Midi
-- matchAndUpdateNoteOns :: MidiVoiceEvent -> State 

