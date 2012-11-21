{-# OPTIONS_GHC -Wall                #-}
module ZMidiBasic where



import ZMidi.Core ( readMidi, MidiFile (..), MidiEvent (..), printMidi
                  , MidiVoiceEvent (..) -- MidiMetaEvent (..), MidiDataEvent (..)
                  , MidiMessage, MidiTrack (..)
                  , MidiScaleType)

import Control.Monad.State (State, modify, get, gets, evalState)
import Control.Arrow (first, second)
import Data.Word (Word8)
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

-- stateTime :: MidiState -> Time
-- stateTime = fst . midiState 

-- stateMessages :: MidiState -> [MidiMessage]
-- stateMessages = snd . midiState

setMessages :: [MidiMessage] -> MidiState -> MidiState
setMessages ms = second (const ms)

stateTimeWith :: (Time -> Time) -> MidiState -> MidiState
stateTimeWith f = first f 

midiTrackToVoice :: MidiTrack -> Voice
midiTrackToVoice m = evalState (mapM toNoteEvent . getTrackMessages $ m) (0, [])
  
toNoteEvent :: MidiMessage -> State MidiState NoteEvent
toNoteEvent (dt, me) = do modify (stateTimeWith (+ (fromIntegral dt)))
                          t <- get
                          return undefined
                            
midiEvent :: MidiEvent -> State MidiState (Maybe NoteEvent)
midiEvent (VoiceEvent e) = fmap Just (voiceEvent e)
-- midiEvent (MetaEvent e)  = fmap Just (metaEvent e)
midiEvent _              = return Nothing

voiceEvent :: MidiVoiceEvent -> State MidiState NoteEvent
voiceEvent (NoteOff chn ptch _vel) = 
  do ms <- gets snd
     let (x, noteOn : y) = span (not . isNoteOnMatch chn ptch) ms
     modify (setMessages (x ++ y))
     return undefined
-- voiceEvent _                       = 
                     
                     
                     

isNoteOnMatch :: Channel -> Pitch -> MidiMessage -> Bool
isNoteOnMatch offc offp (_t, (VoiceEvent (NoteOn onc onp _onv))) = 
   onc == offc && onp == offp
isNoteOnMatch _    _    _                                        = False 

-- -- findAndRemove :: [MidiVoiceEvent] -> MidiVoiceEvent -> (Midi
-- matchAndUpdateNoteOns :: MidiVoiceEvent -> State 

