{-# OPTIONS_GHC -Wall                #-}
module ZMidiBasic where



import ZMidi.Core ( readMidi, MidiFile (..), MidiEvent (..)
                  , MidiVoiceEvent (..), MidiMetaEvent (..)
                  , MidiMessage, MidiTrack (..)
                  , MidiScaleType)

import Control.Monad.State (State, modify, gets, evalState)
import Control.Arrow (first, second)
import Data.Word (Word8)
import Data.Int  (Int8)
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
                      Right mid -> print . concatMap midiTrackToVoice . mf_tracks $ mid 
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
data ScoreEvent = ScoreEvent    { channel     :: Channel
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
  catMaybes $ evalState (mapM toScoreEvent . getTrackMessages $ m) (0, [])
  
toScoreEvent :: MidiMessage -> State MidiState (Maybe ScoreEvent)
toScoreEvent mm@(dt, me) = do modify (stateTimeWith (+ (fromIntegral dt)))
                              case me of
                                (VoiceEvent _) -> voiceEvent mm
                                (MetaEvent  _) -> metaEvent  mm
                                _              -> return Nothing
                            
                            
voiceEvent :: MidiMessage -> State MidiState (Maybe ScoreEvent)
voiceEvent mm = case getVoiceEvent mm of
  Just (NoteOff chn  ptch _vel) 
     -> do ms <- gets snd
           let (x, noteOn : y) = span (not . isNoteOnMatch chn ptch) ms
           modify (setMessages (x ++ y))
           fmap Just $ toMidiNote noteOn (fromIntegral . fst $ mm) -- offset
  Just (NoteOn _chn _ptch _vel)
     -> do  modify (addMessage mm)
            return Nothing
  _  ->     return Nothing    -- ignore NoteAftertouch, Controller,
                              -- ProgramChange, ChanAftertouch, PitchBend
     
isNoteOnMatch :: Channel -> Pitch -> MidiMessage -> Bool
isNoteOnMatch offc offp mm = case getVoiceEvent mm of
  Just (NoteOn onc onp _onv) -> onc == offc && onp == offp
  _                          -> False


toMidiNote :: MidiMessage -> Time -> State MidiState ScoreEvent
toMidiNote (on,(VoiceEvent (NoteOn c p v))) off = 
  do t <- gets fst
     return $ ScoreEvent c p v (off - fromIntegral on) t
toMidiNote _  _ = error "toMidiNote: not a noteOn" -- impossible

getVoiceEvent :: MidiMessage -> Maybe MidiVoiceEvent
getVoiceEvent (_t, (VoiceEvent e)) = Just e
getVoiceEvent _                    = Nothing

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
  
getMetaEvent :: MidiMessage -> Maybe MidiMetaEvent
getMetaEvent (_t, (MetaEvent e)) = Just e
getMetaEvent _                   = Nothing

-- -- findAndRemove :: [MidiVoiceEvent] -> MidiVoiceEvent -> (Midi
-- matchAndUpdateNoteOns :: MidiVoiceEvent -> State 

