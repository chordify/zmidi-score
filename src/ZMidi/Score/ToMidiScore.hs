{-# OPTIONS_GHC -Wall                   #-}
module ZMidi.Score.ToMidiScore ( midiFileToMidiScore ) where


import Data.Word           ( Word8 )
import Data.Maybe          ( catMaybes )
import Data.Ord            ( comparing )
import Data.List           ( partition, sortBy, nub )
import Control.Arrow       ( first, second )
import Control.Monad.State ( State, modify, gets, evalState )

import ZMidi.Core          ( MidiFile (..), MidiEvent (..)
                           , MidiVoiceEvent (..), MidiMetaEvent (..)
                           , MidiMessage, MidiTrack (..), MidiHeader (..) 
                           , MidiTimeDivision (..)
                           )

import ZMidi.Score.Datatypes hiding         ( TPB (..) )
import qualified ZMidi.Score.Datatypes as S ( TPB (..) )

--------------------------------------------------------------------------------
-- Converting a MidiFile
--------------------------------------------------------------------------------

-- | Transforms a 'MidiFile' into a 'MidiScore'
midiFileToMidiScore :: MidiFile -> MidiScore
midiFileToMidiScore mf = MidiScore (select isKeyChange keyChange NoKey meta) 
                                   -- (map (updTimeSig tpb) . nub $ select isTimeSig tsChange NoTimeSig meta) 
                                   (nub $ select isTimeSig tsChange NoTimeSig meta) 
                                   tb
                                   (hdr_format  . mf_header $ mf)
                                   (select isTempoChange tempChange 500000 meta)
                                   (gcIOId . buildTickMap $ trks)
                                   (filter (not . null) trks) where
  
  tb           = getDivision . mf_header $ mf
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
  getDivision :: MidiHeader -> S.TPB
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
getVelocity (NoteOn  _ _ v) = Velocity v
getVelocity (NoteOff _ _ v) = Velocity v
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
