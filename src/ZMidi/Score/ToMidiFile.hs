{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
-- |
-- Module      :  ZMidi.Score.Datatypes
-- Copyright   :  (c) 2012--2014, Utrecht University 
-- License     :  LGPL-3
--
-- Maintainer  :  W. Bas de Haas <w.b.dehaas@uu.nl>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: converting a 'MidiScore' into a ZMidi.Core.MidiFile 
-- (see: <https://hackage.haskell.org/package/zmidi-core> ).
module ZMidi.Score.ToMidiFile ( midiScoreToMidiFile ) where

import Data.Maybe          ( mapMaybe )
import Data.List           ( genericLength, sort )
import Control.Monad.State ( State, get, evalState, put )

import ZMidi.Core          ( MidiFile (..), MidiEvent (..), DeltaTime
                           , MidiVoiceEvent (..), MidiMetaEvent (..)
                           , MidiMessage, MidiTrack (..), MidiHeader (..) 
                           , MidiTimeDivision (..), MidiRunningStatus (..)
                           )

import ZMidi.Score.Datatypes hiding         ( TPB (..) )
import ZMidi.Score.Utilities                ( toMidiNr )

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
                 (floor . logBase @Double 2 . fromIntegral $ d) m n32)
                 

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
      c' = channel c
      v' = velocity v
  in [Timed o (NoteOn c' p' v'), Timed (o + d) (NoteOff c' p' 0)]
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
