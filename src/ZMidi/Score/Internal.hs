module ZMidi.Score.Internal where

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

invalidMidiNumberError :: Show a => a -> b
invalidMidiNumberError w = error ("invalid MIDI note number" ++ show w)
