-- |
-- Module      :  ZMidi.Score.Internal
-- Copyright   :  (c) 2012--2014, Utrecht University 
-- License     :  LGPL-3
--
-- Maintainer  :  W. Bas de Haas <w.b.dehaas@uu.nl>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: functions that are kept internal

module ZMidi.Score.Internal where

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

invalidMidiNumberError :: Show a => a -> b
invalidMidiNumberError w = error ("invalid MIDI note number" ++ show w)
