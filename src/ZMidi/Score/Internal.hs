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

import Data.Map.Strict                ( Map )
import qualified Data.Map.Strict as M ( lookup )

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

invalidMidiNumberError :: Show a => a -> b
invalidMidiNumberError w = error ("invalid MIDI note number" ++ show w)

lookupE :: (Ord a, Show a) => a -> Map a b -> Either String b
lookupE k m = case M.lookup k m of 
    Just p -> Right p
    _      -> Left ("lookupE: could not find key: " ++ show k)