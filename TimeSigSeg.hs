module TimeSigSeg where

import ZMidiBasic

type TimeSigSeg = (TimeSig, Voice)

toTimeSigSegs :: MidiScore -> [TimeSigSeg]
toTimeSigSegs = undefined