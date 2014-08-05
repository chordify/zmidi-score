module ZMidi.IMA.Constants where

import ZMidi.Score.Datatypes

--------------------------------------------------------------------------------
-- parameters
--------------------------------------------------------------------------------

acceptedTimeSigs :: [TimeSig]
acceptedTimeSigs = [ TimeSig 2 2 0 0, TimeSig 2 4 0 0
                   , TimeSig 4 4 0 0, TimeSig 3 4 0 0
                   , TimeSig 6 8 0 0 ]