module RagPatIMA where

import MidiCommonIO 

import ZMidiBasic
import MelFind                      ( getAccompQuant )
import InnerMetricalAnalysis hiding ( Time )
import Math.Statistics              ( pearson )

type IMAMatch = Float
type Pattern  = (Int, Float)

matchIMA :: ShortestNote -> MidiScore -> [Time]
matchIMA q ms = getSpectralWeight (getMinGridSize q ms) 
              . toOnsets . getAccompQuant q $ ms

genPat :: Time -> TimeSig -> (Time -> Float)
genPat t ts = case ts of
        (TimeSig 2 2 _ _) -> project t 2 1 1
        (TimeSig 2 4 _ _) -> project t 2 1 1
        (TimeSig 4 4 _ _) -> project t 4 2 1

project :: Time -> Time -> Time -> Time -> (Time -> Float)
project t prim sec trt x = case x `mod` (prim * t) of
                             0 -> 1.0
                             _ -> case x `mod` (sec * t) of 
                                    0 -> 0.5 
                                    _ -> case x `mod` (trt * t) of 
                                          0 -> 0.2 
                                          _ -> 0.0