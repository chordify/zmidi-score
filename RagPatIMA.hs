{-# OPTIONS_GHC -Wall #-}
module Main where

import MidiCommonIO 

import ZMidiBasic            hiding ( Time )
import MelFind                      ( getAccompQuant )
import InnerMetricalAnalysis hiding ( main )
import LocalMeter           
-- import Math.Statistics              ( pearson )
import System.Environment           ( getArgs )

type IMAMatch = Float
type Pattern  = (Int, Float)

matchIMA :: ShortestNote -> MidiScore -> [Weight]
matchIMA q ms = 
  let acc = getAccompQuant q ms 
  in getSpectralWeight (Period . getMinDur . buildTickMap $ [acc]) 
   . map Time . toOnsets $ acc

preProcessMidi :: ShortestNote -> MidiScore -> [Time]
preProcessMidi q ms = map Time . toOnsets . getAccompQuant q $ ms


genPat :: Int -> TimeSig -> (Int -> Float)
genPat t ts = case ts of
        (TimeSig 2 2 _ _) -> project t 2 1 1
        (TimeSig 2 4 _ _) -> project t 2 1 1
        (TimeSig 4 4 _ _) -> project t 4 2 1
        _                 -> error "unexpected pattern"

-- project :: Time -> Time -> Time -> Time -> (Time -> Float)
project :: Int -> Int -> Int -> Int -> (Int -> Float)
project t prim sec trt x = case x `mod` (prim * t) of
                             0 -> 1.0
                             _ -> case x `mod` (sec * t) of 
                                    0 -> 0.5 
                                    _ -> case x `mod` (trt * t) of 
                                          0 -> 0.2 
                                          _ -> 0.0
                                          
main :: IO ()
main = do arg <- getArgs 
          case arg of
            [fp] -> do ms <- readMidiScore fp 
                       print . preProcessMidi Sixteenth $ ms
                       print . matchIMA Sixteenth $ ms
            _    -> error "Please provide a path to a midifile"