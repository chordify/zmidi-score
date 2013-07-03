module TimeSigSeg where

import ZMidiBasic
import Data.List    ( sort, cycle )

-- type TimeSigSeg = (TimeSig, Voice)
type TimedSeg a = (Timed a, [Timed a])

toTimeSigSegs :: MidiScore -> [TimedSeg a]
toTimeSigSegs = undefined

toTimeSigSeg :: [Timed a] -> [(Timed a, Maybe Time)] -> [TimedSeg a]
toTimeSigSeg v = map toSeg where

  -- toSeg :: (Timed a, Maybe Time) -> TimedSeg a
  toSeg (srt, Nothing ) = (srt, dropWhile ((< onset srt) . onset) v)
  toSeg (srt, Just stp) = (srt, takeWhile ((< stp      ) . onset) 
                              $ dropWhile ((< onset srt) . onset) v )

toSegments :: (Ord a) => [Timed a] -> [(Timed a, Maybe Time)]
toSegments = foldr step [] . sort where

  step :: (Ord a) => Timed a -> [(Timed a, Maybe Time)] -> [(Timed a, Maybe Time)]
  step ts  []   = [(ts , Nothing)] 
  step srt rest =  (srt, Just . onset . fst . head $ rest) : rest

  
testData = take 110 $ zipWith Timed [0..] (cycle ['a'..'z'])