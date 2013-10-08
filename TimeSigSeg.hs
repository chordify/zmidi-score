module TimeSigSeg where

import ZMidiBasic
import MidiCommonIO
import Data.List    ( sort, cycle )
import Data.Maybe   ( catMaybes )

type TimeSigSeg   = TimedSeg TimeSig ScoreEvent
type TimedSeg a b = (Timed a, [Timed b])

-- TODO : two bugs: 1) the timesignature midi event can be set to immediate
-- and to change signature in the next bar, resulting in an unwanted offset
-- 2) 2/4 is not always translated correctly to 4/4 when writing midi files

segByTimeSig :: MidiScore -> [MidiScore]
segByTimeSig ms = map toSeg . toTimeSigSegs $ ms where

  toSeg :: [TimeSigSeg] -> MidiScore
  toSeg tss = let (ts, vs) = unzip tss
              in  ms { getTimeSig = [head ts] -- all TimeSig are the same
                     , getVoices = vs}
  
toTimeSigSegs :: MidiScore -> [[TimeSigSeg]]
toTimeSigSegs ms = columns $ map (segment (getTimeSig ms)) (getVoices ms)

-- | Converts a row matrix to a column matrix
--
-- >>> columns [[1,2,3],[4,5,6],[7,8,9]]
-- [[1,4,7],[2,5,8],[3,6,9]]
--
-- >>> columns [[1,2,3],[4,5],[7,8,9]]
-- [[1,4,7],[2,5,8],[3,9]]
-- 
-- >>> columns [[1,2,3],[4,5,6],[7,8,9,10,11]]
-- [[1,4,7],[2,5,8],[3,6,9],[10],[11]]
columns :: [[a]] -> [[a]]
columns l = let (col, rows) = unzip $ map saveHead l

                saveHead :: [a] -> (Maybe a, [a])
                saveHead []     = (Nothing, [])
                saveHead (h:tl) = (Just h , tl)

            in case catMaybes col of
                 [] -> []
                 c  -> c : columns rows

segment :: Ord a => [Timed a] -> [Timed b] -> [TimedSeg a b]
segment ts v = toTimeSigSeg v (toSegments ts)

-- | Takes a list of 'Timed' values and a list of segment boundaries created
-- by 'toSegments'. This second list will slice the first list up and return
-- the 'TimedSeg'ments. 
toTimeSigSeg :: [Timed b] -> [(Timed a, Maybe Time)] -> [TimedSeg a b]
toTimeSigSeg v = map toSeg where

  -- toSeg :: (Timed a, Maybe Time) -> TimedSeg a
  toSeg (srt, Nothing ) = (srt, dropWhile ((< onset srt) . onset) v)
  toSeg (srt, Just stp) = (srt, takeWhile ((< stp      ) . onset) 
                              $ dropWhile ((< onset srt) . onset) v )

-- | Takes a list of 'Timed' values and creates tuples containing the starting
-- element and 'Maybe' an ending 'Time'. In case of the last value, there
-- will be no end 'Time' and 'Nothing' will be stored.
toSegments :: (Ord a) => [Timed a] -> [(Timed a, Maybe Time)]
toSegments = foldr step [] . sort where

  step :: (Ord a) => Timed a -> [(Timed a, Maybe Time)] -> [(Timed a, Maybe Time)]
  step ts  []   = [(ts , Nothing)] 
  step srt rest =  (srt, Just . onset . fst . head $ rest) : rest

-- some tests
-- testData = take 20 $ zipWith Timed [0..] (cycle ['a'..'z'])
-- readMidiScore "mid\\Coontown Review, De - Pitt-Payne.mid" >>= mapM showMidiScore . segByTimeSig 
-- readMidiScore "mid\\Coontown Review, De - Pitt-Payne.mid" >>= \x -> sequence (zipWith writeMidiScore (segByTimeSig x) ["test1.mid", "test2.mid", "test3.mid", "test4.mid"])