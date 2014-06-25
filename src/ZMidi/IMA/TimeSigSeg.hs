{-# OPTIONS_GHC -Wall                #-}
{-# LANGUAGE DeriveFunctor           #-}
{-# LANGUAGE DeriveGeneric           #-}
module ZMidi.IMA.TimeSigSeg ( TimedSeg (..)
                            , TimeSigSeg
                            , TimeSigTrack
                            , segByTimeSig
                            , toTimeSigSegs
                            , segment
                            ) where

import ZMidi.Score.Datatypes
import Data.List    ( sort )
import Data.Maybe   ( catMaybes )
import Data.Binary  ( Binary )
import GHC.Generics ( Generic )

type TimeSigSeg   = TimedSeg TimeSig [[Timed ScoreEvent]]
type TimeSigTrack = TimedSeg TimeSig  [Timed ScoreEvent]
data TimedSeg a b = TimedSeg { boundary :: Timed a
                             , seg      :: b 
                             } deriving (Show, Eq, Functor, Generic)

instance (Binary a, Binary b) => Binary (TimedSeg a b)
-- TODO : 2/4 is not always translated correctly to 4/4 when writing midi files

segByTimeSig :: MidiScore -> [MidiScore]
segByTimeSig ms = map toMS . toTimeSigSegs $ ms where

  toMS :: TimeSigSeg -> MidiScore
  toMS (TimedSeg ts vs) = ms { getTimeSig = [ts] 
                             , getVoices  = vs
                             }
  
toTimeSigSegs :: MidiScore -> [TimeSigSeg]
toTimeSigSegs ms = map mergeTracks . columns 
                 $ map (segment (getTimeSig ms)) (getVoices ms)


mergeTracks :: [TimeSigTrack] -> TimeSigSeg
mergeTracks [] = error "mergeTracks: emptyList"
mergeTracks t  = TimedSeg (boundary (head t)) (map seg t) -- all TimeSig are the same
                         


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

-- | Segments the second list at the time stamps of the first list
segment :: (Ord a, Ord b) => [Timed a] -> [Timed b] -> [TimedSeg a [Timed b]]
segment [] _ = error "TimeSigSeg.segment: no segment boundaries found"
segment ts v = toTimeSigSeg v (toSegments ts)

-- | Takes a list of 'Timed' values and a list of segment boundaries created
-- by 'toSegments'. This second list will slice the first list up and return
-- the 'TimedSeg'ments. 
toTimeSigSeg :: Ord a => [Timed a] -> [(Timed b, Maybe Time)] 
             -> [TimedSeg b [Timed a]]
toTimeSigSeg v = map (normaliseTime . toSeg) where

  -- toSeg :: (Timed a, Maybe Time) -> TimedSeg a
  toSeg (srt, Nothing ) = TimedSeg srt ( dropWhile ((< onset srt) . onset) v  ) 
  toSeg (srt, Just stp) = TimedSeg srt ( takeWhile ((< stp      ) . onset) 
                                       $ dropWhile ((< onset srt) . onset) v  )

-- | Takes a list of 'Timed' values and creates tuples containing the starting
-- element and 'Maybe' an ending 'Time'. In case of the last value, there
-- will be no end 'Time' and 'Nothing' will be stored.
toSegments :: (Ord a) => [Timed a] -> [(Timed a, Maybe Time)]
toSegments = foldr step [] . sort where

  step :: (Ord a) => Timed a -> [(Timed a, Maybe Time)] -> [(Timed a, Maybe Time)]
  step ts  []   = [(ts , Nothing)] 
  step srt rest =  (srt, Just . onset . fst . head $ rest) : rest

-- | Sets the timestamps in the Timed data to a timestamp relative to the
-- boundary time stamp
normaliseTime :: Ord b => TimedSeg a [Timed b] -> TimedSeg a [Timed b]
normaliseTime (TimedSeg t ts) = TimedSeg t (map f ts) where
  f x = x { onset = onset x - onset t }
  
-- some tests
-- testData = take 20 $ zipWith Timed [0..] (cycle ['a'..'z'])
-- readMidiScore "mid\\Coontown Review, De - Pitt-Payne.mid" >>= mapM showMidiScore . segByTimeSig 
-- readMidiScore "mid\\Coontown Review, De - Pitt-Payne.mid" >>= \x -> sequence (zipWith writeMidiScore (segByTimeSig x) ["test1.mid", "test2.mid", "test3.mid", "test4.mid"])
