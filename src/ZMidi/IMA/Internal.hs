{-# OPTIONS_GHC -Wall                   #-}
module ZMidi.IMA.Internal where

import ZMidi.Score             ( TimeSig (..), Beat, BeatRat )
import qualified Data.Map as M ( Map, lookup )

-- | assumes a number between 0 and 1 and prints them as a sequence of asterisks 
stars :: (Show a, RealFrac a) => a -> String
stars w = replicate (round (20 * w)) '*' 


-- | Parses a string representation of a Time signature
parseTimeSig :: String -> Either String TimeSig
parseTimeSig s = case s of 
                   "4/4" -> Right $ TimeSig 4 4 0 0
                   "2/4" -> Right $ TimeSig 2 4 0 0
                   "2/2" -> Right $ TimeSig 2 2 0 0
                   "3/4" -> Right $ TimeSig 3 4 0 0
                   "6/8" -> Right $ TimeSig 6 8 0 0
                   ts    -> Left ("unknown timesignature: " ++ ts)
                   
-- | Looks up a value and if it is not found throws an error
lookupErr :: Ord k => String -> M.Map k v -> k -> v
lookupErr s m k = case M.lookup k m of 
                    Just v  -> v
                    Nothing -> error s