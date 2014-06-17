{-# OPTIONS_GHC -Wall                   #-}
module ZMidi.IMA.Internal where

-- | assumes a number between 0 and 1 and prints them as a sequence of asterisks 
stars :: (Show a, RealFrac a) => a -> String
stars w = replicate (round (20 * w)) '*' 
