module Main where

import ZMidi.Core         ( readMidi )
import ZMidiBasic
import MidiCommonIO       ( mapDirInDir, mapDir, readMidiScore )

import System.Environment ( getArgs )

main :: IO ()
main = do arg <- getArgs
          case arg of
            ["-d", d] -> print d -- mapDirInDir (mapDir showMidiStats) d
            ["-f", f] -> doScore f
            _         -> putStrLn "usage:  -f <filename> OR -d <directory>"


-- | do stuff with a 'MidiScore' ...
doScore :: FilePath -> IO ()
doScore f = do mf <- readMidiScore f 
               putStrLn . showMidiScore $ mf
               putStrLn . showMidiScore . quantise FourtyEighth $ mf
               

type Bar    = [Bool] -- This should be a fixed length list
type Rhythm = [Bar]




