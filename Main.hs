module Main where

import ZMidi.Core         ( readMidi )
import ZMidiBasic
import MidiCommonIO       ( mapDirInDir, mapDir, readMidiScore )

import System.Environment ( getArgs )
import Data.IntMap.Lazy   ( keys )

main :: IO ()
main = do arg <- getArgs
          case arg of
            ["-d", d] -> do putStrLn ("filepath\tTime Signatures\tKeys\t" 
                                   ++ "Nr. Voices"
                                   ++ "\tgcIOI divisor\tticks p. beat"
                                   ++ "\tNr. Notes\tnr. div. durations\tdurations")
                            mapDirInDir (mapDir showMidiStats) d
            ["-f", f] -> doScore f
            _         -> putStrLn "usage:  -f <filename> OR -d <directory>"


-- | do stuff with a 'MidiScore' ...
doScore :: FilePath -> IO ()
doScore f = readMidiScore f >>= putStrLn . showMidiScore
                                
showMidiStats :: FilePath -> IO ()
showMidiStats fp = do mf <- readMidi fp
                      case mf of
                        Left  err -> putStrLn (fp ++ '\t' : show err)
                        Right mid -> 
                          do let m  = quantise ThirtySecond . midiFileToMidiScore $ mid
                                 tm = buildTickMap . getVoices $ m
                                 d  = gcIOId tm
                             putStrLn (fp ++ '\t' : show (getTimeSig m) 
                                ++ '\t' : show (getKey m)
                                ++ '\t' : (show . length . getVoices $ m) 
                                ++ '\t' : show d 
                                ++ '\t' : (show . ticksPerBeat $ m)
                                ++ '\t' : (show . nrOfNotes $ m)
                                ++ '\t' : (show . length . keys $ tm)
                                ++ '\t' :  show tm)

