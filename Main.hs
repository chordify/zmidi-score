module Main where

import ZMidi.Core (MidiFile, readMidi, printMidi)
import ZMidiBasic

import System.Environment (getArgs)

main :: IO ()
main = do arg <- getArgs
          case arg of
            [f] -> readMidiFile f
            _   -> putStrLn "usage: MidiCSV <filename> "


readMidiFile :: FilePath -> IO ()
readMidiFile f = do mf <- readMidi f
                    case mf of
                      Left  err -> print err
                      Right mid -> do printMidi mid
                                      putStrLn . showMidiScore . midiFileToMidiScore $ mid 
                                   -- print . midiFileToMidiScore $ mid 

                                   
showMidiStats :: FilePath -> IO ()
showMidiStats fp = do mf <- readMidi fp
                      case mf of
                        Left  err -> print err
                        Right mid -> do let m = midiFileToMidiScore mid
                                        putStrLn (fp ++ '\t' : show (getTimeSig m) 
                                                     ++ '\t' : show (getKey m)
                                                     ++ '\t' : (show . length . getVoices $ m) 
                                                     )
                                        let tm = buildTickMap . getVoices $ m
                                        print tm
                                        print (getMinDur tm)
                                        print (isQuantised tm)