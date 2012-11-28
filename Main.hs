module Main where

import ZMidi.Core (MidiFile, readMidi, printMidi)
import ZMidiBasic

import System.Directory (getDirectoryContents, canonicalizePath)
import System.FilePath 
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
                                      let ms =  midiFileToMidiScore mid 
                                      print . buildTickMap . getVoices $ ms
                                      putStrLn . showMidiScore $ ms
                                   -- print . midiFileToMidiScore $ mid 

                                   
showMidiStats :: FilePath -> IO ()
showMidiStats fp = do mf <- readMidi fp
                      case mf of
                        Left  err -> print err
                        Right mid -> 
                          do let m = midiFileToMidiScore mid
                                 tm = buildTickMap . getVoices $ m
                                 q  = if isQuantised tm then "Quantised" else "No"
                             putStrLn (fp ++ '\t' : show (getTimeSig m) 
                                ++ '\t' : show (getKey m)
                                ++ '\t' : (show . length . getVoices $ m) 
                                ++ '\t' : q)
                             
                                        -- print tm
                                        -- print (getMinDur tm)
                                        -- print (isQuantised tm)
                                        
doDir :: (FilePath -> IO ()) ->  FilePath -> IO()
doDir f fp = do fs <- getDirectoryContents fp >>= 
                  return . filter (\x -> x /= "." && x /= "..") 
                cin <- canonicalizePath fp
                mapM_ (f . (cin </>)) fs
     