module Main where

import ZMidi.Core (MidiFile, readMidi, printMidi)
import ZMidiBasic

import Control.Monad (filterM)
import System.Directory ( getDirectoryContents, canonicalizePath
                        , doesDirectoryExist)
import System.FilePath 
import System.Environment (getArgs)

import Debug.Trace (traceShow)

main :: IO ()
main = do arg <- getArgs
          case arg of
            [f] -> mapDirInDir (mapDir showMidiStats)  f
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

mapDirInDir :: (FilePath -> IO ()) -> FilePath ->  IO ()
mapDirInDir f fp = do fs  <- getDirectoryContents fp 
                              >>= return . filter (\x -> x /= "." && x /= "..") 
                      cfp <- canonicalizePath fp
                      filterM doesDirectoryExist (fmap (cfp </>) fs) >>= mapM_ f 
                                        
mapDir :: (FilePath -> IO ()) ->  FilePath -> IO ()
mapDir f fp = do fs <- getDirectoryContents fp >>= 
                   return . filter (\x -> x /= "." && x /= "..") 
                 cin <- canonicalizePath fp
                 traceShow cin (mapM_ (f . (cin </>)) fs)
     