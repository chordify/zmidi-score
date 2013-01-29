module Main where

import ZMidi.Core ( readMidi, printMidi, printMidiHeader
                  , printMidiTrack, MidiFile (..), writeMidi)
import ZMidiBasic

import Data.List (intercalate)
import Control.Monad (filterM)
import System.Directory ( getDirectoryContents, canonicalizePath
                        , doesDirectoryExist)
import System.IO (stderr, hPutStr)
import System.FilePath 
import System.Environment (getArgs)

import Data.IntMap.Lazy (keys)
-- import Debug.Trace (traceShow)

main :: IO ()
main = do arg <- getArgs
          case arg of
            ["-d", d] -> do putStrLn ("filepath\tTime Signatures\tKeys\t" 
                                   ++ "Nr. Voices"
                                   ++ "\tgcIOI divisor\tticks p. beat"
                                   ++ "\tNr. Notes\tnr. div. durations\tdurations")
                            mapDirInDir (mapDir showMidiStats) d
            ["-f", f] -> readMidiFile f
            _         -> putStrLn "usage:  <filename> "


readMidiFile :: FilePath -> IO ()
readMidiFile f = do mf <- readMidi f
                    case mf of
                      Left  err -> putStrLn (f ++ '\t' : show err)
                      Right mid -> do let -- cmid = canonical mid
                                          ms   = midiFileToMidiScore mid
                                          qs   = quantise ms
                                        --  tm   = buildTickMap . getVoices $ ms
                                      printMidi mid
                                      printMidiToFile mid (f ++ ".txt")
                                      printMidiToFile (midiScoreToMidiFile ms) (f ++ ".test.txt")                                 
                                      printMidiToFile (midiScoreToMidiFile qs) (f ++ ".quantised.txt")
                                      writeMidi (f ++ "test.mid") (midiScoreToMidiFile ms)
                                      writeMidi (f ++ "quantised.mid") (midiScoreToMidiFile qs)
                                      -- print tm
                                      -- print . gcIOId $ tm
                                      -- putStrLn . showMidiScore $ ms
                                      -- putStrLn . showMidiScore . quantise $ ms
                                   -- print . midiFileToMidiScore $ cmid 
                                   
printMidiToFile :: MidiFile -> FilePath -> IO ()
printMidiToFile mf fp = 
  let hd = printMidiHeader . mf_header $ mf
      ts = concatMap printMidiTrack . mf_tracks $ mf
  in  writeFile fp . intercalate "\n" $ (hd ++ ts)
  
showMidiStats :: FilePath -> IO ()
showMidiStats fp = do mf <- readMidi fp
                      case mf of
                        Left  err -> putStrLn (fp ++ '\t' : show err)
                        Right mid -> 
                          do let m  = midiFileToMidiScore mid
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


mapDirInDir :: (FilePath -> IO ()) -> FilePath ->  IO ()
mapDirInDir f fp = do fs  <- getDirectoryContents fp 
                              >>= return . filter (\x -> x /= "." && x /= "..") 
                      cfp <- canonicalizePath fp
                      filterM doesDirectoryExist (fmap (cfp </>) fs) >>= mapM_ f 
                                        
mapDir :: (FilePath -> IO ()) ->  FilePath -> IO ()
mapDir f fp = do fs <- getDirectoryContents fp >>= 
                   return . filter (\x -> x /= "." && x /= "..") 
                 cin <- canonicalizePath fp
                 putErrStrLn cin
                 mapM_ (f . (cin </>)) fs

putErrStrLn :: String -> IO ()
putErrStrLn s = do hPutStr stderr s
                   hPutStr stderr "\n"