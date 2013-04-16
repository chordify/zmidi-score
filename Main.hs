{-# OPTIONS_GHC -Wall #-}
module Main where

import ZMidi.Core         ( readMidi )
import ZMidiBasic
import MidiCommonIO       ( mapDirInDir, mapDir, readMidiScore )

-- import System.Environment ( getArgs )
import Data.IntMap.Lazy   ( keys )

-- other libraries
import System.Console.ParseArgs
import RTCParser          ( parseRTCFile )

data RagArgs = Mode| MidiDir | RTC | MidiFile deriving (Eq, Ord, Show)

myArgs :: [Arg RagArgs]
myArgs = [ 
          Arg { argIndex = Mode,
                 argAbbr  = Just 'm',
                 argName  = Just "mode",
                 argData  = argDataRequired "<rtc|stat>" ArgtypeString,
                 argDesc  = "Mode of operation"
               }
         , Arg { argIndex = MidiDir,
                 argAbbr  = Just 'd',
                 argName  = Just "dir",
                 argData  = argDataOptional "filepath" ArgtypeString,
                 argDesc  = "Base directory containing the MIDI files"
               }
         , Arg { argIndex = RTC,
                 argAbbr  = Just 'c',
                 argName  = Just "rtc",
                 argData  = argDataOptional "filpath" ArgtypeString,
                 argDesc  = "The file containing the ragtime compendium"
               }
         , Arg { argIndex = MidiFile,
                 argAbbr  = Just 'f',
                 argName  = Just "file",
                 argData  = argDataOptional "filepath" ArgtypeString,
                 argDesc  = "test"
               }
         ] 

main :: IO ()
main = do args <- parseArgsIO ArgsComplete myArgs
          case (getRequiredArg args Mode, fileOrDir args) of
            ("stat", Left f ) -> doScore f
            ("stat", Right d) -> mapDirInDir (mapDir showMidiStats) d
            ("rtc" , Right d) -> do case getArg args RTC of
                                      Just c  -> parseRTCFile c
                                      Nothing -> usageError args 
                                                 "no compendium specified"
            (m     , _      ) -> usageError args ("invalid mode: " ++ m )
            
            
-- | Checks for either a directory or file argument, returns them in an Either
-- or throws an error otherwise
fileOrDir :: Args RagArgs -> Either FilePath FilePath
fileOrDir args = case (getArg args MidiFile, getArg args MidiDir) of
   (Just _, Just _) -> usageError args "found both a directory and file"
   (Just f, _     ) -> Left f
   (_     , Just d) -> Right d
   (_     , _     ) -> usageError args "No directory or file specified"
          

-- | do stuff with a 'MidiScore' ...
doScore :: FilePath -> IO ()
doScore f = readMidiScore f >>= putStrLn . showMidiScore

-- | Print some stats
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

