{-# OPTIONS_GHC -Wall #-}
module Main where

import ZMidi.Core         ( readMidi )
import ZMidiBasic
import MidiCommonIO       ( mapDirInDir, mapDir', readMidiScore )

-- import System.Environment ( getArgs )
import Data.IntMap.Lazy   ( keys )

-- other libraries
import System.Console.ParseArgs
import Control.Monad      ( void )
import RTCParser          ( readRTC, RTC (..) )
import MatchFile          ( getRTCMeta
                          -- , readRTCMidiPath, readRTCMidis, match
                          -- , printMatch, matchAll, copyRTCMidi, groupRTCMidis
                           )
import RagPat              ( printFileSubDiv, printSubDiv, hasValidTimeSig )

data RagArgs = Mode| MidiDir | RTCFile | MidiFile deriving (Eq, Ord, Show)

myArgs :: [Arg RagArgs]
myArgs = [ 
          Arg { argIndex = Mode,
                 argAbbr  = Just 'm',
                 argName  = Just "mode",
                 argData  = argDataRequired "rtc|stat|subdiv" ArgtypeString,
                 argDesc  = "Mode of operation"
               }
         , Arg { argIndex = MidiDir,
                 argAbbr  = Just 'd',
                 argName  = Just "dir",
                 argData  = argDataOptional "filepath" ArgtypeString,
                 argDesc  = "Base directory containing the MIDI files"
               }
         , Arg { argIndex = RTCFile,
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
main = do arg <- parseArgsIO ArgsComplete myArgs
          case (getRequiredArg arg Mode, fileOrDir arg) of
            ("stat"  , Left f ) -> doScore f
            ("stat"  , Right d) -> do putStr   "file\ttime signature\tis valid time sig\t"
                                      putStr   "keys\tnr of voices\tgc IOI divider\t"
                                      putStrLn "ticks Per Beat\tnr Of Notes\tnote lengths"
                                      void . mapDirInDir (mapDir' showMidiStats) $ d
            ("subdiv", Left f ) -> printFileSubDiv f
            ("subdiv", Right d) -> void . mapDirInDir (mapDir' printSubDiv) $ d
            ("rtc"   , Right d) -> do case getArg arg RTCFile of
                                        Just c  -> mainRTC c d
                                        Nothing -> usageError arg 
                                                   "no compendium specified"
            (m     , _      ) -> usageError arg ("invalid mode: " ++ m )
            
mainRTC :: FilePath -> FilePath -> IO ()
mainRTC comp d = 
  do c <- readRTC comp
     void . mapDirInDir (mapDir' (print . rtcid . getRTCMeta c)) $ d 


{-
createSubCorpus :: FilePath -> FilePath -> IO ()
createSubCorpus comp dir = do c <- readRTC comp
                              m <- readRTCMidis dir
                              mapM_ (copyRTCMidi "D:\\temp\\ragtimesSubSet") 
                                    (matchAll m (groupRTCMidis m) c)

-}

-- | Checks for either a directory or file argument, returns them in an Either
-- or throws an error otherwise
fileOrDir :: Args RagArgs -> Either FilePath FilePath
fileOrDir arg = case (getArg arg MidiFile, getArg arg MidiDir) of
   (Just _, Just _) -> usageError arg "found both a directory and file"
   (Just f, _     ) -> Left f
   (_     , Just d) -> Right d
   (_     , _     ) -> usageError arg "No directory or file specified"
          

-- | do stuff with a 'MidiScore' ...
doScore :: FilePath -> IO ()
doScore f = readMidiScore f >>= putStrLn . showMidiScore

-- | Print some stats
showMidiStats :: FilePath -> IO ()
showMidiStats fp = do mf <- readMidi fp
                      case mf of
                        Left  err -> putStrLn (fp ++ '\t' : show err)
                        Right mid -> 
                          do let m  = quantise FourtyEighth . midiFileToMidiScore $ mid
                                 tm = buildTickMap . getVoices $ m
                                 d  = gcIOId tm
                             putStrLn (fp ++ '\t' : show (getTimeSig m) 
                                ++ '\t' : (show . hasValidTimeSig $ m)
                                ++ '\t' : show (getKey m)
                                ++ '\t' : (show . length . getVoices $ m) 
                                ++ '\t' : show d 
                                ++ '\t' : (show . ticksPerBeat $ m)
                                ++ '\t' : (show . nrOfNotes $ m)
                                ++ '\t' : (show . length . keys $ tm)
                                ++ '\t' :  show tm)

