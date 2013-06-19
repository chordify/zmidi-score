{-# OPTIONS_GHC -Wall #-}
module Main where

import ZMidi.Core         ( readMidi )
import ZMidiBasic
import MidiCommonIO       ( mapDirInDir, mapDir' )

-- other libraries
import System.Console.ParseArgs
import Control.Monad      ( void, when )
import RTCParser          ( readRTC, RTC (..) )
import MatchFile          ( readRTCMidis, matchAll, copyRTCMidi, printMatch )
import RagPat             ( printFileSubDiv, printSubDiv -- , hasValidTimeSig
                          , printFilePatMat, printPatCount, isStraight
                          , hasValidGridSize)

data RagArgs = Mode| MidiDir | RTCFile | MidiFile deriving (Eq, Ord, Show)

myArgs :: [Arg RagArgs]
myArgs = [ 
          Arg { argIndex = Mode,
                 argAbbr  = Just 'm',
                 argName  = Just "mode",
                 argData  = argDataRequired "rtc|stat|subdiv|mkrtc" ArgtypeString,
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
            ("stat"  , Left f ) -> showFileStats f
            ("stat"  , Right d) -> getCompendium arg >>= showDirStats d
            ("subdiv", Left f ) -> printFileSubDiv f
            ("subdiv", Right d) -> void . mapDirInDir (mapDir' printSubDiv) $ d
            ("mkrtc" , Right d) -> getCompendium arg >>= createSubCorpus d 
            ("rtc"   , Right d) -> getCompendium arg >>= ragPatDir d
            ("rtc"   , Left f ) -> printFilePatMat f
            (m       , _      ) -> usageError arg ("invalid mode: " ++ m )



-- | Checks for either a directory or file argument, returns them in an Either
-- or throws an error otherwise
fileOrDir :: Args RagArgs -> Either FilePath FilePath
fileOrDir arg = case (getArg arg MidiFile, getArg arg MidiDir) of
   (Just _, Just _) -> usageError arg "found both a directory and file"
   (Just f, _     ) -> Left f
   (_     , Just d) -> Right d
   (_     , _     ) -> usageError arg "No directory or file specified"
          
getCompendium :: Args RagArgs -> IO [RTC]
getCompendium arg = case getArg arg RTCFile of
                      Just c  -> readRTC c
                      Nothing -> usageError arg "no compendium specified"

-- | stuff to do with the rtc flag                      
ragPatDir :: FilePath -> [RTC] -> IO ()
ragPatDir d c = void . mapDirInDir (mapDir' (printPatCount c)) $ d 

-- | creates a subcorpus
createSubCorpus :: FilePath -> [RTC] -> IO ()
createSubCorpus dir c = do m <- readRTCMidis dir
                           mapM_ (copyRTCMidi "D:\\temp\\ragtimesSubSet") 
                                 (matchAll m c)


-- | Print some stats
showFileStats :: FilePath -> IO ()
showFileStats fp = do mf <- readMidi fp
                      case mf of
                        Left  err -> putStrLn (fp ++ '\t' : show err)
                        Right mid -> 
                          do let m  = quantise FourtyEighth . midiFileToMidiScore $ mid
                                 tm = buildTickMap . getVoices $ m
                                 d  = gcIOId tm
                             when (hasValidGridSize m) 
                                  (putStrLn (fp ++ '\t' : (show . isStraight $ m)))

showDirStats :: FilePath -> [RTC] -> IO ()
showDirStats dir c = do m <- readRTCMidis dir
                        mapM_ (putStrLn . printMatch) $ matchAll m c


