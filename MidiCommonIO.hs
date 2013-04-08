{-# OPTIONS_GHC -Wall                #-}
{-# LANGUAGE TupleSections           #-}
module MidiCommonIO ( mapDirInDir
                    , mapDir
                    , mapDir'
                    , printMidiToFile
                    , readMidiFile
                    , readMidiScore
                    , writeMidiScore
                    -- * Utilities
                    , logDuplicates
                    )where
                    
import ZMidi.Core         ( printMidiHeader, printMidiTrack, MidiFile (..)
                          , readMidi, writeMidi )
import ZMidiBasic         ( MidiScore (..), midiFileToMidiScore
                          , midiScoreToMidiFile )
import Data.List          ( intercalate )
import Control.Monad      ( filterM, void )
import System.Directory   ( getDirectoryContents, canonicalizePath
                          , doesDirectoryExist )
import System.IO          ( stderr, hPutStr )
import System.FilePath    ( (</>) )


-- | Applies a function to every directory inside a specific directory
mapDirInDir :: (FilePath -> IO a) -> FilePath ->  IO ()
mapDirInDir f fp = do fs  <- getDirectoryContents fp 
                              >>= return . filter (\x -> x /= "." && x /= "..") 
                      cfp <- canonicalizePath fp
                      filterM doesDirectoryExist (fmap (cfp </>) fs) >>= mapM_ f 

-- | Applies a function to every file/dir in a directory, similar to 'mapDir',
-- but it discards the result of the evaluation
mapDir' :: (FilePath -> IO a) ->  FilePath -> IO ()
mapDir' f = void . mapDir f

-- | Applies a function to every file/dir in a directory
mapDir :: (FilePath -> IO a) ->  FilePath -> IO [a]
mapDir f fp = do fs  <- getCurDirectoryContents fp
                 cin <- canonicalizePath fp
                 putErrStrLn cin
                 mapM (f . (cin </>)) fs where

  putErrStrLn :: String -> IO ()
  putErrStrLn s = do hPutStr stderr s
                     hPutStr stderr "\n"

-- | Reads a 'MidiFile' converts it into a 'MidiScore' and returns it
readMidiScore :: FilePath -> IO (MidiScore)
readMidiScore f = readMidiFile f >>= return . midiFileToMidiScore

-- | Reads a 'MidiFile'
readMidiFile :: FilePath -> IO (MidiFile)
readMidiFile  f = do mf <- readMidi f
                     case mf of Left  err -> error (f ++ '\t' : show err)
                                Right mid -> return mid

-- | Writes a 'MidiScore' to a file.
writeMidiScore :: MidiScore -> FilePath -> IO ()
writeMidiScore mf f = writeMidi f . midiScoreToMidiFile $ mf


-- | Writes the contents of a 'MidiFile' to a file.
printMidiToFile :: MidiFile -> FilePath -> IO ()
printMidiToFile mf fp = 
  let hd = printMidiHeader . mf_header $ mf
      ts = concatMap printMidiTrack . mf_tracks $ mf
  in  writeFile fp . intercalate "\n" $ (hd ++ ts)
  
-- | Reads all midifiles at a specific location and compares all files to 
-- all files, printing the paths of the files with the same MIDI content 
-- to the user
logDuplicates :: FilePath -> IO ()
logDuplicates fp = do midis <-  getCurDirectoryContents fp 
                            >>= mapM (return . (fp </>))
                            >>= mapM (\x -> readMidiFile x >>= return . (x,))
                      mapM_ (checkFile midis) midis where
  
  -- Prints if the paths differ, but the midi content is identical.  
  checkFile :: [(FilePath, MidiFile)] -> (FilePath, MidiFile) -> IO ()
  checkFile midis (f, mf) = case filter (\(x,y) -> x /= f && y == mf) midis of
    [] -> return ()
    l  -> mapM_ (\(m,_) -> putStrLn (f ++ " == " ++ m)) l

--------------------------------------------------------------------------------
-- Directory utils
--------------------------------------------------------------------------------

-- | Like 'getCurDirectoryContents', but filters the results for "." and ".."
getCurDirectoryContents :: FilePath -> IO [FilePath]
getCurDirectoryContents fp = 
  getDirectoryContents fp >>= return . filter (\x -> x /= "." && x /= "..") 

