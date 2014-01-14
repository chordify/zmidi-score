{-# OPTIONS_GHC -Wall                #-}
{-# LANGUAGE TupleSections           #-}
module MidiCommonIO (-- * Mapping
                      mapDirInDir
                    , mapDir
                    , mapDir_
                    -- * Folding
                    , foldrDirInDir
                    , foldrDir
                    -- * Reading and Writing
                    , readMidiFile
                    , readMidiScoreSafe
                    , readMidiScore
                    , writeMidiScore
                    -- * Utilities
                    -- , printMidiToFile
                    , logDuplicates
                    , removeTrackLabels
                    , putErrStrLn
                    )where
                    
import ZMidi.Core         ( MidiFile (..), readMidi, writeMidi )
import ZMidiBasic         ( MidiScore (..), midiFileToMidiScore
                          , midiScoreToMidiFile, removeLabels )
import Control.Monad      ( filterM, void )
import System.Directory   ( getDirectoryContents, canonicalizePath
                          , doesDirectoryExist )
import System.IO          ( stderr, hPutStrLn )
import System.FilePath    ( (</>) )
import Data.Foldable      ( foldrM )

--------------------------------------------------------------------------------
-- Mapping
--------------------------------------------------------------------------------

-- | Applies a function to every directory inside a specific directory
mapDirInDir :: (FilePath -> IO a) -> FilePath ->  IO [a]
mapDirInDir f fp = do fs  <- getDirectoryContents fp 
                              >>= return . filter (\x -> x /= "." && x /= "..") 
                      cfp <- canonicalizePath fp
                      filterM doesDirectoryExist (fmap (cfp </>) fs) >>= mapM f 

-- | Applies a function to every file/dir in a directory, similar to 'mapDir',
-- but it discards the result of the evaluation
mapDir_ :: (FilePath -> IO a) ->  FilePath -> IO ()
mapDir_ f = void . mapDir f

-- | Applies a function to every file/dir in a directory
mapDir :: (FilePath -> IO a) ->  FilePath -> IO [a]
mapDir f fp = do fs  <- getCurDirectoryContents fp
                 cin <- canonicalizePath fp
                 putErrStrLn cin
                 mapM (f . (cin </>)) fs 

foldrDirInDir :: (FilePath -> b -> IO b) -> b -> FilePath -> IO b
foldrDirInDir f b fp = 
  do fs  <- getDirectoryContents fp 
              >>= return . filter (\x -> x /= "." && x /= "..") 
     cfp <- canonicalizePath fp
     filterM doesDirectoryExist (fmap (cfp </>) fs)  >>= foldrM f b  
                 
-- | Applies a function to every file/dir in a directory
foldrDir :: (FilePath -> b -> IO b) -> b -> FilePath -> IO b
foldrDir f b fp = do fs  <- getCurDirectoryContents fp
                     cin <- canonicalizePath fp
                     putErrStrLn cin
                     foldrM (\x y -> f (cin </> x) $! y) b fs 

--------------------------------------------------------------------------------
-- Reading & Writing
--------------------------------------------------------------------------------

readMidiScoreSafe :: FilePath -> IO (Maybe MidiScore)
readMidiScoreSafe f = 
  do mf <- readMidi f
     case mf of Left  _er -> return   Nothing
                Right mid -> return . Just . midiFileToMidiScore $ mid


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

                      
--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

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
  
-- | Removes the Track labels from a 'MidiFile'
removeTrackLabels :: FilePath -> IO ()
removeTrackLabels f = readMidiFile f >>= 
                      writeMidi (f ++ ".noLab.mid") . removeLabels
                      

putErrStrLn :: String -> IO ()
putErrStrLn s = hPutStrLn stderr s
                   
--------------------------------------------------------------------------------
-- Unexported directory utils
--------------------------------------------------------------------------------

-- | Like 'getCurDirectoryContents', but filters the results for "." and ".."
getCurDirectoryContents :: FilePath -> IO [FilePath]
getCurDirectoryContents fp = 
  getDirectoryContents fp >>= return . filter (\x -> x /= "." && x /= "..") 

