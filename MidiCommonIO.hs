module MidiCommonIO ( mapDirInDir
                    , mapDir
                    , printMidiToFile
                    , readMidiScore
                    )where
                    
import ZMidi.Core         ( printMidiHeader, printMidiTrack, MidiFile (..)
                          , readMidi )
import ZMidiBasic         ( MidiScore (..), empty
                          , midiFileToMidiScore )
import Data.List          ( intercalate )
import Control.Monad      ( filterM )
import System.Directory   ( getDirectoryContents, canonicalizePath
                          , doesDirectoryExist )
import System.IO          ( stderr, hPutStr )
import System.FilePath    ( (</>) )


-- | Applies a function to every directory inside a specific directory
mapDirInDir :: (FilePath -> IO ()) -> FilePath ->  IO ()
mapDirInDir f fp = do fs  <- getDirectoryContents fp 
                              >>= return . filter (\x -> x /= "." && x /= "..") 
                      cfp <- canonicalizePath fp
                      filterM doesDirectoryExist (fmap (cfp </>) fs) >>= mapM_ f 

-- | Applies a function to every file/dir in a directory
mapDir :: (FilePath -> IO ()) ->  FilePath -> IO ()
mapDir f fp = do fs <- getDirectoryContents fp >>= 
                   return . filter (\x -> x /= "." && x /= "..") 
                 cin <- canonicalizePath fp
                 putErrStrLn cin
                 mapM_ (f . (cin </>)) fs where

  putErrStrLn :: String -> IO ()
  putErrStrLn s = do hPutStr stderr s
                     hPutStr stderr "\n"

-- | reads a 'MidiFile' converts it into a 'MidiScore' and returns it
readMidiScore :: FilePath -> IO (MidiScore)
readMidiScore f = do mf <- readMidi f
                     case mf of
                       Left  err -> do putStrLn (f ++ '\t' : show err)
                                       return empty
                       Right mid ->    return (midiFileToMidiScore mid)
                     
-- | Writes the contents of a 'MidiFile' to a file.
printMidiToFile :: MidiFile -> FilePath -> IO ()
printMidiToFile mf fp = 
  let hd = printMidiHeader . mf_header $ mf
      ts = concatMap printMidiTrack . mf_tracks $ mf
  in  writeFile fp . intercalate "\n" $ (hd ++ ts)


