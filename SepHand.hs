module Main (main) where

import ZMidi.Core ( readMidi )
import ZMidiBasic

import Data.List (intercalate)
import System.Directory ( getDirectoryContents, canonicalizePath )
import System.IO (stderr, hPutStr)
import System.FilePath 
import System.Environment (getArgs)

main :: IO ()
main = do arg <- getArgs
          case arg of
            ["-d", d] -> do putStrLn ("filepath\tmin 1\tmax 1\tmin 2\tmax 2")
                            mapDir showMidiStats d
            ["-f", f] -> readMidiFile f
            _         -> putStrLn "usage:  <filename> "


readMidiFile :: FilePath -> IO ()
readMidiFile f = do mf <- readMidi f
                    case mf of
                      Left  err -> putStrLn (f ++ '\t' : show err)
                      Right mid -> do let ms   = midiFileToMidiScore mid
                                      mapM_ (putStrLn . show . voiceStats) 
                                            (getVoices ms)
                      
voiceStats :: Voice -> (Int, Int) --, Double)
voiceStats v = let ps = map getPitch v 
               in (minimum ps, maximum ps) --, (fromIntegral . sum $ ps) / (genericLength ps))
  
  where getPitch :: Num a => Timed ScoreEvent -> a
        getPitch tse = case getEvent tse of 
                         (NoteEvent _c p _v _d) -> fromIntegral p
                         _                      -> error "unexpected ScoreEvent!"

showVoiceStats :: Voice -> String
showVoiceStats v = let (mn,mx) = voiceStats v in show mn ++ '\t' : show mx

showMidiStats :: FilePath -> IO ()
showMidiStats fp = do mf <- readMidi fp
                      case mf of
                        Left  err -> putStrLn (fp ++ '\t' : show err)
                        Right mid -> 
                          do putStr (fp ++ "\t")
                             putStrLn . intercalate "\t"  . map showVoiceStats $
                                      (getVoices . midiFileToMidiScore $ mid )
                                        
mapDir :: (FilePath -> IO ()) ->  FilePath -> IO ()
mapDir f fp = do fs <- getDirectoryContents fp >>= 
                   return . filter (\x -> x /= "." && x /= "..") 
                 cin <- canonicalizePath fp
                 putErrStrLn cin
                 mapM_ (f . (cin </>)) fs

putErrStrLn :: String -> IO ()
putErrStrLn s = do hPutStr stderr s
                   hPutStr stderr "\n"