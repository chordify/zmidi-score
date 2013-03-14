module Main (main) where

import ZMidiBasic
import MidiCommonIO       ( readMidiScore, mapDir )

import Data.List          ( intercalate )
import System.Environment ( getArgs )

main :: IO ()
main = do arg <- getArgs
          case arg of
            ["-d", d] -> do putStrLn ("filepath\tmin 1\tmax 1\tmin 2\tmax 2")
                            mapDir showMidiStats d
            ["-f", f] -> readMidiFile f
            _         -> putStrLn "usage:  <filename> "


readMidiFile :: FilePath -> IO ()
readMidiFile f = do ms <- readMidiScore f
                    mapM_ (putStrLn . show . voiceStats) (getVoices ms)

                                      
                      
voiceStats :: Voice -> (Pitch,Pitch) 
voiceStats v = let ps = map getPitch v 
               in (minimum ps, maximum ps)
  
  where getPitch :: Timed ScoreEvent -> Pitch
        getPitch tse = case getEvent tse of 
                         (NoteEvent _c p _v _d) -> p
                         _                      -> error "unexpected ScoreEvent!"

showVoiceStats :: Voice -> String
showVoiceStats v = let (mn,mx) = voiceStats v in show mn ++ '\t' : show mx

showMidiStats :: FilePath -> IO ()
showMidiStats fp = do ms <- readMidiScore fp
                      putStr (fp ++ "\t")
                      putStrLn . intercalate "\t"  . map showVoiceStats 
                               . getVoices $ ms 
                                        