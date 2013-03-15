module Main (main) where

import ZMidi.Core         ( writeMidi )
import ZMidiBasic
import MidiCommonIO       ( readMidiScore, mapDir )

import Data.List          ( intercalate, sort )
import System.Environment ( getArgs )

main :: IO ()
main = do arg <- getArgs
          case arg of
            ["-d", d] -> do putStrLn ("filepath\tmin 1\tmax 1\tmin 2\tmax 2")
                            mapDir showMidiStats d
            ["-f", f] -> readMidiFile f
            _         -> putStrLn "usage:  <filename> "


readMidiFile :: FilePath -> IO ()
readMidiFile f = 
  do ms <- readMidiScore f
     writeMidi (f ++ "1track.mid") . midiScoreToMidiFile . mergeTracks $ ms
     -- mapM_ (putStrLn . show . voiceStats) (getVoices ms)
                    

                      
voiceStats :: Voice -> (Pitch,Pitch) 
voiceStats v = let ps = map getPitch v 
               in (minimum ps, maximum ps)

showVoiceStats :: Voice -> String
showVoiceStats v = let (mn,mx) = voiceStats v in show mn ++ '\t' : show mx

showMidiStats :: FilePath -> IO ()
showMidiStats fp = do ms <- readMidiScore fp
                      putStr (fp ++ "\t")
                      putStrLn . intercalate "\t"  . map showVoiceStats 
                               . getVoices $ ms 
                                        
mergeTracks :: MidiScore -> MidiScore
mergeTracks ms = ms {getVoices = [sort . map (fmap setChan) . concat . getVoices $ ms]} 
  where setChan n = n {channel = 1}