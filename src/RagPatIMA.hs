{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import ZMidi.Score         hiding ( numerator, denominator )
import ZMidi.IO.Common            ( readQMidiScoreSafe, mapDirInDir, mapDir, warning )
import ZMidi.Skyline.MelFind      ( mergeTracks )
import Ragtime.TimeSigSeg
import Ragtime.MidiIMA
import Ragtime.NSWProf

import IMA.InnerMetricalAnalysis hiding           ( Time(..) )
import qualified IMA.InnerMetricalAnalysis as IMA ( Time(..) )

import System.Environment          ( getArgs )
import Data.List                   ( nubBy, foldl' )
import Data.Ratio                  ( numerator, denominator )
import Data.Function               ( on )
import Data.Map.Strict             ( empty, Map, insertWith, unionWith, toList )
import Control.Arrow               ( first )
import Text.Printf                 ( printf )



-- 
starMeter :: Time -> NSWMeterSeg -> IO ()
starMeter tpb (TimedSeg (Timed t ts) s) = 
  do putStrLn . printf ("%6d: ======================= " ++ show ts 
                         ++ " =======================" ) $ t
     mapM_ (toStar t ts) s where
                    
  -- prints one line e.g. "1152 1 3 1C  ***************"
  toStar :: Time -> TimeSig -> Timed (Maybe ScoreEvent, NSWeight) -> IO ()
  toStar os x (Timed g (se,w)) = 
    let (b, BarRat r) = getBeatInBar x tpb g
    in putStrLn (printf ("%6d: %3d - %2d / %2d: " ++ showMSE se ++ ": " ++ stars w) 
                (g+os) b (numerator r) (denominator r)) 
                
  showMSE :: Maybe ScoreEvent -> String
  showMSE = maybe "    " (show . pitch) 



    

-- testing
main :: IO ()
main = 
  do arg <- getArgs 
     case arg of
       ["-f", fp] -> do x <- readQMidiScoreSafe FourtyEighth fp 
                        let ms = either error id x
                        -- does the file contains disruptive onsets?
                        case doIMA ms of 
                          Right s -> 
                            do let tpb = ticksPerBeat (qMidiScore ms)
                               -- putStrLn ("Ticks per beat: " ++ show tpb)
                               putStrLn (printf "Quantisation deviation: %.4f" (avgQDevQMS ms))
                               mapM_ (starMeter tpb) s
                               printMeterStats . collectNSWProf 
                                  (map (toNSWProf tpb) s) $ empty
                          Left e      -> putStrLn e -- show the error
                            
       ["-a", fp] -> mapDirInDir (\x -> mapDir readProf x >>= unionNWProfMaps) fp
                            >>= unionNWProfMaps >>= writeNSWProf "nswProf.bin" 
                            >>= printMeterStats
                            
       ["-d", fp] -> mapDir readProf fp >>= unionNWProfMaps 
                        >>= writeNSWProf "nswProf.bin" >>= printMeterStats    
                        
       ["-m", fp] -> undefined
                        
       _    -> error "Please use -f <file> or -d <ragtime directory>"
   
   
-- combines two inner metrical analysis maps into one, summing all results
unionNWProfMaps :: [Map TimeSig NSWProf] -> IO (Map TimeSig NSWProf)
unionNWProfMaps m = do let r = foldr (unionWith mergeNSWProf) empty m
                       r `seq` return r

-- Prints the average normalised inner metric analysis profiles to the user
printMeterStats :: Map TimeSig NSWProf -> IO ()
printMeterStats = mapM_ (putStrLn . showNSWProf) . toList 
   
printSongStats :: QMidiScore -> IO ()
printSongStats m = let s = "q: %.3f ts: " ++ (show . getTimeSig . qMidiScore $ m)
                   in  putStrLn . printf s . avgQDevQMS $ m
   
-- Reads a file and does an inner metric analysis per time signature segment
readProf :: FilePath -> IO (Map TimeSig NSWProf)
readProf fp = do qm <- readQMidiScoreSafe FourtyEighth fp 
                 case qm >>= qMidiScoreToNSWProfMaps of
                   Right w -> do putStrLn fp 
                                 -- either error printSongStats qm
                                 -- printMeterStats w
                                 qm `seq` w `seq` return w
                   Left  e -> warning fp e >> return empty
                 
-- Transforms quantised midi into an inner metric analysis or a failure warning
qMidiScoreToNSWProfMaps :: QMidiScore -> Either String (Map TimeSig NSWProf)
qMidiScoreToNSWProfMaps qms =     timeSigCheck qms 
                              >>= toNSWProfSegs
                              >>= (\x -> return $ collectNSWProf x empty)
       
-- Checks for a valid time siganture
timeSigCheck :: QMidiScore -> Either String QMidiScore
timeSigCheck ms | hasTimeSigs (qMidiScore ms) = Right ms
                | otherwise = Left "Has no valid time signature"


 