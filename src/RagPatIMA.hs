{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import ZMidi.Score.Datatypes ( TimeSig, hasTimeSigs, getTimeSig )
import ZMidi.Score.Quantise  ( QMidiScore (..), ShortestNote (..), avgQDevQMS )
import ZMidi.IO.Common       ( readQMidiScoreSafe, mapDirInDir, mapDir, warning)
import Ragtime.MidiIMA
import Ragtime.NSWProf

import System.Environment    ( getArgs )
import Data.Map.Strict       ( empty, Map, unionWith, toList )
import Control.Monad         ( void )
import Text.Printf           ( printf )

-- testing
main :: IO ()
main = 
  do arg <- getArgs 
     case arg of
       ["-f", fp] -> readQMidiScoreSafe FourtyEighth fp 
                        >>= return . either error id >>= void . printIMA 
                            
       ["-a", fp] -> mapDirInDir (\x -> mapDir readProf x >>= unionNWProfMaps) fp
                        >>= unionNWProfMaps >>= writeNSWProf "nswProf.bin" 
                        >>= printMeterStats
                            
       ["-d", fp] -> mapDir readProf fp >>= unionNWProfMaps 
                        >>= writeNSWProf "nswProf.bin" >>= printMeterStats    
                        
       ["-m", fp] -> do qm <- readQMidiScoreSafe FourtyEighth fp
                                >>= return . either error id 
                        m  <- readNSWProf "ragtimeMeterProfiles_2013-01-30.bin" 
                                >>= return . toNSWVecSeg (qGridUnit qm)
                        void . printIMA $ qm 
                        print . findMeter m $ qm
                        
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
                
 