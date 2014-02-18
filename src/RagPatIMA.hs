{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import ZMidi.Score.Datatypes ( TimeSig (..), hasTimeSigs, getTimeSig )
import ZMidi.Score.Quantise  ( QMidiScore (..), ShortestNote (..), avgQDevQMS
                             , toQBins, qToQBins )
import ZMidi.IO.Common       ( readQMidiScoreSafe, mapDirInDir, mapDir, warning)
import Ragtime.MidiIMA
import Ragtime.NSWProf       ( vectorizeAll, SWProf, writeNSWProf, readNSWProf
                             , mergeSWProf, showNSWProf, NSWeight, normSWProf 
                             , NSWProf )

import System.Environment    ( getArgs )
import Data.Map.Strict       ( empty, Map, unionWith, toList )
import qualified Data.Map.Strict as M ( map )
import Data.Vector           ( Vector )
import Data.List             ( intercalate )
import Control.Monad         ( void )
import Text.Printf           ( printf )

-- testing
main :: IO ()
main = 
  do arg <- getArgs 
     let meters = [ TimeSig 4 4 0 0
                  , TimeSig 2 4 0 0
                  , TimeSig 2 2 0 0
                  , TimeSig 3 4 0 0
                  , TimeSig 6 8 0 0
                  ]
         profIn = "ragtimeMeterProfiles_2013-02-11.bin"
     case arg of
       ["-f", fp] -> readQMidiScoreSafe FourtyEighth fp 
                        >>= return . either error id >>= printIMA
                        >>= return . M.map normSWProf . (flip collectSWProf) empty
                        >>= printMeterStats
                            
       ["-a", fp] -> mapDirInDir (\x -> mapDir readProf x >>= unionSWProfMaps) fp
                        >>= unionSWProfMaps >>= return . M.map normSWProf 
                        >>= writeNSWProf "nswProf.bin" >>= printMeterStats
                            
       ["-d", fp] -> mapDir readProf fp >>= unionSWProfMaps 
                        >>= return . M.map normSWProf
                        >>= writeNSWProf "nswProf.bin" >>= printMeterStats    
                        
       ["-m", fp] -> do qm <- readQMidiScoreSafe FourtyEighth fp
                                >>= return . either error id 
                        m  <- readNSWProf profIn
                                >>= return . selectMeters meters
                        let m' = vectorizeAll (qToQBins qm) m
                        either error (mapM_ print) . matchMeters m' $ qm

       ["-c", fp] -> do m  <- readNSWProf profIn
                                >>= return . selectMeters meters
                        mc <- readQMidiScoreSafe FourtyEighth fp 
                                >>= return . (>>= meterCheck m)
                                >>= return . either error id 
                        mapM_ (putStrLn . printMeterMatchVerb (toQBins FourtyEighth)) mc
       
       ["-r", fp] -> do m  <- readNSWProf profIn
                                >>= return . selectMeters meters
                        void . mapDirInDir (mapDir (dirMeterMatch m)) $ fp

       ["-p"    ] ->    readNSWProf profIn >>= return . selectMeters meters
                                           >>= printMeterStats

       _    -> error "Please use -f <file> or -d <ragtime directory>"
   
   
-- combines two inner metrical analysis maps into one, summing all results
unionSWProfMaps :: [Map TimeSig SWProf] -> IO (Map TimeSig SWProf)
unionSWProfMaps m = do let r = foldr (unionWith mergeSWProf) empty m
                            -- step a b = unionWith mergeProf a b
                       r `seq` return r

-- Prints the average normalised inner metric analysis profiles to the user
printMeterStats :: Map TimeSig NSWProf -> IO ()
printMeterStats = mapM_ (putStrLn . showNSWProf) . toList 
   
printSongStats :: QMidiScore -> IO ()
printSongStats m = let s = "q: %.3f ts: " ++ (show . getTimeSig . qMidiScore $ m)
                   in  putStrLn . printf s . avgQDevQMS $ m
   
-- Reads a file and does an inner metric analysis per time signature segment
readProf :: FilePath -> IO (Map TimeSig SWProf)
readProf fp = do qm <- readQMidiScoreSafe FourtyEighth fp 
                 case qm >>= qMidiScoreToSWProfMaps of
                   Right w -> do putStrLn fp 
                                 -- either error printSongStats qm
                                 return (M.map normSWProf w) >>= printMeterStats 
                                 qm `seq` w `seq` return w
                   Left  e -> warning fp e >> return empty
                 
-- Transforms quantised midi into an inner metric analysis or a failure warning
qMidiScoreToSWProfMaps :: QMidiScore -> Either String (Map TimeSig SWProf)
qMidiScoreToSWProfMaps qm =    timeSigCheck qm 
                           >>= toSWProfSegs
                           >>= (\x -> return $ collectSWProf x empty)

dirMeterMatch :: Map TimeSig NSWProf -> FilePath -> IO ()
dirMeterMatch m fp = readQMidiScoreSafe FourtyEighth fp 
                       >>= return . (>>= doMeterMatch (vectorizeAll (toQBins FourtyEighth) m))
                       >>= either (warning fp) putStrLn

  where doMeterMatch :: [(TimeSig, Vector NSWeight)] -> QMidiScore 
                     -> Either String String
        doMeterMatch m' qm = timeSigCheck qm 
                          >>= matchMeters m' >>= pickMeters
                          >>= return . intercalate "\n" 
                                     . map (\x -> fp ++ "\t" ++ printPickMeter x)
                              
-- Checks for a valid time signature
timeSigCheck :: QMidiScore -> Either String QMidiScore
timeSigCheck ms | hasTimeSigs (qMidiScore ms) = Right ms
                | otherwise = Left "Has no valid time signature" 

                
                
 