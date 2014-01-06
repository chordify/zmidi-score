{-# OPTIONS_GHC -Wall #-}
module Main where

import MidiCommonIO 

import ZMidiBasic            
import MelFind                      ( getAccompQuant, findMelodyQuant )
import TimeSigSeg
import InnerMetricalAnalysis hiding ( Time )
import qualified InnerMetricalAnalysis as IMA ( Time (..) )        
-- import Math.Statistics              ( pearson )
import System.Environment           ( getArgs )
-- import Data.List.Ordered            ( nub )
import Data.Maybe                   ( isJust )

type IMAMatch = Float
type Pattern  = (Int, Float)

matchMeterIMA :: ShortestNote -> MidiScore -> [(Int, Bool, Double)]
matchMeterIMA q ms = 
  let acc = getAccompQuant q ms 
      -- ts  = getTimeSig ms
      ons = toOnsets acc
      ws  = getSpectralWeight (Period . getMinDur . buildTickMap $ [acc]) 
           . map IMA.Time $ ons
      mx  = fromIntegral . maximum . map snd $ ws
  in  match mx ws ons

matchIMA :: ShortestNote -> MidiScore -> [(Int, Bool, Double)]
matchIMA q ms = 
  let acc = getAccompQuant q ms 
      -- ts  = getTimeSig ms
      ons = toOnsets acc
      ws  = getSpectralWeight (Period . getMinDur . buildTickMap $ [acc]) 
           . map IMA.Time $ ons
      mx  = fromIntegral . maximum . map snd $ ws
  in  match mx ws ons
  
match :: Double -> [(Int, SWeight)] -> [Int] -> [(Int, Bool, Double)]
match _ [] [] = []
match m ((g, w):t) [] =              (g, False, fromIntegral w / m) : match m t []
match m ((g, w):t) (o:os) | g <  o = (g, False, fromIntegral w / m) : match m t (o:os)
                          | g == o = (g, True , fromIntegral w / m) : match m t os
                          | otherwise = error "unmatched onset"
match _ _ _ = error "list of unequal lengths"             

preProcessMidi :: ShortestNote -> MidiScore -> [IMA.Time]
preProcessMidi q ms = map IMA.Time . toOnsets . getAccompQuant q $ ms

-- normalise :: (Num a, Fractional b) => [a] -> [b]
normalise :: [Int] -> [Double]
normalise l = let m = fromIntegral $ maximum l 
              in map ((/ m) . fromIntegral) (l :: [Int])
             
              
              
genPat :: Int -> TimeSig -> (Int -> Float)
genPat t ts = case ts of
        (TimeSig 2 2 _ _) -> project t 2 1 1
        (TimeSig 2 4 _ _) -> project t 2 1 1
        (TimeSig 4 4 _ _) -> project t 4 2 1
        _                 -> error "unexpected pattern"

-- project :: Time -> Time -> Time -> Time -> (Time -> Float)
project :: Int -> Int -> Int -> Int -> (Int -> Float)
project t prim sec trt x = case x `mod` (prim * t) of
                             0 -> 1.0
                             _ -> case x `mod` (sec * t) of 
                                    0 -> 0.5 
                                    _ -> case x `mod` (trt * t) of 
                                          0 -> 0.2 
                                          _ -> 0.0

testBeatBar :: FilePath -> IO ()
testBeatBar fp = do ms <- readMidiScore fp
                    let ts  = getTimeSig ms
                        tpb = ticksPerBeat ms
                    print ts
                    -- print . map (updTimeSig tpb) $ ts
                    -- print tpb
                    -- mapM_ print . map (timeSigChange tpb) $ ts
                    _ <- sequence $ zipWith writeMidiScore (segByTimeSig ms) (map (: ".mid") "1234567890")
                    putStrLn "Done"

                    
toStar :: TimeSig -> Time -> (Int, Bool, Double) -> IO ()
toStar ts tpb (g,o,d) = let (bar, bt) = getBeatInBar ts tpb g
                        in  putStrLn (show g ++ " " ++ show bar ++ " "
                              ++ (maybe " " show bt)
                              ++ (if o then " x " else "   ") 
                              ++ replicate (round (20 * d)) '*' )
                    
main :: IO ()
main = do arg <- getArgs 
          case arg of
            [fp] -> do ms <- readMidiScore fp 
                       print . map IMA.time . preProcessMidi Sixteenth $ ms
                       print . getMinDur . buildTickMap $ [getAccompQuant Sixteenth ms]
                       mapM_ (toStar (getEvent . head . getTimeSig $ ms) (ticksPerBeat ms)) . matchIMA Sixteenth $ ms
            _    -> error "Please provide a path to a midifile"