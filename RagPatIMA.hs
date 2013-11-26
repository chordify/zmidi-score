{-# OPTIONS_GHC -Wall #-}
module Main where

import MidiCommonIO 

import ZMidiBasic            hiding ( Time )
import MelFind                      ( getAccompQuant )
import InnerMetricalAnalysis hiding ( main )
import LocalMeter           
-- import Math.Statistics              ( pearson )
import System.Environment           ( getArgs )

type IMAMatch = Float
type Pattern  = (Int, Float)

matchIMA :: ShortestNote -> MidiScore -> [(Int, Bool, Double)]
matchIMA q ms = 
  let acc = getAccompQuant q ms 
      ons = toOnsets acc
      ws  = getSpectralWeight (Period . getMinDur . buildTickMap $ [acc]) 
           . map Time $ ons
      mx  = fromIntegral . maximum . map snd $ ws
  in  match mx ws ons
  
match :: Double -> [(Int, Weight)] -> [Int] -> [(Int, Bool, Double)]
match _ [] [] = []
match m ((g, w):t) [] =              (g, False, fromIntegral w / m) : match m t []
match m ((g, w):t) (o:os) | g <  o = (g, False, fromIntegral w / m) : match m t (o:os)
                          | g == o = (g, True , fromIntegral w / m) : match m t os
                          | otherwise = error "unmatched onset"
match _ _ _ = error "list of unequal lengths"
                          

preProcessMidi :: ShortestNote -> MidiScore -> [Time]
preProcessMidi q ms = map Time . toOnsets . getAccompQuant q $ ms

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

-- printWeight :: [Double] -> IO ()
-- printWeight = undefined

-- match :: [Int] -> [Int] -> [Double] -> [(Int, Bool, Double)]
-- match grid ons = undefined -- zipWith (\g o -> (g 

toStar :: (Int, Bool, Double) -> IO ()
toStar (g,o,d) = putStrLn (show g ++ (if o then " x " else "   ") 
                                  ++ replicate (round (20 * d)) '*' )
                                          
main :: IO ()
main = do arg <- getArgs 
          case arg of
            [fp] -> do ms <- readMidiScore fp 
                       print . map time . preProcessMidi Sixteenth $ ms
                       print . getMinDur . buildTickMap $ [getAccompQuant Sixteenth ms]
                       mapM_ toStar . matchIMA Sixteenth $ ms
            _    -> error "Please provide a path to a midifile"