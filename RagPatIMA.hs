{-# OPTIONS_GHC -Wall #-}
module Main where

import ZMidiBasic            
import MidiCommonIO                 ( readMidiScore )
import MelFind                      ( getAccompQuant, mergeTracks )
import TimeSigSeg
import InnerMetricalAnalysis hiding ( Time )
import qualified InnerMetricalAnalysis as IMA ( Time (..) )        
-- import Math.Statistics              ( pearson )
import System.Environment           ( getArgs )
import Data.List            ( nubBy )
import Data.Function                ( on )

type IMAMatch = Float
type Pattern  = (Int, Float)

matchMeterIMA :: ShortestNote -> MidiScore 
              -> [TimedSeg TimeSig [Timed (Maybe ScoreEvent, Double)]]
matchMeterIMA q ms = 
  let -- quantise the merge all tracks and remove nub notes with the same onset
      v   = nubBy ((==) `on` onset) 
          . head . getVoices . mergeTracks . quantise q $ ms
      -- calculate the minimal beat duration
      mn  = Period . getMinDur . buildTickMap $ [v]
      -- calculate the spectral weights
      ws  = getSpectralWeight mn . map IMA.Time . toOnsets $ v
      -- calculate the maximum weight
      mx  = fromIntegral . maximum . map snd $ ws
      -- split the midi file per 
  in map normaliseTime $ segment (getTimeSig ms) (match mx ws v) 

-- | matches a grid with spectral weights with the onsets that created the
-- weights. The first argument is the maximum 'Weight' found among the weights
match :: Double -> [(Int, SWeight)] -> Voice -> [Timed (Maybe ScoreEvent, Double)]
match _ [] []              = []
match m ((g, w):ws) []     =          addWeight m w (Left g) : match m ws []
match m ((g, w):ws) (t:ts) | g <  o = addWeight m w (Left g) : match m ws (t:ts)
                           | g == o = addWeight m w (Right t): match m ws ts
                           | otherwise = error "unmatched onset"
                               where o = onset t
match _ _ _                = error "list of unequal lengths"             

addWeight :: Double -> SWeight -> Either Int (Timed ScoreEvent) 
          -> Timed (Maybe ScoreEvent, Double)
addWeight m w e = either ((flip Timed) (Nothing, w')) f e
  where w'  = fromIntegral w / m
        f t = t {getEvent = (Just $ getEvent t, w')}

-- normalise :: (Num a, Fractional b) => [a] -> [b]
normalise :: [Int] -> [Double]
normalise l = let m = fromIntegral $ maximum l 
              in map ((/ m) . fromIntegral) (l :: [Int])
             
{-                            
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
-}

--
starMeter :: Time -> (TimedSeg TimeSig [Timed (Maybe ScoreEvent, Double)]) -> IO ()
starMeter tpb (TimedSeg (Timed t ts) s) = 
  do putStrLn (show t ++ " ================== " ++ show ts ++ " ==================" )
     mapM_ (toStar t ts) s where
                    
  -- prints one line e.g. "1152 1 3 1C  ***************"
  toStar :: Time -> TimeSig -> Timed (Maybe ScoreEvent, Double) -> IO ()
  toStar os x (Timed g (se,w)) = 
    let (bar, bt) = getBeatInBar x tpb g
    in  putStrLn (show (g+os) ++ " " ++ show bar ++ " " ++ (maybe " "  show bt)
                              ++ " " ++ (maybe "   " (show . pitch) se)
                              ++ " " ++ replicate (round (20 * w)) '*' )

-- testing
main :: IO ()
main = do arg <- getArgs 
          case arg of
            [fp] -> do ms <- readMidiScore fp 
                       -- print . map IMA.time . preProcessMidi Sixteenth $ ms
                       print . getMinDur . buildTickMap $ [getAccompQuant Sixteenth ms]
                       mapM_ (starMeter (ticksPerBeat ms)) . matchMeterIMA Sixteenth $ ms
            _    -> error "Please provide a path to a midifile"