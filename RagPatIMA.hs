{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import ZMidiBasic            
import MidiCommonIO                 ( readMidiScore )
import MelFind                      ( getAccompQuant, mergeTracks )
import TimeSigSeg
import InnerMetricalAnalysis hiding ( Time )
import qualified InnerMetricalAnalysis as IMA ( Time (..) )        
-- import Math.Statistics              ( pearson )
import System.Environment           ( getArgs )
import Data.List            ( nubBy, intercalate, foldl' )
import Data.Function                ( on )
import Data.IntMap.Lazy             ( empty, IntMap, insertWith, foldrWithKey )
import qualified Data.IntMap.Lazy  as M ( Key )

import Debug.Trace
-- type Pattern  = (Int, Float)

-- | Normalised spectral weights (value between 0 and 1)
newtype NSWeigth = NSWeigth { nsweight :: Double }
                     deriving ( Eq, Show, Num, Ord, Enum, Real, Floating
                              , Fractional, RealFloat, RealFrac )
                              
type NSWMeterSeg = TimedSeg TimeSig [Timed (Maybe ScoreEvent, NSWeigth)]

matchMeterIMA :: ShortestNote -> MidiScore -> [NSWMeterSeg]
matchMeterIMA q ms = 
  let -- quantise the merge all tracks and remove nub notes with the same onset
      -- TODO : we should be able to use Data.List.Ordered, but this nub give
      -- other results, this must be investigated
      v   = nubBy ((==) `on` onset) 
          . head . getVoices . mergeTracks . quantise q $ ms
      -- calculate the minimal beat duration
      mn  = Period . getMinDur . buildTickMap $ [v]
      -- calculate the spectral weights
      ws  = getSpectralWeight mn . map IMA.Time . toOnsets $ v
      -- calculate the maximum weight
      mx  = NSWeigth . fromIntegral . maximum . map snd $ ws
      -- split the midi file per 
  in map normaliseTime $ segment (getTimeSig ms) (match mx ws v) 

-- | matches a grid with spectral weights with the onsets that created the
-- weights. The first argument is the maximum 'Weight' found among the weights
match :: NSWeigth -> [(Int, SWeight)] -> Voice -> [Timed (Maybe ScoreEvent, NSWeigth)]
match _ [] []              = []
match m ((g, w):ws) []     =          addWeight m w (Left g) : match m ws []
match m ((g, w):ws) (t:ts) | g <  o = addWeight m w (Left g) : match m ws (t:ts)
                           | g == o = addWeight m w (Right t): match m ws ts
                           | otherwise = error "unmatched onset"
                               where o = onset t
match _ _ _                = error "list of unequal lengths"             

addWeight :: NSWeigth -> SWeight -> Either Int (Timed ScoreEvent) 
          -> Timed (Maybe ScoreEvent, NSWeigth)
addWeight m w e = either ((flip Timed) (Nothing, w')) f e
  where w'  = fromIntegral w / m
        f t = t {getEvent = (Just $ getEvent t, w')}

-- normalise :: (Num a, Fractional b) => [a] -> [b]
normalise :: [Int] -> [Double]
normalise l = let m = fromIntegral $ maximum l 
              in map ((/ m) . fromIntegral) (l :: [Int])

--
starMeter :: Time -> NSWMeterSeg -> IO ()
starMeter tpb (TimedSeg (Timed t ts) s) = 
  do putStrLn (show t ++ " ================== " ++ show ts ++ " ==================" )
     mapM_ (toStar t ts) s where
                    
  -- prints one line e.g. "1152 1 3 1C  ***************"
  toStar :: Time -> TimeSig -> Timed (Maybe ScoreEvent, NSWeigth) -> IO ()
  toStar os x (Timed g (se,w)) = 
    let (bar, bt) = getBeatInBar x tpb g
    in  putStrLn (show (g+os) ++ " " ++ show bar ++ " " ++ (maybe " "  show bt)
                              ++ " " ++ (maybe "   " (show . pitch) se)
                              ++ " " ++ stars w)

stars :: NSWeigth -> String
stars w = replicate (round (20 * w)) '*' 
--------------------------------------------------------------------------------
-- IMA profiles
--------------------------------------------------------------------------------

-- data NSWProf = NSWProf { pTimeSig :: TimeSig 
                       -- , pWeights :: [(Int, NSWeigth)]
                       -- } deriving (Show, Eq)
                       
type NSWProf     = TimedSeg TimeSig (NrOfBars, IntMap NSWeigth)
newtype NrOfBars = NrOfBars  { nrOfBars :: Int }
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral )
                       
toNSWProf :: Time ->  NSWMeterSeg -> NSWProf
toNSWProf tpb (TimedSeg ts s) = TimedSeg ts (foldl' toProf (1,empty) s) where

  toProf :: (NrOfBars, IntMap NSWeigth) -> Timed (Maybe ScoreEvent, NSWeigth)
         -> (NrOfBars, IntMap NSWeigth)
  toProf (b, m) (Timed g (_se,w)) = 
    let (bar, jbt) = getBeatInBar (getEvent ts) tpb g
    in case jbt of 
         Just bt -> (NrOfBars bar, insertWith (+) bt w m)
         Nothing -> (b , m)

showNSWProf :: NSWProf -> String
showNSWProf (TimedSeg ts (bars, m)) = intercalate "\n" ( hdr : foldrWithKey shw [] m )

  where shw :: M.Key -> NSWeigth -> [String] -> [String]
        shw bt w r = (show bt ++ ": " ++ stars (w / fromIntegral bars)) : r
        
        hdr = "\n" ++ show ts ++ "\n"


-- testing
main :: IO ()
main = do arg <- getArgs 
          case arg of
            [fp] -> do ms <- readMidiScore fp 
                       -- print . map IMA.time . preProcessMidi Sixteenth $ ms
                       print . getMinDur . buildTickMap $ [getAccompQuant Sixteenth ms]
                       let seg =  matchMeterIMA Sixteenth ms
                           tpb = ticksPerBeat ms
                       mapM_ (starMeter tpb) seg
                       mapM_ (putStrLn . showNSWProf . toNSWProf tpb) seg
            _    -> error "Please provide a path to a midifile"