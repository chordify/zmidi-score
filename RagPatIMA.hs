{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import ZMidiBasic            
import MidiCommonIO                 ( readMidiScore, readMidiScoreSafe, mapDirInDir, mapDir, foldrDir )
import MelFind                      ( getAccompQuant, mergeTracks )
import TimeSigSeg
import InnerMetricalAnalysis hiding ( Time )
import qualified InnerMetricalAnalysis as IMA ( Time (..) )        
-- import Math.Statistics              ( pearson )
import System.Environment           ( getArgs )
import Data.List            ( nubBy, intercalate, foldl' )
import Data.Function                ( on )
import Data.Maybe                   ( catMaybes )
import Data.Map.Strict             ( empty, Map, insertWith, foldrWithKey, unionWith, toList  )

-- | Normalised spectral weights (value between 0 and 1)
newtype NSWeight = NSWeight { nsweight :: Double }
                     deriving ( Eq, Show, Num, Ord, Enum, Real, Floating
                              , Fractional, RealFloat, RealFrac )
                              
type NSWMeterSeg = TimedSeg TimeSig [Timed (Maybe ScoreEvent, NSWeight)]

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
      mx  = NSWeight . fromIntegral . maximum . map snd $ ws
      -- split the midi file per 
  in map normaliseTime $ segment (getTimeSig ms) (match mx ws v) 

-- | matches a grid with spectral weights with the onsets that created the
-- weights. The first argument is the maximum 'Weight' found among the weights
match :: NSWeight -> [(Int, SWeight)] -> Voice -> [Timed (Maybe ScoreEvent, NSWeight)]
match _ [] []              = []
match m ((g, w):ws) []     =          addWeight m w (Left g) : match m ws []
match m ((g, w):ws) (t:ts) | g <  o = addWeight m w (Left g) : match m ws (t:ts)
                           | g == o = addWeight m w (Right t): match m ws ts
                           | otherwise = error "unmatched onset"
                               where o = onset t
match _ _ _                = error "list of unequal lengths"             

addWeight :: NSWeight -> SWeight -> Either Int (Timed ScoreEvent) 
          -> Timed (Maybe ScoreEvent, NSWeight)
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
  toStar :: Time -> TimeSig -> Timed (Maybe ScoreEvent, NSWeight) -> IO ()
  toStar os x (Timed g (se,w)) = 
    let (bar, bt) = getBeatInBar x tpb g
    in  putStrLn (show (g+os) ++ " " ++ show bar ++ " " ++ (maybe " "  show bt)
                              ++ " " ++ (maybe "   " (show . pitch) se)
                              ++ " " ++ stars w)

stars :: NSWeight -> String
stars w = replicate (round (20 * w)) '*' 
--------------------------------------------------------------------------------
-- IMA profiles
--------------------------------------------------------------------------------

-- | Normalised Spectral Weight Profiles
type NSWProfSeg  = TimedSeg TimeSig NSWProf
type NSWProf     = (NrOfBars, Map Int NSWeight)

-- | Stores the number of bars
newtype NrOfBars = NrOfBars  { nrOfBars :: Int }
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral )

toNSWProfSegs :: MidiScore -> [NSWProfSeg]
toNSWProfSegs m = map (toNSWProf (ticksPerBeat m)) . matchMeterIMA Sixteenth $ m

-- | Calculates sums the NSW profiles for a meter section
toNSWProf :: Time ->  NSWMeterSeg -> NSWProfSeg
toNSWProf tpb (TimedSeg ts s) = TimedSeg ts (foldl' toProf (1,empty) s) where

  toProf :: NSWProf -> Timed (Maybe ScoreEvent, NSWeight) -> NSWProf
  toProf (b, m) (Timed g (_se,w)) = case getBeatInBar (getEvent ts) tpb g of 
     (bar, Just bt) -> (NrOfBars bar, insertWith (+) bt w m)
     (_  , Nothing) -> (b , m)

-- | Plots an 'NSWProf'ile by calculating the average profile
showNSWProf :: (TimeSig, NSWProf) -> String
showNSWProf (ts, (bars, m)) = intercalate "\n" ( show ts : foldrWithKey shw [] m )

  where shw :: Int -> NSWeight -> [String] -> [String]
        shw bt w r = (show bt ++ ": " ++ stars (w / fromIntegral bars)) : r

-- | Collects all profiles sorted by time signature in one map
collectNSWProf :: [NSWProfSeg] -> Map TimeSig NSWProf -> Map TimeSig NSWProf
collectNSWProf s m = foldr doSeg m s where

  doSeg :: NSWProfSeg -> Map TimeSig NSWProf -> Map TimeSig NSWProf
  doSeg (TimedSeg ts p) m = insertWith mergeNSWProf (getEvent ts) p m
  
-- | merges two 'NSWProf's by summing its values
mergeNSWProf :: NSWProf -> NSWProf -> NSWProf
mergeNSWProf (a, ma) (b, mb) = let m = unionWith (+) ma mb in m `seq` (a + b, m)  

  
-- testing
main :: IO ()
main = 
  do arg <- getArgs 
     case arg of
       ["-f", fp] -> do ms <- readMidiScore fp 
                        print . getMinDur . buildTickMap $ [getAccompQuant Sixteenth ms]
                        let s   =  matchMeterIMA Sixteenth ms
                            tpb = ticksPerBeat ms
                        mapM_ (starMeter tpb) s
                        mapM_ (putStrLn . showNSWProf) . toList 
                            . collectNSWProf (map (toNSWProf tpb) s) $ empty
                            
       ["-d", fp] -> do foldrDir readProf empty fp 
                            >>= mapM_ (putStrLn . showNSWProf) . toList 
                        
       _    -> error "Please use -f <file> or -d <ragtime directory>"
       
       
readProf :: FilePath -> Map TimeSig NSWProf -> IO (Map TimeSig NSWProf)
readProf fp m = do ms <- readMidiScoreSafe fp
                   putStrLn fp
                   case ms of
                     Just x  -> return . collectNSWProf (toNSWProfSegs x) $! m 
                     Nothing -> return m