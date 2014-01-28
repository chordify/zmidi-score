{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import ZMidi.Score         
import ZMidi.IO.Common          ( readMidiScore, readMidiScoreSafe, putErrStrLn
                                    , mapDirInDir, mapDir, foldrDir, foldrDirInDir )
import ZMidi.Skyline.MelFind                      ( getAccompQuant, mergeTracks )
import Ragtime.TimeSigSeg
import IMA.InnerMetricalAnalysis hiding ( Time )
import qualified IMA.InnerMetricalAnalysis as IMA ( Time (..) )        
-- import Math.Statistics              ( pearson )
import System.Environment           ( getArgs )
import Data.List            ( nubBy, intercalate, foldl' )
import Data.Ratio                   ( Ratio )
import Data.Function                ( on )
import Data.Maybe                   ( catMaybes )
import Data.Map.Strict             ( empty, Map, insertWith, foldrWithKey, unionWith, toList  )
import Control.Arrow               ( first, second )
import Data.Foldable      ( foldrM )

-- | Normalised spectral weights (value between 0 and 1)
newtype NSWeight = NSWeight { nsweight :: Double }
                     deriving ( Eq, Show, Num, Ord, Enum, Real, Floating
                              , Fractional, RealFloat, RealFrac )
                              
type NSWMeterSeg = TimedSeg TimeSig [Timed (Maybe ScoreEvent, NSWeight)]

-- TODO create a MPMidiScore for monophonic MidiScores
-- TODO create a QMPMidiScore for quantised monophonic MidiScores

matchMeterIMA :: ShortestNote -> MidiScore -> Either String (Float, [NSWMeterSeg])
matchMeterIMA sn = either Left matchMeterQIMA . quantiseSafe sn

-- before applying matchMeter 
matchMeterQIMA :: QMidiScore -> Either String (Float, [NSWMeterSeg])
matchMeterQIMA (QMidiScore msq _sn gu d) = 
  let -- quantise the merge all tracks and remove nub notes with the same onset
      -- (msq, d, gu) = quantiseDev q $ ms
      dev = (fromIntegral d / fromIntegral (nrOfNotes msq)) / fromIntegral gu 
      ms' = mergeTracks msq
      -- remove duplicates
      -- TODO : we should be able to use Data.List.Ordered, but this nub give
      -- other results, this must be investigated
      v   = nubBy ((==) `on` onset) . head . getVoices $ ms'
      ons = map fromIntegral . toOnsets $ v
      -- calculate the spectral weights
  in case addMaxPerCheck (getSpectralWeight (fromIntegral . minDur $ ms')) ons of
      Right w ->     -- calculate the maximum weight
                 let mx = NSWeight . fromIntegral . maximum . map snd $ w
                     m  = match mx (map (first Time) w) v
                     -- split the midi file per 
                 in  Right (dev, map normaliseTime $ segment (getTimeSig ms') m)
      Left e  -> Left e

-- | matches a grid with spectral weights with the onsets that created the
-- weights. The first argument is the maximum 'Weight' found among the weights
match :: NSWeight -> [(Time, SWeight)] -> Voice -> [Timed (Maybe ScoreEvent, NSWeight)]
match _ [] []              = []
match m ((g, w):ws) []     =          addWeight m w (Left g) : match m ws []
match m ((g, w):ws) (t:ts) | g <  o = addWeight m w (Left g) : match m ws (t:ts)
                           | g == o = addWeight m w (Right t): match m ws ts
                           | otherwise = error "unmatched onset"
                               where o = onset t
match _ _ _                = error "list of unequal lengths"             

addWeight :: NSWeight -> SWeight -> Either Time (Timed ScoreEvent) 
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
    in  putStrLn (show (g+os) ++ " " ++ show bar ++ " " ++ show bt
                              ++ " " ++ (maybe "   " (show . pitch) se)
                              ++ " " ++ stars w)

stars :: NSWeight -> String
stars w = replicate (round (20 * w)) '*' 
--------------------------------------------------------------------------------
-- IMA profiles
--------------------------------------------------------------------------------

-- | Normalised Spectral Weight Profiles
type NSWProfSeg  = TimedSeg TimeSig NSWProf
type NSWProf     = (NrOfBars, Map (Ratio Time) NSWeight)

-- | Stores the number of bars
newtype NrOfBars = NrOfBars  { nrOfBars :: Int }
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral )

toNSWProfSegs :: MidiScore -> Either String (Float, [NSWProfSeg])
toNSWProfSegs m = fmap (second (map (toNSWProf (ticksPerBeat m)))) (matchMeterIMA FourtyEighth m )

-- | Calculates sums the NSW profiles for a meter section
toNSWProf :: Time ->  NSWMeterSeg -> NSWProfSeg
toNSWProf tpb (TimedSeg ts s) = TimedSeg ts (foldl' toProf (1,empty) s) where

  toProf :: NSWProf -> Timed (Maybe ScoreEvent, NSWeight) -> NSWProf
  toProf (b, m) (Timed g (_se,w)) = 
    let (bar, bt) = getBeatInBar (getEvent ts) tpb g 
        m'        = insertWith (+) bt w m 
    in  m' `seq` (fromIntegral bar, m')

-- | Plots an 'NSWProf'ile by calculating the average profile
showNSWProf :: (TimeSig, NSWProf) -> String
showNSWProf (ts, (bars, m)) = intercalate "\n" ( show ts : foldrWithKey shw [] m )

  where shw :: Ratio Time -> NSWeight -> [String] -> [String]
        shw bt w r = let x = w / fromIntegral bars
                     in (show bt ++ ": " ++ show x ++ "  " ++ stars x) : r

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
                        print . minDur $ ms
                        let es  = matchMeterIMA FourtyEighth ms
                            tpb = ticksPerBeat ms
                        putStrLn ("Ticks per beat: " ++ show tpb)
                        -- does the file contains disruptive onsets?
                        case es of 
                          Right (d,s) -> 
                            do putStrLn ("Quantisation deviation: " ++ show d)
                               mapM_ (starMeter tpb) s
                               printMeterStats . collectNSWProf 
                                  (map (toNSWProf tpb) s) $ empty
                          Left e      -> putStrLn e -- show the error
                            
       ["-a", fp] -> do mapDirInDir (\x -> mapDir readProf x >>= unionNWProfMaps) fp
                            >>= unionNWProfMaps >>= printMeterStats
                            
       ["-d", fp] -> do mapDir readProf fp >>= unionNWProfMaps >>= printMeterStats
                        
       _    -> error "Please use -f <file> or -d <ragtime directory>"
   
unionNWProfMaps :: [Map TimeSig NSWProf] -> IO (Map TimeSig NSWProf)
unionNWProfMaps m = do let r = foldr (unionWith mergeNSWProf) empty m
                       r `seq` return r


printMeterStats :: Map TimeSig NSWProf -> IO ()
printMeterStats = mapM_ (putStrLn . showNSWProf) . toList 
   
readProf :: FilePath -> IO (Map TimeSig NSWProf)
readProf fp = 
  do ms <- readMidiScoreSafe fp
     case ms of
       Just x  -> do putStrLn ((show . map getEvent . getTimeSig $ x) ++ ": " ++ fp)
                     case getTimeSig x of
                       -- ignore the piece if not time signature is present
                       [Timed _ NoTimeSig] -> return empty 
                       -- check for weird onsets
                       _  -> case toNSWProfSegs x of
                               Right (d,p) -> 
                                 do let r = collectNSWProf p empty 
                                    -- putStrLn ("Q deviation: " ++ show d)
                                    -- mapM_ (putStrLn . showNSWProf) (toList r)
                                    r `seq` return r
                               Left  e -> 
                                 do putErrStrLn ("Warning: skipping " 
                                                 ++ fp ++ ": " ++ e)
                                    return empty
       Nothing -> return empty
       