module Main (main) where

import ZMidi.Core         ( writeMidi )
import ZMidiBasic
import MidiCommonIO       ( readMidiScore, mapDir )

import Data.List          ( intercalate, sortBy, groupBy, genericLength
                          , intersectBy )
import Data.Function      ( on )
import Data.Ord           ( comparing )
import Control.Arrow      ( (***) )
import System.Environment ( getArgs )

main :: IO ()
main = do arg <- getArgs
          case arg of
            ["-d", d] -> do putStrLn ("filepath\tmin 1\tmax 1\tmin 2\tmax 2")
                            mapDir showMidiStats d
            ["-f", f] -> readMidiFile f
            ["-q", f] ->   readMidiScore f >>= writeMidi (f ++ ".quant.mid") 
                         . midiScoreToMidiFile . quantise ThirtySecond
            _  -> putStrLn "usage:  -f <filename> OR -d <directory> OR -q <filename>" 


readMidiFile :: FilePath -> IO ()
readMidiFile f = readMidiScore f >>=  writeMidi (f ++ ".handsep.mid") 
                                   .  midiScoreToMidiFile 
                                   .  sepHand skyLine . mergeTracks 

                      
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

-- | Merges all tracks into one track
mergeTracks :: MidiScore -> MidiScore
mergeTracks ms = 
  ms {getVoices = [sortBy (comparing onset) . setChans 0 . concat . getVoices $ ms]} 
 
-- TODO: adapt channel number
-- | A hand separation function that takes a separation function and applies
-- this to a 'MidiScore'. sepHand throws an error the number of tracks is not 1
sepHand :: (Voice -> (Voice, Voice)) -> MidiScore -> MidiScore
sepHand f mf = case getVoices mf of
  [x] -> let (r,l) = f x in mf {getVoices = [r,l]}
  _   -> error "sepHand: more or less than 1 voice!"
    
-- | An implementation of the "skyline algorithm" that picks the highest note
skyLine :: Voice -> (Voice, Voice)
skyLine = (f 0 *** f 1) . unzip . map (pickHigh p) . groupBy ((==) `on` onset)
  where f c = setChans c . concat
        p   = Pitch (0, 0)

-- | Picks the highest notes and separates them from the rest (high,low)
pickHigh :: Pitch -> [Timed ScoreEvent] 
         -> ([Timed ScoreEvent],[Timed ScoreEvent])
pickHigh _ [ ] = error "pickHigh: empty list"
pickHigh p l | getPitch h > p = ([h], t)
             | otherwise      = ([ ], l) 
                 where (h:t)  = reverse . sortBy (comparing getPitch) $ l 

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

leftHandRetrieval :: MidiScore -> MidiScore -> PrecisionRecallFMeasure
leftHandRetrieval gt test = undefined

noteRetrieval :: Voice -> Voice -> PrecisionRecallFMeasure
noteRetrieval gt test = precRecF eqf gt test where
  
  eqf :: Timed ScoreEvent -> Timed ScoreEvent -> Bool
  eqf (Timed onA datA) (Timed onB datB) =  onA           == onB 
                                        && pitch    datA == pitch datB
                                        && velocity datA == velocity datB
                                        && duration datA == duration datB

-- | calculates the the triplet of Precision, Recall and F-Measure given
-- a list of segmentable groundtruth items and test items.
precRecF :: (a -> a -> Bool) -> [a] -> [a] -> PrecisionRecallFMeasure
precRecF eqf gt test = 
  let inters    = genericLength $ intersectBy eqf gt test
      precision = inters / genericLength test
      recall    = inters / genericLength gt
      fMeasure  = (2 * precision * recall) / (precision + recall)
  in precision `seq` recall `seq` fMeasure `seq` PRF precision recall fMeasure

-- | Datatype for storing the triplet of Precision, Recall and F-Measure
data PrecisionRecallFMeasure = PRF Double Double Double 

instance Show PrecisionRecallFMeasure where
  show (PRF p r f) = intercalate "\t" . map show $ [p,r,f]
  
{-
-- | a version of 'Data.List.intersect' that takes advantage of the fact that
-- the two intersected lists are sorted ascendingly (it is approximatly 30 times
-- as fast)
sortIntersect :: Ord a => [a] -> [a] -> [a]
sortIntersect [] _  = []
sortIntersect _  [] = []
sortIntersect (x:xs) ys  
  | null rest      =     sortIntersect xs rest
  | x == head rest = x : sortIntersect xs (tail rest)
  | otherwise      =     sortIntersect xs rest
      where rest = dropWhile (< x) ys
 -}
--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
setChans :: Channel -> [Timed ScoreEvent] -> [Timed ScoreEvent]
setChans c = map (setChan c)

setChan :: Channel -> Timed ScoreEvent -> Timed ScoreEvent
setChan c tse = fmap f tse where f ne = ne {channel = c}
