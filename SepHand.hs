module Main (main) where

import ZMidi.Core         ( writeMidi )
import ZMidiBasic
import MidiCommonIO       ( readMidiScore, mapDir )

import Data.List          ( intercalate, sort, groupBy, genericLength, intersect )
import Data.Function      ( on )
import Control.Arrow      ( (***) )
import System.Environment ( getArgs )

main :: IO ()
main = do arg <- getArgs
          case arg of
            ["-d", d] -> do putStrLn ("filepath\tmin 1\tmax 1\tmin 2\tmax 2")
                            mapDir showMidiStats d
            ["-f", f] -> readMidiFile f
            _         -> putStrLn "usage:  <filename> "


readMidiFile :: FilePath -> IO ()
readMidiFile f = 
  do ms <- readMidiScore f
     writeMidi (f ++ ".handsep.mid") . midiScoreToMidiFile 
             . sepHand skyLine . mergeTracks $ ms
     -- mapM_ (putStrLn . show . voiceStats) (getVoices ms)
                    

                      
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
  ms {getVoices = [sort . map (fmap setChan) . concat . getVoices $ ms]} 
    where setChan n = n {channel = 0}
 
-- TODO: adapt channel number
-- | A hand separation function that takes a separation function and applies
-- this to a 'MidiScore'. sepHand throws an error the number of tracks is not 1
sepHand :: (Voice -> (Voice, Voice)) -> MidiScore -> MidiScore
sepHand f mf = case getVoices mf of
  [x] -> let (l,r) = f x in mf {getVoices = [r,l]}
  _   -> error "sepHand: more or less than 1 voice!"
    
-- | An implementation of the "skyline algorithm" that picks the highest note
skyLine :: Voice -> (Voice, Voice)
skyLine = (concat *** concat) . unzip . map pickHigh . groupBy ((==) `on` onset)

pickHigh :: [Timed ScoreEvent] -> ([Timed ScoreEvent],[Timed ScoreEvent])
pickHigh [ ] = error "pickHigh: empty list"
pickHigh [x] = ([],[x])
pickHigh l   = let (h:t) = reverse . sort $ l in (t,[h])

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

-- | calculates the the triplet of Precision, Recall and F-Measure given
-- a list of segmentable groundtruth items and test items.
precRecF :: Eq a => [a] -> [a] -> PrecisionRecallFMeasure
precRecF gt test = let inters    = genericLength (gt `intersect` test) 
                       precision = inters / genericLength test
                       recall    = inters / genericLength gt
                       fMeasure  = (2 * precision * recall) / (precision + recall)
                   in precision `seq` recall `seq` fMeasure 
                                `seq` PRF precision recall fMeasure

-- | Datatype for storing the triplet of Precision, Recall and F-Measure
data PrecisionRecallFMeasure = PRF Double Double Double 

instance Show PrecisionRecallFMeasure where
  -- show (PRF p r f) = show p ++ '\t' : show r ++ '\t' : show f
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
 