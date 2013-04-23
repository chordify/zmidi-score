module Main where

import ZMidi.Core         ( readMidi )
import ZMidiBasic
import MidiCommonIO       ( mapDirInDir, mapDir, readMidiScore )

import System.Environment ( getArgs )
import Data.List          ( intercalate, genericLength, sortBy )
import Data.Ord           ( comparing )
import Control.Arrow      ( first )
import Evaluation
import MelFind            ( findMelodyQuant )

import Math.Statistics    ( correl )

-- import Debug.Trace
-- traceShow' a = traceShow a a 

-- | do stuff with a 'MidiScore' ...
test :: FilePath -> IO ()
test f = do p <- readMidiScore f >>= return . scoreToPatterns FourtyEighth
            putStrLn . showPats $ p
            print . rankSubDiv $ p

showPats :: [Pattern] -> String
showPats = intercalate "\n" . map show

-- | Takes a midiscore, quantises it, finds the melody, and turns it into a 
-- 'Pattern' list, where every 'Pattern' represents a beat
scoreToPatterns :: ShortestNote -> MidiScore -> [Pattern]
scoreToPatterns q ms = groupEvery (toGridUnit q) . toPat [0, minLen .. ] 
                     . map onset . findMelodyQuant q $ ms where

  minLen  = getMinGridSize q ms
  
  toPat :: [Time] -> [Time] -> [Onset]
  toPat [] []  = []
  toPat _  []  = []
  toPat (g:gs) (d:ds) | g == d = I : toPat gs ds
                      | g <  d = if d - g < minLen 
                                 then error "unquantised interval encountered"         
                                 else O : toPat gs (d:ds)
                                 

getMinGridSize :: ShortestNote -> MidiScore -> Time
getMinGridSize q ms = case ticksPerBeat ms `divMod` (toGridUnit q) of
                        (d,0) -> d
                        (d,m) -> error "getMinGridSize: invalid quantisation"
                                 
-- | Groups a list of patterns in fixed size lists, if the last list is not 
-- of the same length, the remainder is filled with 'X's
groupEvery :: Int -> Pattern -> [Pattern]
groupEvery x p | glen == x =  g : groupEvery x r
               | otherwise = [g ++ replicate (x - glen) X]
  where (g,r) = splitAt x p 
        glen  = length g

--------------------------------------------------------------------------------
-- Matching beat subdivisions
--------------------------------------------------------------------------------

-- ranks different 'Subdiv'isions based on how well the correlate to the data
rankSubDiv :: [Pattern] -> [(Double, DivLab)]
rankSubDiv ps = reverse . sortBy (comparing fst) . map matchBeatDiv $ pats where

  matchBeatDiv :: SubDiv -> (Double, DivLab)
  matchBeatDiv s@(p, _) = let rs = filter (not . isNaN) . map (recall p) $ ps
                          in  first (const $ sum rs / genericLength rs) s

-- calculates the correlation between a template 'Pattern' and a list of data
-- 'Patterns'. The pattern is summarised by 'countMatch's
correlPat :: Pattern -> [Pattern] -> Double
correlPat p s = correl (toDouble p) (countMatch s)
               
-- | Given a template 'Pattern' counts the number of onsets in each position and
-- normalises this list by dividing all number by the highest count, yielding
-- a list of 'Doubles' between 0 and 1.               
countMatch :: [Pattern] -> [Double]
countMatch = normalise . foldr step [ 0, 0, 0,  0, 0, 0,  0, 0, 0,  0, 0, 0  ] where

  step :: Pattern -> [Int] -> [Int]
  step x cs = zipWith doPattern x cs

  doPattern :: Onset -> Int -> Int
  doPattern  I c = succ c
  doPattern  _ c = c
               
  normalise :: [Int] -> [Double]
  normalise i = let m = fromIntegral . maximum $ i 
                in  map (\x -> fromIntegral x / m) i

-- | Labels for the different subdivisions
data DivLab = Straight | Swing | Evenly deriving (Show, Eq)
type SubDiv = (Pattern, DivLab)

-- all patterns
pats :: [SubDiv]
pats =  [(straightGrid, Straight), (swingGrid, Swing), (evenDistGrid, Evenly)]
                
straightGrid, swingGrid, evenDistGrid :: Pattern
straightGrid = [ I, O, O, I, O, O, I, O, O, I, O, O ]
swingGrid    = [ I, O, I, O, I, O, I, O, I, O, I, O ]
evenDistGrid = [ I, I, I, I, I, I, I, I, I, I, I, I ]

toDouble :: Pattern -> [Double]
toDouble = map convert where
  
  convert :: Onset -> Double
  convert I = 0.8
  convert _ = 0.0

--------------------------------------------------------------------------------
-- Matching rhythmic patterns
--------------------------------------------------------------------------------

data Onset = X -- | don't care symbol, ignored in all evaluation
           | O -- | No onset
           | I -- | Onset
             deriving Eq
             
instance Show Onset where
  show o = case o of
            X -> "*"
            O -> "_"
            I -> "x"
  showList l s = s ++ (intercalate " " . map show $ l)

instance Matchable Onset where
  -- eqi :: Onset -> Onset -> EqIgnore
  eqi I I = Equal
  eqi O O = Equal
  eqi X _ = Ignore
  eqi _ X = Ignore
  eqi _ _ = NotEq  
  
type Pattern = [Onset]



untied1 = [ I, I, O, I,  X, X, X, X,  X, X, X, X,  X, X, X, X ]
untied2 = [ X, X, X, X,  I, I, O, I,  X, X, X, X,  X, X, X, X ]
untied3 = [ X, X, X, X,  X, X, X, X,  I, I, O, I,  X, X, X, X ]
untied4 = [ X, X, X, X,  X, X, X, X,  X, X, X, X,  I, I, O, I ]

tied1   = [ I, O, I, I,  O, I, I, O,  X, X, X, X,  X, X, X, X ]
tied2   = [ X, X, X, X,  X, X, X, X,  I, O, I, I,  O, I, I, O ]
tiedStr = [ X, X, X, X,  I, O, I, I,  O, I, I, O,  X, X, X, X ]




-- oeq :: Onset -> Onset 
             
-- type Bar    = [Bool] -- This should be a fixed length list
-- type Rhythm = [Bar]




