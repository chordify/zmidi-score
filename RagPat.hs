module Main where

import ZMidi.Core         ( readMidi )
import ZMidiBasic
import MidiCommonIO       ( mapDirInDir, mapDir, readMidiScore )

import System.Environment ( getArgs )
import Data.List          ( intercalate, genericLength )
import Evaluation
import MelFind            ( findMelodyQuant )

-- import Debug.Trace
-- traceShow' a = traceShow a a 

-- | do stuff with a 'MidiScore' ...
test :: FilePath -> IO ()
test f = do p <- readMidiScore f >>= return . scoreToPatterns FourtyEighth
            putStrLn . showPats $ p
            print . matchBeatDiv straightGrid $ p

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

matchBeatDiv :: Pattern -> [Pattern] -> Double
matchBeatDiv p ps = let rs = filter (not . isNaN) . map (recall p) $ ps
                    in  sum rs / genericLength rs




straightGrid, swingGrid, evenDistGrid :: Pattern
straightGrid = [ X, O, O, I, O, O, X, O, O, I, O, O ]
swingGrid    = [ X, O, I, O, I, O, X, O, I, O, I, O ]
evenDistGrid = [ I, I, I, I, I, I, I, I, I, I, I, I ]

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




