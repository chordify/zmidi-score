module Main where

import ZMidi.Core         ( readMidi )
import ZMidiBasic
import MidiCommonIO       ( mapDirInDir, mapDir, readMidiScore )

import System.Environment ( getArgs )
import Data.List          ( intercalate )
import Evaluation
import SepHand            ( findMelody )


-- | do stuff with a 'MidiScore' ...
test :: FilePath -> IO ()
test f = do mf <- readMidiScore f 
            print . scoreToPatterns $ mf


scoreToPatterns :: MidiScore -> Pattern
scoreToPatterns ms = toPat [0, minLen .. ] . map onset . findMelody 
                   . quantise FourtyEighth $ ms where

  minLen = ticksPerBeat ms `div` (toGridUnit FourtyEighth)

  convert a b = zipWith 
  
  toPat :: [Time] -> [Time] -> [Onset]
  toPat [] []  = []
  toPat _  []  = []
  toPat (g:gs) (d:ds) | g == d = I : toPat gs ds
                      | g <  d = if d -  g < minLen 
                                 then error "unquantised interval encountered"         
                                 else O : toPat gs (d:ds)
  
  -- toPat :: [Time] -> Pattern
  -- toPat [] = []
  -- toPat t  | i = 0      = O
           -- | i < minLen = error "unquantised interval encountered"
           -- |            where i = t - minLen 
             
  

--------------------------------------------------------------------------------
-- Matching rhythmic patterns
--------------------------------------------------------------------------------

data Onset = X -- | don't care symbol, ignored in all evaluation
           | O -- | No onset
           | I -- | Onset
             deriving Eq
             
instance Show Onset where
  show o = case o of
            X -> "_"
            O -> "O"
            I -> "I"
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




